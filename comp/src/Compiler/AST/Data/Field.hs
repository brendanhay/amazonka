{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

-- Module      : Compiler.AST.Data.Field
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.AST.Data.Field where

import           Compiler.AST.TypeOf
import           Compiler.Types
import           Control.Applicative
import           Control.Comonad
import           Control.Comonad.Cofree
import           Control.Lens
import           Data.Function                (on)
import qualified Data.HashMap.Strict          as Map
import qualified Data.HashSet                 as Set
import           Data.List                    (findIndex, sortBy)
import           Data.Maybe
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Text.Manipulate
import           Language.Haskell.Exts.Syntax (Name (..))

-- | Convenience type to package up some information from the struct with the
-- related field, namely the memberId and the (Set.member required).
data Field = Field
    { _fieldOrdinal   :: !Int
    , _fieldId        :: Id    -- ^ The memberId from the struct members map.
    , _fieldRef       :: Ref   -- ^ The original struct member reference.
    , _fieldRequired  :: !Bool -- ^ Does the struct have this member in the required set.
    , _fieldPayload   :: !Bool -- ^ Does the struct have this memeber marked as the payload.
    , _fieldPrefix    :: Maybe Text
    , _fieldNamespace :: Maybe Text
    , _fieldDirection :: Maybe Direction
    }

makeLenses ''Field

instance IsStreaming Field where
    streaming = streaming . _fieldRef

instance TypeOf Field where
    typeOf f = req (f ^. fieldRequired) (typeOf (f ^. fieldRef))
      where
        req :: Bool -> TType -> TType
        req True  t = t
        req False t = TMaybe t

instance HasInfo Field where
    info = fieldAnn . info

-- FIXME: Can just add the metadata to field as well since
-- the protocol/timestamp are passed in everywhere in the .Syntax module.
mkFields :: HasMetadata a f
         => a
         -> Solved
         -> StructF (Shape Solved)
         -> [Field]
mkFields m s st = sortFields rs $
    zipWith mk [1..] $ Map.toList (st ^. members)
  where
    mk :: Int -> (Id, Ref) -> Field
    mk i (k, v) = Field i k v req pay p ns d
      where
        req = k `elem` rs
        pay = Just k == st ^. payload

        ns  = (view xmlUri <$> v ^. refXMLNamespace)
          <|> (m ^. xmlNamespace)

    rs = getRequired st

    p = s ^. annPrefix
    d = case s ^. relMode of
        Uni x -> Just x
        Bi    -> Nothing

-- | Ensures that streaming fields appear last in the parameter ordering,
-- but doesn't affect the rest of the order which is determined by parsing
-- of the JSON service definition.
sortFields :: [Id] -> [Field] -> [Field]
sortFields xs = zipWith (set fieldOrdinal) [1..]
    -- FIXME: optimisation
    . sortBy (on compare streaming)
    . sortBy (on compare idx)
  where
    idx x = fromMaybe (-1) $ findIndex (== _fieldId x) xs

fieldLens, fieldAccessor :: Getter Field Text
fieldLens     = to (\f -> f ^. fieldId . lensId     (f ^. fieldPrefix))
fieldAccessor = to (\f -> f ^. fieldId . accessorId (f ^. fieldPrefix))

fieldIsParam :: Field -> Bool
fieldIsParam f = f ^. fieldRequired && not (f ^. fieldMonoid)

fieldParamName :: Field -> Name
fieldParamName = Ident
    . Text.unpack
    . Text.cons 'p'
    . upperHead
    . view (fieldId . typeId)

fieldHelp :: Getter Field Help
fieldHelp = fieldRef
    . refDocumentation
    . to (fromMaybe "FIXME: Undocumented member.")

fieldLocation :: Getter Field (Maybe Location)
fieldLocation = fieldRef . refLocation

fieldMonoid :: Getter Field Bool
fieldMonoid = fieldAnn . to (elem DMonoid . derivingOf)

fieldStream :: Getter Field Bool
fieldStream = to f
  where
    f x = x ^. fieldRef . refStreaming
       || x ^. fieldAnn . infoStreaming

fieldList1, fieldList, fieldMap :: Field -> Bool
fieldList1 f = fieldList f && nonEmpty f
fieldList    = not . isn't _List . unwrap . view fieldAnn
fieldMap     = not . isn't _Map  . unwrap . view fieldAnn

fieldAnn :: Lens' Field (Shape Solved)
fieldAnn = fieldRef . refAnn
