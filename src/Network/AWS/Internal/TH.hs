{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

-- |
-- Module      : Network.AWS.Internal.TH
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.TH
    (
    -- * Template Instances
      deriveTmpl
    , deriveTmpl'

    -- * QueryString Instances
    , deriveQS
    , deriveQS'

    -- * Aeson.TH Options
    , fieldOptions
    , loweredFieldOptions
    , underscoredFieldOptions
    ) where

import           Control.Monad
import           Data.Aeson.TH
import qualified Data.ByteString.Char8       as BS
import           Data.Monoid
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Network.AWS.Internal.String
import           Network.AWS.Internal.Types
import           Paths_aws_haskell           (getDataFileName)

--
-- Template
--

deriveTmpl :: Name -> Q [Dec]
deriveTmpl name = deriveTmpl' "template/" name

deriveTmpl' :: FilePath -> Name -> Q [Dec]
deriveTmpl' path name = liftM2 (++)
    (deriveToJSON (defaultOptions { fieldLabelModifier = f }) name)
    (embedTemplate path name)
  where
    f = lowerFirst . dropLower

--
-- QueryString
--

deriveQS :: Name -> Q [Dec]
deriveQS name = deriveQS' (lowerFirst . dropLower) name

deriveQS' :: (String -> String) -> Name -> Q [Dec]
deriveQS' f name = do
    ref <- reify name
    case ref of
        TyConI (DataD _ _ _ [RecC _ fields] _) -> do
            let names   = map (\(n, _, _) -> n) fields
                field n = [| queryParam s . $(global n) |]
                  where
                    s = BS.pack . f $ nameBase n
                query   = listE $ map field names

            [d|instance QueryString $(conT name) where
                   queryString x = concatMap ($ x) $query|]
        _ ->
            error $ show name ++
                ": can only derive QueryString instances for named record fields"

--
-- Aeson.TH Options
--

options, fieldOptions, loweredFieldOptions, underscoredFieldOptions :: Options
options                 = defaultOptions
fieldOptions            = options { fieldLabelModifier = dropLower }
loweredFieldOptions     = options { fieldLabelModifier = lowerAll . dropLower }
underscoredFieldOptions = options { fieldLabelModifier = underscore . dropLower }

--
-- Internal
--

instance Lift BS.ByteString where
    lift = return . LitE . StringL . BS.unpack

embedTemplate :: FilePath -> Name -> Q [Dec]
embedTemplate path name =
    [d|instance Template $(conT name) where
           readTemplate _ = $(template >>= embed)|]
  where
    template = runIO $
        getDataFileName (path <> suffix (show name)) >>= BS.readFile

    embed bstr = do
        pack <- [| BS.pack |]
        return $! AppE pack $! LitE $! StringL $! BS.unpack bstr
