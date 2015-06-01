{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Compiler.AST.Data.Instance
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.AST.Data.Instance where

import           Compiler.AST.Data.Field
import           Compiler.Formatting
import           Compiler.Types          hiding (input, output)
import           Control.Error
import           Control.Lens
import           Data.Aeson
import qualified Data.Foldable           as Fold
import           Data.List               (deleteBy, find)
import           Data.Maybe
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Text.Manipulate

-- FIXME: XML namespace requirement for ToElement
-- general tidy up of syntax/instance/data/annotations
-- build

data Inst
    = FromXML   [Field]
    | FromJSON  [Field]
    | ToXML     [Field]
    | ToElement Field
    | ToJSON    [Field]
    | ToQuery   [Either Text Field]
    | ToHeaders [Field]
    | ToPath    [Either Text Field]
    | ToBody    Field
      deriving (Eq, Show)

instance ToJSON Inst where
    toJSON = toJSON . instToText

instToText :: Inst -> Text
instToText = \case
    FromJSON     {} -> "FromJSON"
    FromXML      {} -> "FromXML"
    ToJSON       {} -> "ToJSON"
    ToXML        {} -> "ToXML"
    ToElement {} -> "ToElement"
    ToQuery      {} -> "ToQuery"
    ToHeaders    {} -> "ToHeaders"
    ToPath       {} -> "ToPath"
    ToBody       {} -> "ToBody"

prodInsts :: HasRelation a
          => Protocol
          -> a
          -> [Field]
          -> Either Error [Inst]
prodInsts p r = pure . shape p (r ^. relMode)

sumInsts :: HasRelation a => Protocol -> a -> [Text]
sumInsts p r = map instToText $ shape p (r ^. relMode) []

-- FIXME: this is of a singluar horror.
requestInsts :: Protocol -> HTTP Identity -> [Field] -> Either Error [Inst]
requestInsts p h fs = do
    ps  <- uri uriPath
    qs  <- uri uriQuery
    xml <- xelement
    return $!
        [ ToHeaders    hs
        , ToPath       ps
        ] ++ maybe [] ((:[]) . ToBody) (find (view fieldStream) bs)
          ++ maybe [ToQuery []] (g (qs <> map Right (satisfies [Querystring] fs))) (find f is)
          ++ filter (not . f) xml
  where
    hs  = satisfies [Header, Headers] fs
    bs  = satisfy (\l -> isNothing l || Just Body == l) fs
    is' = filter (not . view fieldStream) bs
    is  = shape p (Uni Input) is'

    xelement = traverse go is
      where
        go (ToXML [])  = return $  ToXML []
        go (ToXML [x]) = return $! ToElement x
        go (ToXML _)   = Left "More than one toxmlelement field"
        go x           = return x

    f ToQuery {} = True
    f _          = False

    -- f' ToXML {} = True
    -- f' _        = False

    g ys (ToQuery xs) = [ToQuery (xs <> ys)]
    g _  x            = [x]

    uri l = traverse go (h ^. l)
      where
        go (Tok t) = return (Left t)
        go (Var v) = do
            let m = format ("Missing field corresponding to URI var " % iprimary) v
            f <- note m (Fold.find ((v ==) . view fieldId) fs)
            return (Right f)

shape :: Protocol -> Mode -> [Field] -> [Inst]
shape p m fs = case m of
    Bi         -> [input  p fs, output p fs]
    Uni Input  -> [input  p fs]
    Uni Output -> [output p fs]

input :: Protocol -> [Field] -> Inst
input = \case
    JSON     -> ToJSON
    RestJSON -> ToJSON
    XML      -> ToXML
    RestXML  -> ToXML
    Query    -> ToQuery . map Right
    EC2      -> ToQuery . map Right

output :: Protocol -> [Field] -> Inst
output = \case
    JSON     -> FromJSON
    RestJSON -> FromJSON
    XML      -> FromXML
    RestXML  -> FromXML
    Query    -> FromXML
    EC2      -> FromXML

satisfies :: [Location] -> [Field] -> [Field]
satisfies xs = satisfy (`elem` map Just xs)

satisfy :: (Maybe Location -> Bool) -> [Field] -> [Field]
satisfy f = filter (f . view fieldLocation)
