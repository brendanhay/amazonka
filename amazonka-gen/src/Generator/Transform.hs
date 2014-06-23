{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Generator.Transform
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Generator.Transform where

import           Control.Lens
import           Data.Default
import           Data.Function
import           Data.HashMap.Strict       (HashMap)
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.String
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Util
import           GHC.Generics
import           Generator.AST
import           Network.HTTP.Types.Method

transform :: [Service] -> [Service]
transform = map service

service :: Service -> Service
service s = s & svcOperations %~ map (operation s)

operation :: Service -> Operation -> Operation
operation s o = o
    & opService   .~ s ^. svcName
    & opNamespace .~ s ^. svcVersionNamespace <> NS [_opName o]
    & opImports  <>~ imports
    & opRequest   %~ request o
    & opResponse  %~ response o
  where
    imports = sort
        [ "Network.AWS.Data"
        , "Network.AWS.Types"
        , "Data.Monoid"
        , "GHC.Generics"
        , "Data.Time"
        , s ^. svcTypesNamespace
        , fromString $ "Network.AWS.Request." ++ show (s ^. svcType)
        ]

request :: Operation -> Request -> Request
request o = rqName .~ o ^. opName

response :: Operation -> Response -> Response
response o = rsName .~ o ^. opName
