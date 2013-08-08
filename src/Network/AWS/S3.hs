{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module      : Network.AWS.S3
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.S3 where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString           (ByteString)
import Data.Monoid
import Data.String
import Data.Text                 (Text)
import Network.AWS.Internal
import Network.Http.Client       hiding (get)

--
-- S3 Requests
--

data S3

instance AWSSigner S3 where
    sign = version3

instance AWSRegion S3 where
    regionalise _ = id
