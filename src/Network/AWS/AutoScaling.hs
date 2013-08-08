{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module      : Network.AWS.AutoScaling
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.AutoScaling where

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
-- AutoScaling Requests
--

data AutoScaling

instance AWSSigner AutoScaling where
    sign = version3

instance AWSRegion AutoScaling where
    regionalise _ = id

