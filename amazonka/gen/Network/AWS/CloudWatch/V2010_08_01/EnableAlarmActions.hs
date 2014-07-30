{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.V2010_08_01.EnableAlarmActions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Enables actions for the specified alarms.
module Network.AWS.CloudWatch.V2010_08_01.EnableAlarmActions where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.CloudWatch.V2010_08_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data EnableAlarmActions = EnableAlarmActions
    { _eaaiAlarmNames :: [Text]
      -- ^ The names of the alarms to enable actions for.
    } deriving (Generic)

instance ToQuery EnableAlarmActions where
    toQuery = genericToQuery def

instance AWSRequest EnableAlarmActions where
    type Sv EnableAlarmActions = CloudWatch
    type Rs EnableAlarmActions = EnableAlarmActionsResponse

    request = post "EnableAlarmActions"
    response _ _ = return (Right EnableAlarmActionsResponse)

data EnableAlarmActionsResponse = EnableAlarmActionsResponse
    deriving (Eq, Show, Generic)
