{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.V2010_08_01.DisableAlarmActions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Disables actions for the specified alarms. When an alarm's actions are
-- disabled the alarm's state may change, but none of the alarm's actions will
-- execute.
module Network.AWS.CloudWatch.V2010_08_01.DisableAlarmActions where

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

data DisableAlarmActions = DisableAlarmActions
    { _daaiAlarmNames :: [Text]
      -- ^ The names of the alarms to disable actions for.
    } deriving (Generic)

instance ToQuery DisableAlarmActions where
    toQuery = genericToQuery def

instance AWSRequest DisableAlarmActions where
    type Sv DisableAlarmActions = CloudWatch
    type Rs DisableAlarmActions = DisableAlarmActionsResponse

    request = post "DisableAlarmActions"
    response _ _ = return (Right DisableAlarmActionsResponse)

data DisableAlarmActionsResponse = DisableAlarmActionsResponse
    deriving (Eq, Show, Generic)
