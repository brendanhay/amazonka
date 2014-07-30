{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.V2010_08_01.SetAlarmState
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Temporarily sets the state of an alarm. When the updated StateValue differs
-- from the previous value, the action configured for the appropriate state is
-- invoked. This is not a permanent change. The next periodic alarm check (in
-- about a minute) will set the alarm to its actual state.
module Network.AWS.CloudWatch.V2010_08_01.SetAlarmState where

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

-- | Minimum specification for a 'SetAlarmState' request.
setAlarmState :: Text -- ^ '_sasiAlarmName'
              -> Text -- ^ '_sasiStateReason'
              -> StateValue -- ^ '_sasiStateValue'
              -> SetAlarmState
setAlarmState p1 p2 p3 = SetAlarmState
    { _sasiAlarmName = p1
    , _sasiStateReason = p2
    , _sasiStateValue = p3
    , _sasiStateReasonData = Nothing
    }

data SetAlarmState = SetAlarmState
    { _sasiAlarmName :: Text
      -- ^ The descriptive name for the alarm. This name must be unique
      -- within the user's AWS account. The maximum length is 255
      -- characters.
    , _sasiStateReason :: Text
      -- ^ The reason that this alarm is set to this specific state (in
      -- human-readable text format).
    , _sasiStateValue :: StateValue
      -- ^ The value of the state.
    , _sasiStateReasonData :: Maybe Text
      -- ^ The reason that this alarm is set to this specific state (in
      -- machine-readable JSON format).
    } deriving (Generic)

instance ToQuery SetAlarmState where
    toQuery = genericToQuery def

instance AWSRequest SetAlarmState where
    type Sv SetAlarmState = CloudWatch
    type Rs SetAlarmState = SetAlarmStateResponse

    request = post "SetAlarmState"
    response _ _ = return (Right SetAlarmStateResponse)

data SetAlarmStateResponse = SetAlarmStateResponse
    deriving (Eq, Show, Generic)
