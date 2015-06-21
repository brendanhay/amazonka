{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudWatch.DisableAlarmActions
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Disables actions for the specified alarms. When an alarm\'s actions are
-- disabled the alarm\'s state may change, but none of the alarm\'s actions
-- will execute.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DisableAlarmActions.html>
module Network.AWS.CloudWatch.DisableAlarmActions
    (
    -- * Request
      DisableAlarmActions
    -- ** Request constructor
    , disableAlarmActions
    -- ** Request lenses
    , daaAlarmNames

    -- * Response
    , DisableAlarmActionsResponse
    -- ** Response constructor
    , disableAlarmActionsResponse
    ) where

import Network.AWS.CloudWatch.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disableAlarmActions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daaAlarmNames'
newtype DisableAlarmActions = DisableAlarmActions'{_daaAlarmNames :: [Text]} deriving (Eq, Read, Show)

-- | 'DisableAlarmActions' smart constructor.
disableAlarmActions :: DisableAlarmActions
disableAlarmActions = DisableAlarmActions'{_daaAlarmNames = mempty};

-- | The names of the alarms to disable actions for.
daaAlarmNames :: Lens' DisableAlarmActions [Text]
daaAlarmNames = lens _daaAlarmNames (\ s a -> s{_daaAlarmNames = a});

instance AWSRequest DisableAlarmActions where
        type Sv DisableAlarmActions = CloudWatch
        type Rs DisableAlarmActions =
             DisableAlarmActionsResponse
        request = post
        response = receiveNull DisableAlarmActionsResponse'

instance ToHeaders DisableAlarmActions where
        toHeaders = const mempty

instance ToPath DisableAlarmActions where
        toPath = const "/"

instance ToQuery DisableAlarmActions where
        toQuery DisableAlarmActions'{..}
          = mconcat
              ["Action" =: ("DisableAlarmActions" :: ByteString),
               "Version" =: ("2010-08-01" :: ByteString),
               "AlarmNames" =: toQueryList "member" _daaAlarmNames]

-- | /See:/ 'disableAlarmActionsResponse' smart constructor.
data DisableAlarmActionsResponse = DisableAlarmActionsResponse' deriving (Eq, Read, Show)

-- | 'DisableAlarmActionsResponse' smart constructor.
disableAlarmActionsResponse :: DisableAlarmActionsResponse
disableAlarmActionsResponse = DisableAlarmActionsResponse';
