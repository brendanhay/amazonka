{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudWatch.EnableAlarmActions
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

-- | Enables actions for the specified alarms.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_EnableAlarmActions.html>
module Network.AWS.CloudWatch.EnableAlarmActions
    (
    -- * Request
      EnableAlarmActions
    -- ** Request constructor
    , enableAlarmActions
    -- ** Request lenses
    , eaaAlarmNames

    -- * Response
    , EnableAlarmActionsResponse
    -- ** Response constructor
    , enableAlarmActionsResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudWatch.Types

-- | /See:/ 'enableAlarmActions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eaaAlarmNames'
newtype EnableAlarmActions = EnableAlarmActions'{_eaaAlarmNames :: [Text]} deriving (Eq, Read, Show)

-- | 'EnableAlarmActions' smart constructor.
enableAlarmActions :: [Text] -> EnableAlarmActions
enableAlarmActions pAlarmNames = EnableAlarmActions'{_eaaAlarmNames = pAlarmNames};

-- | The names of the alarms to enable actions for.
eaaAlarmNames :: Lens' EnableAlarmActions [Text]
eaaAlarmNames = lens _eaaAlarmNames (\ s a -> s{_eaaAlarmNames = a});

instance AWSRequest EnableAlarmActions where
        type Sv EnableAlarmActions = CloudWatch
        type Rs EnableAlarmActions =
             EnableAlarmActionsResponse
        request = post
        response = receiveNull EnableAlarmActionsResponse'

instance ToHeaders EnableAlarmActions where
        toHeaders = const mempty

instance ToPath EnableAlarmActions where
        toPath = const "/"

instance ToQuery EnableAlarmActions where
        toQuery EnableAlarmActions'{..}
          = mconcat
              ["Action" =: ("EnableAlarmActions" :: ByteString),
               "Version" =: ("2010-08-01" :: ByteString),
               "AlarmNames" =: "member" =: _eaaAlarmNames]

-- | /See:/ 'enableAlarmActionsResponse' smart constructor.
data EnableAlarmActionsResponse = EnableAlarmActionsResponse' deriving (Eq, Read, Show)

-- | 'EnableAlarmActionsResponse' smart constructor.
enableAlarmActionsResponse :: EnableAlarmActionsResponse
enableAlarmActionsResponse = EnableAlarmActionsResponse';
