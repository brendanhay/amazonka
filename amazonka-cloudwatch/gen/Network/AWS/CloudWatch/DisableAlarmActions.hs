{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.DisableAlarmActions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Disables actions for the specified alarms. When an alarm\'s actions are
-- disabled the alarm\'s state may change, but none of the alarm\'s actions
-- will execute.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DisableAlarmActions.html AWS API Reference> for DisableAlarmActions.
module Network.AWS.CloudWatch.DisableAlarmActions
    (
    -- * Creating a Request
      DisableAlarmActions
    , disableAlarmActions
    -- * Request Lenses
    , daaAlarmNames

    -- * Destructuring the Response
    , DisableAlarmActionsResponse
    , disableAlarmActionsResponse
    ) where

import           Network.AWS.CloudWatch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'disableAlarmActions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daaAlarmNames'
newtype DisableAlarmActions = DisableAlarmActions'
    { _daaAlarmNames :: [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisableAlarmActions' smart constructor.
disableAlarmActions :: DisableAlarmActions
disableAlarmActions =
    DisableAlarmActions'
    { _daaAlarmNames = mempty
    }

-- | The names of the alarms to disable actions for.
daaAlarmNames :: Lens' DisableAlarmActions [Text]
daaAlarmNames = lens _daaAlarmNames (\ s a -> s{_daaAlarmNames = a}) . _Coerce;

instance AWSRequest DisableAlarmActions where
        type Sv DisableAlarmActions = CloudWatch
        type Rs DisableAlarmActions =
             DisableAlarmActionsResponse
        request = postQuery
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
data DisableAlarmActionsResponse =
    DisableAlarmActionsResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisableAlarmActionsResponse' smart constructor.
disableAlarmActionsResponse :: DisableAlarmActionsResponse
disableAlarmActionsResponse = DisableAlarmActionsResponse'
