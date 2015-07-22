{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.EnableAlarmActions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Enables actions for the specified alarms.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_EnableAlarmActions.html>
module Network.AWS.CloudWatch.EnableAlarmActions
    (
    -- * Request
      EnableAlarmActions
    -- ** Request constructor
    , enableAlarmActions
    -- ** Request lenses
    , eaarqAlarmNames

    -- * Response
    , EnableAlarmActionsResponse
    -- ** Response constructor
    , enableAlarmActionsResponse
    ) where

import           Network.AWS.CloudWatch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'enableAlarmActions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eaarqAlarmNames'
newtype EnableAlarmActions = EnableAlarmActions'
    { _eaarqAlarmNames :: [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableAlarmActions' smart constructor.
enableAlarmActions :: EnableAlarmActions
enableAlarmActions =
    EnableAlarmActions'
    { _eaarqAlarmNames = mempty
    }

-- | The names of the alarms to enable actions for.
eaarqAlarmNames :: Lens' EnableAlarmActions [Text]
eaarqAlarmNames = lens _eaarqAlarmNames (\ s a -> s{_eaarqAlarmNames = a});

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
               "AlarmNames" =:
                 toQueryList "member" _eaarqAlarmNames]

-- | /See:/ 'enableAlarmActionsResponse' smart constructor.
data EnableAlarmActionsResponse =
    EnableAlarmActionsResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableAlarmActionsResponse' smart constructor.
enableAlarmActionsResponse :: EnableAlarmActionsResponse
enableAlarmActionsResponse = EnableAlarmActionsResponse'
