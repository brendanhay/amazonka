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
    , eaaAlarmNames

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
-- * 'eaaAlarmNames'
newtype EnableAlarmActions = EnableAlarmActions'
    { _eaaAlarmNames :: [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableAlarmActions' smart constructor.
enableAlarmActions :: EnableAlarmActions
enableAlarmActions =
    EnableAlarmActions'
    { _eaaAlarmNames = mempty
    }

-- | The names of the alarms to enable actions for.
eaaAlarmNames :: Lens' EnableAlarmActions [Text]
eaaAlarmNames = lens _eaaAlarmNames (\ s a -> s{_eaaAlarmNames = a}) . _Coerce;

instance AWSRequest EnableAlarmActions where
        type Sv EnableAlarmActions = CloudWatch
        type Rs EnableAlarmActions =
             EnableAlarmActionsResponse
        request = postQuery
        response = receiveNull EnableAlarmActionsResponse'

instance ToHeaders EnableAlarmActions where
        toHeaders = const mempty

instance ToPath EnableAlarmActions where
        toPath = const mempty

instance ToQuery EnableAlarmActions where
        toQuery EnableAlarmActions'{..}
          = mconcat
              ["Action" =: ("EnableAlarmActions" :: ByteString),
               "Version" =: ("2010-08-01" :: ByteString),
               "AlarmNames" =: toQueryList "member" _eaaAlarmNames]

-- | /See:/ 'enableAlarmActionsResponse' smart constructor.
data EnableAlarmActionsResponse =
    EnableAlarmActionsResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableAlarmActionsResponse' smart constructor.
enableAlarmActionsResponse :: EnableAlarmActionsResponse
enableAlarmActionsResponse = EnableAlarmActionsResponse'
