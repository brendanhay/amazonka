{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DeleteSubscriptionFilter
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes a subscription filter associated with the specified log group.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DeleteSubscriptionFilter.html>
module Network.AWS.CloudWatchLogs.DeleteSubscriptionFilter
    (
    -- * Request
      DeleteSubscriptionFilter
    -- ** Request constructor
    , deleteSubscriptionFilter
    -- ** Request lenses
    , drqLogGroupName
    , drqFilterName

    -- * Response
    , DeleteSubscriptionFilterResponse
    -- ** Response constructor
    , deleteSubscriptionFilterResponse
    ) where

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteSubscriptionFilter' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drqLogGroupName'
--
-- * 'drqFilterName'
data DeleteSubscriptionFilter = DeleteSubscriptionFilter'
    { _drqLogGroupName :: !Text
    , _drqFilterName   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteSubscriptionFilter' smart constructor.
deleteSubscriptionFilter :: Text -> Text -> DeleteSubscriptionFilter
deleteSubscriptionFilter pLogGroupName pFilterName =
    DeleteSubscriptionFilter'
    { _drqLogGroupName = pLogGroupName
    , _drqFilterName = pFilterName
    }

-- | The name of the log group that is associated with the subscription
-- filter to delete.
drqLogGroupName :: Lens' DeleteSubscriptionFilter Text
drqLogGroupName = lens _drqLogGroupName (\ s a -> s{_drqLogGroupName = a});

-- | The name of the subscription filter to delete.
drqFilterName :: Lens' DeleteSubscriptionFilter Text
drqFilterName = lens _drqFilterName (\ s a -> s{_drqFilterName = a});

instance AWSRequest DeleteSubscriptionFilter where
        type Sv DeleteSubscriptionFilter = CloudWatchLogs
        type Rs DeleteSubscriptionFilter =
             DeleteSubscriptionFilterResponse
        request = postJSON
        response
          = receiveNull DeleteSubscriptionFilterResponse'

instance ToHeaders DeleteSubscriptionFilter where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.DeleteSubscriptionFilter" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteSubscriptionFilter where
        toJSON DeleteSubscriptionFilter'{..}
          = object
              ["logGroupName" .= _drqLogGroupName,
               "filterName" .= _drqFilterName]

instance ToPath DeleteSubscriptionFilter where
        toPath = const "/"

instance ToQuery DeleteSubscriptionFilter where
        toQuery = const mempty

-- | /See:/ 'deleteSubscriptionFilterResponse' smart constructor.
data DeleteSubscriptionFilterResponse =
    DeleteSubscriptionFilterResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteSubscriptionFilterResponse' smart constructor.
deleteSubscriptionFilterResponse :: DeleteSubscriptionFilterResponse
deleteSubscriptionFilterResponse = DeleteSubscriptionFilterResponse'
