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
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DeleteSubscriptionFilter.html AWS API Reference> for DeleteSubscriptionFilter.
module Network.AWS.CloudWatchLogs.DeleteSubscriptionFilter
    (
    -- * Creating a Request
      DeleteSubscriptionFilter
    , deleteSubscriptionFilter
    -- * Request Lenses
    , dLogGroupName
    , dFilterName

    -- * Destructuring the Response
    , DeleteSubscriptionFilterResponse
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
-- * 'dLogGroupName'
--
-- * 'dFilterName'
data DeleteSubscriptionFilter = DeleteSubscriptionFilter'
    { _dLogGroupName :: !Text
    , _dFilterName   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteSubscriptionFilter' smart constructor.
deleteSubscriptionFilter :: Text -> Text -> DeleteSubscriptionFilter
deleteSubscriptionFilter pLogGroupName_ pFilterName_ =
    DeleteSubscriptionFilter'
    { _dLogGroupName = pLogGroupName_
    , _dFilterName = pFilterName_
    }

-- | The name of the log group that is associated with the subscription
-- filter to delete.
dLogGroupName :: Lens' DeleteSubscriptionFilter Text
dLogGroupName = lens _dLogGroupName (\ s a -> s{_dLogGroupName = a});

-- | The name of the subscription filter to delete.
dFilterName :: Lens' DeleteSubscriptionFilter Text
dFilterName = lens _dFilterName (\ s a -> s{_dFilterName = a});

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
              ["logGroupName" .= _dLogGroupName,
               "filterName" .= _dFilterName]

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
