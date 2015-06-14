{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudWatchLogs.DeleteRetentionPolicy
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

-- | Deletes the retention policy of the specified log group. Log events
-- would not expire if they belong to log groups without a retention
-- policy.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DeleteRetentionPolicy.html>
module Network.AWS.CloudWatchLogs.DeleteRetentionPolicy
    (
    -- * Request
      DeleteRetentionPolicy
    -- ** Request constructor
    , deleteRetentionPolicy
    -- ** Request lenses
    , drpLogGroupName

    -- * Response
    , DeleteRetentionPolicyResponse
    -- ** Response constructor
    , deleteRetentionPolicyResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudWatchLogs.Types

-- | /See:/ 'deleteRetentionPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drpLogGroupName'
newtype DeleteRetentionPolicy = DeleteRetentionPolicy'{_drpLogGroupName :: Text} deriving (Eq, Read, Show)

-- | 'DeleteRetentionPolicy' smart constructor.
deleteRetentionPolicy :: Text -> DeleteRetentionPolicy
deleteRetentionPolicy pLogGroupName = DeleteRetentionPolicy'{_drpLogGroupName = pLogGroupName};

-- | The name of the log group that is associated with the retention policy
-- to delete.
drpLogGroupName :: Lens' DeleteRetentionPolicy Text
drpLogGroupName = lens _drpLogGroupName (\ s a -> s{_drpLogGroupName = a});

instance AWSRequest DeleteRetentionPolicy where
        type Sv DeleteRetentionPolicy = CloudWatchLogs
        type Rs DeleteRetentionPolicy =
             DeleteRetentionPolicyResponse
        request = postJSON
        response = receiveNull DeleteRetentionPolicyResponse'

instance ToHeaders DeleteRetentionPolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.DeleteRetentionPolicy" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteRetentionPolicy where
        toJSON DeleteRetentionPolicy'{..}
          = object ["logGroupName" .= _drpLogGroupName]

instance ToPath DeleteRetentionPolicy where
        toPath = const "/"

instance ToQuery DeleteRetentionPolicy where
        toQuery = const mempty

-- | /See:/ 'deleteRetentionPolicyResponse' smart constructor.
data DeleteRetentionPolicyResponse = DeleteRetentionPolicyResponse' deriving (Eq, Read, Show)

-- | 'DeleteRetentionPolicyResponse' smart constructor.
deleteRetentionPolicyResponse :: DeleteRetentionPolicyResponse
deleteRetentionPolicyResponse = DeleteRetentionPolicyResponse';
