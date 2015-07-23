{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DeleteRetentionPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the retention policy of the specified log group. Log events
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
    , drprqLogGroupName

    -- * Response
    , DeleteRetentionPolicyResponse
    -- ** Response constructor
    , deleteRetentionPolicyResponse
    ) where

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteRetentionPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drprqLogGroupName'
newtype DeleteRetentionPolicy = DeleteRetentionPolicy'
    { _drprqLogGroupName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteRetentionPolicy' smart constructor.
deleteRetentionPolicy :: Text -> DeleteRetentionPolicy
deleteRetentionPolicy pLogGroupName_ =
    DeleteRetentionPolicy'
    { _drprqLogGroupName = pLogGroupName_
    }

-- | The name of the log group that is associated with the retention policy
-- to delete.
drprqLogGroupName :: Lens' DeleteRetentionPolicy Text
drprqLogGroupName = lens _drprqLogGroupName (\ s a -> s{_drprqLogGroupName = a});

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
          = object ["logGroupName" .= _drprqLogGroupName]

instance ToPath DeleteRetentionPolicy where
        toPath = const "/"

instance ToQuery DeleteRetentionPolicy where
        toQuery = const mempty

-- | /See:/ 'deleteRetentionPolicyResponse' smart constructor.
data DeleteRetentionPolicyResponse =
    DeleteRetentionPolicyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteRetentionPolicyResponse' smart constructor.
deleteRetentionPolicyResponse :: DeleteRetentionPolicyResponse
deleteRetentionPolicyResponse = DeleteRetentionPolicyResponse'
