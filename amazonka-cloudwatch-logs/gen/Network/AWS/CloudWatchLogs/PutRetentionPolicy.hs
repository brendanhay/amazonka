{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.PutRetentionPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Sets the retention of the specified log group. A retention policy allows
-- you to configure the number of days you want to retain log events in the
-- specified log group.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutRetentionPolicy.html>
module Network.AWS.CloudWatchLogs.PutRetentionPolicy
    (
    -- * Request
      PutRetentionPolicy
    -- ** Request constructor
    , putRetentionPolicy
    -- ** Request lenses
    , prpLogGroupName
    , prpRetentionInDays

    -- * Response
    , PutRetentionPolicyResponse
    -- ** Response constructor
    , putRetentionPolicyResponse
    ) where

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'putRetentionPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prpLogGroupName'
--
-- * 'prpRetentionInDays'
data PutRetentionPolicy = PutRetentionPolicy'
    { _prpLogGroupName    :: !Text
    , _prpRetentionInDays :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutRetentionPolicy' smart constructor.
putRetentionPolicy :: Text -> Int -> PutRetentionPolicy
putRetentionPolicy pLogGroupName pRetentionInDays =
    PutRetentionPolicy'
    { _prpLogGroupName = pLogGroupName
    , _prpRetentionInDays = pRetentionInDays
    }

-- | The name of the log group to associate the retention policy with.
prpLogGroupName :: Lens' PutRetentionPolicy Text
prpLogGroupName = lens _prpLogGroupName (\ s a -> s{_prpLogGroupName = a});

-- | FIXME: Undocumented member.
prpRetentionInDays :: Lens' PutRetentionPolicy Int
prpRetentionInDays = lens _prpRetentionInDays (\ s a -> s{_prpRetentionInDays = a});

instance AWSRequest PutRetentionPolicy where
        type Sv PutRetentionPolicy = CloudWatchLogs
        type Rs PutRetentionPolicy =
             PutRetentionPolicyResponse
        request = postJSON
        response = receiveNull PutRetentionPolicyResponse'

instance ToHeaders PutRetentionPolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.PutRetentionPolicy" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutRetentionPolicy where
        toJSON PutRetentionPolicy'{..}
          = object
              ["logGroupName" .= _prpLogGroupName,
               "retentionInDays" .= _prpRetentionInDays]

instance ToPath PutRetentionPolicy where
        toPath = const "/"

instance ToQuery PutRetentionPolicy where
        toQuery = const mempty

-- | /See:/ 'putRetentionPolicyResponse' smart constructor.
data PutRetentionPolicyResponse =
    PutRetentionPolicyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutRetentionPolicyResponse' smart constructor.
putRetentionPolicyResponse :: PutRetentionPolicyResponse
putRetentionPolicyResponse = PutRetentionPolicyResponse'
