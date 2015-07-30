{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.PutDestination
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a @Destination@. A destination encapsulates a
-- physical resource (such as a Kinesis stream) and allows you to subscribe
-- to a real-time stream of log events of a different account, ingested
-- through @PutLogEvents@ requests. Currently, the only supported physical
-- resource is a Amazon Kinesis stream belonging to the same account as the
-- destination.
--
-- A destination controls what is written to its Amazon Kinesis stream
-- through an access policy. By default, PutDestination does not set any
-- access policy with the destination, which means a cross-account user
-- will not be able to call @PutSubscriptionFilter@ against this
-- destination. To enable that, the destination owner must call
-- @PutDestinationPolicy@ after PutDestination.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutDestination.html>
module Network.AWS.CloudWatchLogs.PutDestination
    (
    -- * Request
      PutDestination
    -- ** Request constructor
    , putDestination
    -- ** Request lenses
    , pdDestinationName
    , pdTargetARN
    , pdRoleARN

    -- * Response
    , PutDestinationResponse
    -- ** Response constructor
    , putDestinationResponse
    -- ** Response lenses
    , pdrsDestination
    , pdrsStatus
    ) where

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'putDestination' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pdDestinationName'
--
-- * 'pdTargetARN'
--
-- * 'pdRoleARN'
data PutDestination = PutDestination'
    { _pdDestinationName :: !Text
    , _pdTargetARN       :: !Text
    , _pdRoleARN         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutDestination' smart constructor.
putDestination :: Text -> Text -> Text -> PutDestination
putDestination pDestinationName_ pTargetARN_ pRoleARN_ =
    PutDestination'
    { _pdDestinationName = pDestinationName_
    , _pdTargetARN = pTargetARN_
    , _pdRoleARN = pRoleARN_
    }

-- | A name for the destination.
pdDestinationName :: Lens' PutDestination Text
pdDestinationName = lens _pdDestinationName (\ s a -> s{_pdDestinationName = a});

-- | The ARN of an Amazon Kinesis stream to deliver matching log events to.
pdTargetARN :: Lens' PutDestination Text
pdTargetARN = lens _pdTargetARN (\ s a -> s{_pdTargetARN = a});

-- | The ARN of an IAM role that grants Amazon CloudWatch Logs permissions to
-- do Amazon Kinesis PutRecord requests on the desitnation stream.
pdRoleARN :: Lens' PutDestination Text
pdRoleARN = lens _pdRoleARN (\ s a -> s{_pdRoleARN = a});

instance AWSRequest PutDestination where
        type Sv PutDestination = CloudWatchLogs
        type Rs PutDestination = PutDestinationResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 PutDestinationResponse' <$>
                   (x .?> "destination") <*> (pure (fromEnum s)))

instance ToHeaders PutDestination where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.PutDestination" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutDestination where
        toJSON PutDestination'{..}
          = object
              ["destinationName" .= _pdDestinationName,
               "targetArn" .= _pdTargetARN, "roleArn" .= _pdRoleARN]

instance ToPath PutDestination where
        toPath = const mempty

instance ToQuery PutDestination where
        toQuery = const mempty

-- | /See:/ 'putDestinationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pdrsDestination'
--
-- * 'pdrsStatus'
data PutDestinationResponse = PutDestinationResponse'
    { _pdrsDestination :: !(Maybe Destination)
    , _pdrsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutDestinationResponse' smart constructor.
putDestinationResponse :: Int -> PutDestinationResponse
putDestinationResponse pStatus_ =
    PutDestinationResponse'
    { _pdrsDestination = Nothing
    , _pdrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
pdrsDestination :: Lens' PutDestinationResponse (Maybe Destination)
pdrsDestination = lens _pdrsDestination (\ s a -> s{_pdrsDestination = a});

-- | FIXME: Undocumented member.
pdrsStatus :: Lens' PutDestinationResponse Int
pdrsStatus = lens _pdrsStatus (\ s a -> s{_pdrsStatus = a});
