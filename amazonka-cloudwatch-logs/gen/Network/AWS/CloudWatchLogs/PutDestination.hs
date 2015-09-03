{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.PutDestination
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a 'Destination'. A destination encapsulates a
-- physical resource (such as a Kinesis stream) and allows you to subscribe
-- to a real-time stream of log events of a different account, ingested
-- through 'PutLogEvents' requests. Currently, the only supported physical
-- resource is a Amazon Kinesis stream belonging to the same account as the
-- destination.
--
-- A destination controls what is written to its Amazon Kinesis stream
-- through an access policy. By default, PutDestination does not set any
-- access policy with the destination, which means a cross-account user
-- will not be able to call 'PutSubscriptionFilter' against this
-- destination. To enable that, the destination owner must call
-- 'PutDestinationPolicy' after PutDestination.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutDestination.html AWS API Reference> for PutDestination.
module Network.AWS.CloudWatchLogs.PutDestination
    (
    -- * Creating a Request
      putDestination
    , PutDestination
    -- * Request Lenses
    , pdDestinationName
    , pdTargetARN
    , pdRoleARN

    -- * Destructuring the Response
    , putDestinationResponse
    , PutDestinationResponse
    -- * Response Lenses
    , pdrsDestination
    , pdrsResponseStatus
    ) where

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.CloudWatchLogs.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'putDestination' smart constructor.
data PutDestination = PutDestination'
    { _pdDestinationName :: !Text
    , _pdTargetARN       :: !Text
    , _pdRoleARN         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdDestinationName'
--
-- * 'pdTargetARN'
--
-- * 'pdRoleARN'
putDestination
    :: Text -- ^ 'pdDestinationName'
    -> Text -- ^ 'pdTargetARN'
    -> Text -- ^ 'pdRoleARN'
    -> PutDestination
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
        type Rs PutDestination = PutDestinationResponse
        request = postJSON cloudWatchLogs
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
              (catMaybes
                 [Just ("destinationName" .= _pdDestinationName),
                  Just ("targetArn" .= _pdTargetARN),
                  Just ("roleArn" .= _pdRoleARN)])

instance ToPath PutDestination where
        toPath = const "/"

instance ToQuery PutDestination where
        toQuery = const mempty

-- | /See:/ 'putDestinationResponse' smart constructor.
data PutDestinationResponse = PutDestinationResponse'
    { _pdrsDestination    :: !(Maybe Destination)
    , _pdrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutDestinationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdrsDestination'
--
-- * 'pdrsResponseStatus'
putDestinationResponse
    :: Int -- ^ 'pdrsResponseStatus'
    -> PutDestinationResponse
putDestinationResponse pResponseStatus_ =
    PutDestinationResponse'
    { _pdrsDestination = Nothing
    , _pdrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
pdrsDestination :: Lens' PutDestinationResponse (Maybe Destination)
pdrsDestination = lens _pdrsDestination (\ s a -> s{_pdrsDestination = a});

-- | The response status code.
pdrsResponseStatus :: Lens' PutDestinationResponse Int
pdrsResponseStatus = lens _pdrsResponseStatus (\ s a -> s{_pdrsResponseStatus = a});
