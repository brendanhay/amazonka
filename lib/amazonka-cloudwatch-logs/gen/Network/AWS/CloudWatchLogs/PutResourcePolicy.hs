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
-- Module      : Network.AWS.CloudWatchLogs.PutResourcePolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a resource policy allowing other AWS services to put log events to this account, such as Amazon Route 53. An account can have up to 50 resource policies per region.
--
--
module Network.AWS.CloudWatchLogs.PutResourcePolicy
    (
    -- * Creating a Request
      putResourcePolicy
    , PutResourcePolicy
    -- * Request Lenses
    , prpPolicyName
    , prpPolicyDocument

    -- * Destructuring the Response
    , putResourcePolicyResponse
    , PutResourcePolicyResponse
    -- * Response Lenses
    , prprsResourcePolicy
    , prprsResponseStatus
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putResourcePolicy' smart constructor.
data PutResourcePolicy = PutResourcePolicy'
  { _prpPolicyName     :: !(Maybe Text)
  , _prpPolicyDocument :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutResourcePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prpPolicyName' - Name of the new policy. This parameter is required.
--
-- * 'prpPolicyDocument' - Details of the new policy, including the identity of the principal that is enabled to put logs to this account. This is formatted as a JSON string. The following example creates a resource policy enabling the Route 53 service to put DNS query logs in to the specified log group. Replace "logArn" with the ARN of your CloudWatch Logs resource, such as a log group or log stream. { "Version": "2012-10-17" "Statement": [ { "Sid": "Route53LogsToCloudWatchLogs", "Effect": "Allow", "Principal": { "Service": [ "route53.amazonaws.com" ] }, "Action":"logs:PutLogEvents", "Resource": logArn } ] }
putResourcePolicy
    :: PutResourcePolicy
putResourcePolicy =
  PutResourcePolicy' {_prpPolicyName = Nothing, _prpPolicyDocument = Nothing}


-- | Name of the new policy. This parameter is required.
prpPolicyName :: Lens' PutResourcePolicy (Maybe Text)
prpPolicyName = lens _prpPolicyName (\ s a -> s{_prpPolicyName = a})

-- | Details of the new policy, including the identity of the principal that is enabled to put logs to this account. This is formatted as a JSON string. The following example creates a resource policy enabling the Route 53 service to put DNS query logs in to the specified log group. Replace "logArn" with the ARN of your CloudWatch Logs resource, such as a log group or log stream. { "Version": "2012-10-17" "Statement": [ { "Sid": "Route53LogsToCloudWatchLogs", "Effect": "Allow", "Principal": { "Service": [ "route53.amazonaws.com" ] }, "Action":"logs:PutLogEvents", "Resource": logArn } ] }
prpPolicyDocument :: Lens' PutResourcePolicy (Maybe Text)
prpPolicyDocument = lens _prpPolicyDocument (\ s a -> s{_prpPolicyDocument = a})

instance AWSRequest PutResourcePolicy where
        type Rs PutResourcePolicy = PutResourcePolicyResponse
        request = postJSON cloudWatchLogs
        response
          = receiveJSON
              (\ s h x ->
                 PutResourcePolicyResponse' <$>
                   (x .?> "resourcePolicy") <*> (pure (fromEnum s)))

instance Hashable PutResourcePolicy where

instance NFData PutResourcePolicy where

instance ToHeaders PutResourcePolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.PutResourcePolicy" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutResourcePolicy where
        toJSON PutResourcePolicy'{..}
          = object
              (catMaybes
                 [("policyName" .=) <$> _prpPolicyName,
                  ("policyDocument" .=) <$> _prpPolicyDocument])

instance ToPath PutResourcePolicy where
        toPath = const "/"

instance ToQuery PutResourcePolicy where
        toQuery = const mempty

-- | /See:/ 'putResourcePolicyResponse' smart constructor.
data PutResourcePolicyResponse = PutResourcePolicyResponse'
  { _prprsResourcePolicy :: !(Maybe ResourcePolicy)
  , _prprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutResourcePolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prprsResourcePolicy' - The new policy.
--
-- * 'prprsResponseStatus' - -- | The response status code.
putResourcePolicyResponse
    :: Int -- ^ 'prprsResponseStatus'
    -> PutResourcePolicyResponse
putResourcePolicyResponse pResponseStatus_ =
  PutResourcePolicyResponse'
    {_prprsResourcePolicy = Nothing, _prprsResponseStatus = pResponseStatus_}


-- | The new policy.
prprsResourcePolicy :: Lens' PutResourcePolicyResponse (Maybe ResourcePolicy)
prprsResourcePolicy = lens _prprsResourcePolicy (\ s a -> s{_prprsResourcePolicy = a})

-- | -- | The response status code.
prprsResponseStatus :: Lens' PutResourcePolicyResponse Int
prprsResponseStatus = lens _prprsResponseStatus (\ s a -> s{_prprsResponseStatus = a})

instance NFData PutResourcePolicyResponse where
