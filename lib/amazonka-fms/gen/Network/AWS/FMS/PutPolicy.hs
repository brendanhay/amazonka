{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.PutPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Firewall Manager policy.
--
--
-- Firewall Manager provides the following types of policies:
--
--     * An AWS WAF policy (type WAFV2), which defines rule groups to run first in the corresponding AWS WAF web ACL and rule groups to run last in the web ACL.
--
--     * An AWS WAF Classic policy (type WAF), which defines a rule group.
--
--     * A Shield Advanced policy, which applies Shield Advanced protection to specified accounts and resources.
--
--     * A security group policy, which manages VPC security groups across your AWS organization.
--
--     * An AWS Network Firewall policy, which provides firewall rules to filter network traffic in specified Amazon VPCs.
--
--
--
-- Each policy is specific to one of the types. If you want to enforce more than one policy type across accounts, create multiple policies. You can create multiple policies for each type.
--
-- You must be subscribed to Shield Advanced to create a Shield Advanced policy. For more information about subscribing to Shield Advanced, see <https://docs.aws.amazon.com/waf/latest/DDOSAPIReference/API_CreateSubscription.html CreateSubscription> .
module Network.AWS.FMS.PutPolicy
  ( -- * Creating a Request
    putPolicy,
    PutPolicy,

    -- * Request Lenses
    ppTagList,
    ppPolicy,

    -- * Destructuring the Response
    putPolicyResponse,
    PutPolicyResponse,

    -- * Response Lenses
    pprsPolicyARN,
    pprsPolicy,
    pprsResponseStatus,
  )
where

import Network.AWS.FMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putPolicy' smart constructor.
data PutPolicy = PutPolicy'
  { _ppTagList :: !(Maybe [Tag]),
    _ppPolicy :: !Policy
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppTagList' - The tags to add to the AWS resource.
--
-- * 'ppPolicy' - The details of the AWS Firewall Manager policy to be created.
putPolicy ::
  -- | 'ppPolicy'
  Policy ->
  PutPolicy
putPolicy pPolicy_ =
  PutPolicy' {_ppTagList = Nothing, _ppPolicy = pPolicy_}

-- | The tags to add to the AWS resource.
ppTagList :: Lens' PutPolicy [Tag]
ppTagList = lens _ppTagList (\s a -> s {_ppTagList = a}) . _Default . _Coerce

-- | The details of the AWS Firewall Manager policy to be created.
ppPolicy :: Lens' PutPolicy Policy
ppPolicy = lens _ppPolicy (\s a -> s {_ppPolicy = a})

instance AWSRequest PutPolicy where
  type Rs PutPolicy = PutPolicyResponse
  request = postJSON fms
  response =
    receiveJSON
      ( \s h x ->
          PutPolicyResponse'
            <$> (x .?> "PolicyArn") <*> (x .?> "Policy") <*> (pure (fromEnum s))
      )

instance Hashable PutPolicy

instance NFData PutPolicy

instance ToHeaders PutPolicy where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSFMS_20180101.PutPolicy" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON PutPolicy where
  toJSON PutPolicy' {..} =
    object
      ( catMaybes
          [("TagList" .=) <$> _ppTagList, Just ("Policy" .= _ppPolicy)]
      )

instance ToPath PutPolicy where
  toPath = const "/"

instance ToQuery PutPolicy where
  toQuery = const mempty

-- | /See:/ 'putPolicyResponse' smart constructor.
data PutPolicyResponse = PutPolicyResponse'
  { _pprsPolicyARN ::
      !(Maybe Text),
    _pprsPolicy :: !(Maybe Policy),
    _pprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pprsPolicyARN' - The Amazon Resource Name (ARN) of the policy.
--
-- * 'pprsPolicy' - The details of the AWS Firewall Manager policy.
--
-- * 'pprsResponseStatus' - -- | The response status code.
putPolicyResponse ::
  -- | 'pprsResponseStatus'
  Int ->
  PutPolicyResponse
putPolicyResponse pResponseStatus_ =
  PutPolicyResponse'
    { _pprsPolicyARN = Nothing,
      _pprsPolicy = Nothing,
      _pprsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the policy.
pprsPolicyARN :: Lens' PutPolicyResponse (Maybe Text)
pprsPolicyARN = lens _pprsPolicyARN (\s a -> s {_pprsPolicyARN = a})

-- | The details of the AWS Firewall Manager policy.
pprsPolicy :: Lens' PutPolicyResponse (Maybe Policy)
pprsPolicy = lens _pprsPolicy (\s a -> s {_pprsPolicy = a})

-- | -- | The response status code.
pprsResponseStatus :: Lens' PutPolicyResponse Int
pprsResponseStatus = lens _pprsResponseStatus (\s a -> s {_pprsResponseStatus = a})

instance NFData PutPolicyResponse
