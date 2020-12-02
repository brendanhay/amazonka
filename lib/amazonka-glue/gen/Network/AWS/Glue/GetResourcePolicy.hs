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
-- Module      : Network.AWS.Glue.GetResourcePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a specified resource policy.
module Network.AWS.Glue.GetResourcePolicy
  ( -- * Creating a Request
    getResourcePolicy,
    GetResourcePolicy,

    -- * Request Lenses
    grpResourceARN,

    -- * Destructuring the Response
    getResourcePolicyResponse,
    GetResourcePolicyResponse,

    -- * Response Lenses
    grprrsPolicyInJSON,
    grprrsUpdateTime,
    grprrsPolicyHash,
    grprrsCreateTime,
    grprrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getResourcePolicy' smart constructor.
newtype GetResourcePolicy = GetResourcePolicy'
  { _grpResourceARN ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetResourcePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grpResourceARN' - The ARN of the AWS Glue resource for the resource policy to be retrieved. For more information about AWS Glue resource ARNs, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-api-common.html#aws-glue-api-regex-aws-glue-arn-id AWS Glue ARN string pattern>
getResourcePolicy ::
  GetResourcePolicy
getResourcePolicy = GetResourcePolicy' {_grpResourceARN = Nothing}

-- | The ARN of the AWS Glue resource for the resource policy to be retrieved. For more information about AWS Glue resource ARNs, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-api-common.html#aws-glue-api-regex-aws-glue-arn-id AWS Glue ARN string pattern>
grpResourceARN :: Lens' GetResourcePolicy (Maybe Text)
grpResourceARN = lens _grpResourceARN (\s a -> s {_grpResourceARN = a})

instance AWSRequest GetResourcePolicy where
  type Rs GetResourcePolicy = GetResourcePolicyResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          GetResourcePolicyResponse'
            <$> (x .?> "PolicyInJson")
            <*> (x .?> "UpdateTime")
            <*> (x .?> "PolicyHash")
            <*> (x .?> "CreateTime")
            <*> (pure (fromEnum s))
      )

instance Hashable GetResourcePolicy

instance NFData GetResourcePolicy

instance ToHeaders GetResourcePolicy where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.GetResourcePolicy" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetResourcePolicy where
  toJSON GetResourcePolicy' {..} =
    object (catMaybes [("ResourceArn" .=) <$> _grpResourceARN])

instance ToPath GetResourcePolicy where
  toPath = const "/"

instance ToQuery GetResourcePolicy where
  toQuery = const mempty

-- | /See:/ 'getResourcePolicyResponse' smart constructor.
data GetResourcePolicyResponse = GetResourcePolicyResponse'
  { _grprrsPolicyInJSON ::
      !(Maybe Text),
    _grprrsUpdateTime :: !(Maybe POSIX),
    _grprrsPolicyHash :: !(Maybe Text),
    _grprrsCreateTime :: !(Maybe POSIX),
    _grprrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetResourcePolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grprrsPolicyInJSON' - Contains the requested policy document, in JSON format.
--
-- * 'grprrsUpdateTime' - The date and time at which the policy was last updated.
--
-- * 'grprrsPolicyHash' - Contains the hash value associated with this policy.
--
-- * 'grprrsCreateTime' - The date and time at which the policy was created.
--
-- * 'grprrsResponseStatus' - -- | The response status code.
getResourcePolicyResponse ::
  -- | 'grprrsResponseStatus'
  Int ->
  GetResourcePolicyResponse
getResourcePolicyResponse pResponseStatus_ =
  GetResourcePolicyResponse'
    { _grprrsPolicyInJSON = Nothing,
      _grprrsUpdateTime = Nothing,
      _grprrsPolicyHash = Nothing,
      _grprrsCreateTime = Nothing,
      _grprrsResponseStatus = pResponseStatus_
    }

-- | Contains the requested policy document, in JSON format.
grprrsPolicyInJSON :: Lens' GetResourcePolicyResponse (Maybe Text)
grprrsPolicyInJSON = lens _grprrsPolicyInJSON (\s a -> s {_grprrsPolicyInJSON = a})

-- | The date and time at which the policy was last updated.
grprrsUpdateTime :: Lens' GetResourcePolicyResponse (Maybe UTCTime)
grprrsUpdateTime = lens _grprrsUpdateTime (\s a -> s {_grprrsUpdateTime = a}) . mapping _Time

-- | Contains the hash value associated with this policy.
grprrsPolicyHash :: Lens' GetResourcePolicyResponse (Maybe Text)
grprrsPolicyHash = lens _grprrsPolicyHash (\s a -> s {_grprrsPolicyHash = a})

-- | The date and time at which the policy was created.
grprrsCreateTime :: Lens' GetResourcePolicyResponse (Maybe UTCTime)
grprrsCreateTime = lens _grprrsCreateTime (\s a -> s {_grprrsCreateTime = a}) . mapping _Time

-- | -- | The response status code.
grprrsResponseStatus :: Lens' GetResourcePolicyResponse Int
grprrsResponseStatus = lens _grprrsResponseStatus (\s a -> s {_grprrsResponseStatus = a})

instance NFData GetResourcePolicyResponse
