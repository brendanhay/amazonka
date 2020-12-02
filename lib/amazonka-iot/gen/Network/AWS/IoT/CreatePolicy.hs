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
-- Module      : Network.AWS.IoT.CreatePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS IoT policy.
--
--
-- The created policy is the default version for the policy. This operation creates a policy version with a version identifier of __1__ and sets __1__ as the policy's default version.
module Network.AWS.IoT.CreatePolicy
  ( -- * Creating a Request
    createPolicy,
    CreatePolicy,

    -- * Request Lenses
    cpTags,
    cpPolicyName,
    cpPolicyDocument,

    -- * Destructuring the Response
    createPolicyResponse,
    CreatePolicyResponse,

    -- * Response Lenses
    cprsPolicyName,
    cprsPolicyDocument,
    cprsPolicyVersionId,
    cprsPolicyARN,
    cprsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the CreatePolicy operation.
--
--
--
-- /See:/ 'createPolicy' smart constructor.
data CreatePolicy = CreatePolicy'
  { _cpTags :: !(Maybe [Tag]),
    _cpPolicyName :: !Text,
    _cpPolicyDocument :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreatePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpTags' - Metadata which can be used to manage the policy.
--
-- * 'cpPolicyName' - The policy name.
--
-- * 'cpPolicyDocument' - The JSON document that describes the policy. __policyDocument__ must have a minimum length of 1, with a maximum length of 2048, excluding whitespace.
createPolicy ::
  -- | 'cpPolicyName'
  Text ->
  -- | 'cpPolicyDocument'
  Text ->
  CreatePolicy
createPolicy pPolicyName_ pPolicyDocument_ =
  CreatePolicy'
    { _cpTags = Nothing,
      _cpPolicyName = pPolicyName_,
      _cpPolicyDocument = pPolicyDocument_
    }

-- | Metadata which can be used to manage the policy.
cpTags :: Lens' CreatePolicy [Tag]
cpTags = lens _cpTags (\s a -> s {_cpTags = a}) . _Default . _Coerce

-- | The policy name.
cpPolicyName :: Lens' CreatePolicy Text
cpPolicyName = lens _cpPolicyName (\s a -> s {_cpPolicyName = a})

-- | The JSON document that describes the policy. __policyDocument__ must have a minimum length of 1, with a maximum length of 2048, excluding whitespace.
cpPolicyDocument :: Lens' CreatePolicy Text
cpPolicyDocument = lens _cpPolicyDocument (\s a -> s {_cpPolicyDocument = a})

instance AWSRequest CreatePolicy where
  type Rs CreatePolicy = CreatePolicyResponse
  request = postJSON ioT
  response =
    receiveJSON
      ( \s h x ->
          CreatePolicyResponse'
            <$> (x .?> "policyName")
            <*> (x .?> "policyDocument")
            <*> (x .?> "policyVersionId")
            <*> (x .?> "policyArn")
            <*> (pure (fromEnum s))
      )

instance Hashable CreatePolicy

instance NFData CreatePolicy

instance ToHeaders CreatePolicy where
  toHeaders = const mempty

instance ToJSON CreatePolicy where
  toJSON CreatePolicy' {..} =
    object
      ( catMaybes
          [ ("tags" .=) <$> _cpTags,
            Just ("policyDocument" .= _cpPolicyDocument)
          ]
      )

instance ToPath CreatePolicy where
  toPath CreatePolicy' {..} =
    mconcat ["/policies/", toBS _cpPolicyName]

instance ToQuery CreatePolicy where
  toQuery = const mempty

-- | The output from the CreatePolicy operation.
--
--
--
-- /See:/ 'createPolicyResponse' smart constructor.
data CreatePolicyResponse = CreatePolicyResponse'
  { _cprsPolicyName ::
      !(Maybe Text),
    _cprsPolicyDocument :: !(Maybe Text),
    _cprsPolicyVersionId :: !(Maybe Text),
    _cprsPolicyARN :: !(Maybe Text),
    _cprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreatePolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprsPolicyName' - The policy name.
--
-- * 'cprsPolicyDocument' - The JSON document that describes the policy.
--
-- * 'cprsPolicyVersionId' - The policy version ID.
--
-- * 'cprsPolicyARN' - The policy ARN.
--
-- * 'cprsResponseStatus' - -- | The response status code.
createPolicyResponse ::
  -- | 'cprsResponseStatus'
  Int ->
  CreatePolicyResponse
createPolicyResponse pResponseStatus_ =
  CreatePolicyResponse'
    { _cprsPolicyName = Nothing,
      _cprsPolicyDocument = Nothing,
      _cprsPolicyVersionId = Nothing,
      _cprsPolicyARN = Nothing,
      _cprsResponseStatus = pResponseStatus_
    }

-- | The policy name.
cprsPolicyName :: Lens' CreatePolicyResponse (Maybe Text)
cprsPolicyName = lens _cprsPolicyName (\s a -> s {_cprsPolicyName = a})

-- | The JSON document that describes the policy.
cprsPolicyDocument :: Lens' CreatePolicyResponse (Maybe Text)
cprsPolicyDocument = lens _cprsPolicyDocument (\s a -> s {_cprsPolicyDocument = a})

-- | The policy version ID.
cprsPolicyVersionId :: Lens' CreatePolicyResponse (Maybe Text)
cprsPolicyVersionId = lens _cprsPolicyVersionId (\s a -> s {_cprsPolicyVersionId = a})

-- | The policy ARN.
cprsPolicyARN :: Lens' CreatePolicyResponse (Maybe Text)
cprsPolicyARN = lens _cprsPolicyARN (\s a -> s {_cprsPolicyARN = a})

-- | -- | The response status code.
cprsResponseStatus :: Lens' CreatePolicyResponse Int
cprsResponseStatus = lens _cprsResponseStatus (\s a -> s {_cprsResponseStatus = a})

instance NFData CreatePolicyResponse
