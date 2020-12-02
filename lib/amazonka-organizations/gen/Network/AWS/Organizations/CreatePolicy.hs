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
-- Module      : Network.AWS.Organizations.CreatePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a policy of a specified type that you can attach to a root, an organizational unit (OU), or an individual AWS account.
--
--
-- For more information about policies and their use, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies.html Managing Organization Policies> .
--
-- If the request includes tags, then the requester must have the @organizations:TagResource@ permission.
--
-- This operation can be called only from the organization's management account.
module Network.AWS.Organizations.CreatePolicy
  ( -- * Creating a Request
    createPolicy,
    CreatePolicy,

    -- * Request Lenses
    cpTags,
    cpContent,
    cpDescription,
    cpName,
    cpType,

    -- * Destructuring the Response
    createPolicyResponse,
    CreatePolicyResponse,

    -- * Response Lenses
    cprsPolicy,
    cprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createPolicy' smart constructor.
data CreatePolicy = CreatePolicy'
  { _cpTags :: !(Maybe [Tag]),
    _cpContent :: !Text,
    _cpDescription :: !Text,
    _cpName :: !Text,
    _cpType :: !PolicyType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreatePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpTags' - A list of tags that you want to attach to the newly created policy. For each tag in the list, you must specify both a tag key and a value. You can set the value to an empty string, but you can't set it to @null@ . For more information about tagging, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources> in the AWS Organizations User Guide.
--
-- * 'cpContent' - The policy text content to add to the new policy. The text that you supply must adhere to the rules of the policy type you specify in the @Type@ parameter.
--
-- * 'cpDescription' - An optional description to assign to the policy.
--
-- * 'cpName' - The friendly name to assign to the policy. The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
--
-- * 'cpType' - The type of policy to create. You can specify one of the following values:     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>      * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>      * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY>      * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
createPolicy ::
  -- | 'cpContent'
  Text ->
  -- | 'cpDescription'
  Text ->
  -- | 'cpName'
  Text ->
  -- | 'cpType'
  PolicyType ->
  CreatePolicy
createPolicy pContent_ pDescription_ pName_ pType_ =
  CreatePolicy'
    { _cpTags = Nothing,
      _cpContent = pContent_,
      _cpDescription = pDescription_,
      _cpName = pName_,
      _cpType = pType_
    }

-- | A list of tags that you want to attach to the newly created policy. For each tag in the list, you must specify both a tag key and a value. You can set the value to an empty string, but you can't set it to @null@ . For more information about tagging, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources> in the AWS Organizations User Guide.
cpTags :: Lens' CreatePolicy [Tag]
cpTags = lens _cpTags (\s a -> s {_cpTags = a}) . _Default . _Coerce

-- | The policy text content to add to the new policy. The text that you supply must adhere to the rules of the policy type you specify in the @Type@ parameter.
cpContent :: Lens' CreatePolicy Text
cpContent = lens _cpContent (\s a -> s {_cpContent = a})

-- | An optional description to assign to the policy.
cpDescription :: Lens' CreatePolicy Text
cpDescription = lens _cpDescription (\s a -> s {_cpDescription = a})

-- | The friendly name to assign to the policy. The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
cpName :: Lens' CreatePolicy Text
cpName = lens _cpName (\s a -> s {_cpName = a})

-- | The type of policy to create. You can specify one of the following values:     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>      * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>      * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY>      * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
cpType :: Lens' CreatePolicy PolicyType
cpType = lens _cpType (\s a -> s {_cpType = a})

instance AWSRequest CreatePolicy where
  type Rs CreatePolicy = CreatePolicyResponse
  request = postJSON organizations
  response =
    receiveJSON
      ( \s h x ->
          CreatePolicyResponse' <$> (x .?> "Policy") <*> (pure (fromEnum s))
      )

instance Hashable CreatePolicy

instance NFData CreatePolicy

instance ToHeaders CreatePolicy where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSOrganizationsV20161128.CreatePolicy" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreatePolicy where
  toJSON CreatePolicy' {..} =
    object
      ( catMaybes
          [ ("Tags" .=) <$> _cpTags,
            Just ("Content" .= _cpContent),
            Just ("Description" .= _cpDescription),
            Just ("Name" .= _cpName),
            Just ("Type" .= _cpType)
          ]
      )

instance ToPath CreatePolicy where
  toPath = const "/"

instance ToQuery CreatePolicy where
  toQuery = const mempty

-- | /See:/ 'createPolicyResponse' smart constructor.
data CreatePolicyResponse = CreatePolicyResponse'
  { _cprsPolicy ::
      !(Maybe Policy),
    _cprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreatePolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprsPolicy' - A structure that contains details about the newly created policy.
--
-- * 'cprsResponseStatus' - -- | The response status code.
createPolicyResponse ::
  -- | 'cprsResponseStatus'
  Int ->
  CreatePolicyResponse
createPolicyResponse pResponseStatus_ =
  CreatePolicyResponse'
    { _cprsPolicy = Nothing,
      _cprsResponseStatus = pResponseStatus_
    }

-- | A structure that contains details about the newly created policy.
cprsPolicy :: Lens' CreatePolicyResponse (Maybe Policy)
cprsPolicy = lens _cprsPolicy (\s a -> s {_cprsPolicy = a})

-- | -- | The response status code.
cprsResponseStatus :: Lens' CreatePolicyResponse Int
cprsResponseStatus = lens _cprsResponseStatus (\s a -> s {_cprsResponseStatus = a})

instance NFData CreatePolicyResponse
