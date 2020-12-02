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
-- Module      : Network.AWS.Organizations.CreatePolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a policy of a specified type that you can attach to a root, an organizational unit (OU), or an individual AWS account.
--
--
-- For more information about policies and their use, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies.html Managing Organization Policies> .
--
-- This operation can be called only from the organization's master account.
--
module Network.AWS.Organizations.CreatePolicy
    (
    -- * Creating a Request
      createPolicy
    , CreatePolicy
    -- * Request Lenses
    , cpContent
    , cpDescription
    , cpName
    , cpType

    -- * Destructuring the Response
    , createPolicyResponse
    , CreatePolicyResponse
    -- * Response Lenses
    , cprsPolicy
    , cprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createPolicy' smart constructor.
data CreatePolicy = CreatePolicy'
  { _cpContent     :: !Text
  , _cpDescription :: !Text
  , _cpName        :: !Text
  , _cpType        :: !PolicyType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpContent' - The policy content to add to the new policy. For example, if you create a <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html service control policy> (SCP), this string must be JSON text that specifies the permissions that admins in attached accounts can delegate to their users, groups, and roles. For more information about the SCP syntax, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_reference_scp-syntax.html Service Control Policy Syntax> in the /AWS Organizations User Guide/ .
--
-- * 'cpDescription' - An optional description to assign to the policy.
--
-- * 'cpName' - The friendly name to assign to the policy. The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
--
-- * 'cpType' - The type of policy to create.
createPolicy
    :: Text -- ^ 'cpContent'
    -> Text -- ^ 'cpDescription'
    -> Text -- ^ 'cpName'
    -> PolicyType -- ^ 'cpType'
    -> CreatePolicy
createPolicy pContent_ pDescription_ pName_ pType_ =
  CreatePolicy'
    { _cpContent = pContent_
    , _cpDescription = pDescription_
    , _cpName = pName_
    , _cpType = pType_
    }


-- | The policy content to add to the new policy. For example, if you create a <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html service control policy> (SCP), this string must be JSON text that specifies the permissions that admins in attached accounts can delegate to their users, groups, and roles. For more information about the SCP syntax, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_reference_scp-syntax.html Service Control Policy Syntax> in the /AWS Organizations User Guide/ .
cpContent :: Lens' CreatePolicy Text
cpContent = lens _cpContent (\ s a -> s{_cpContent = a})

-- | An optional description to assign to the policy.
cpDescription :: Lens' CreatePolicy Text
cpDescription = lens _cpDescription (\ s a -> s{_cpDescription = a})

-- | The friendly name to assign to the policy. The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
cpName :: Lens' CreatePolicy Text
cpName = lens _cpName (\ s a -> s{_cpName = a})

-- | The type of policy to create.
cpType :: Lens' CreatePolicy PolicyType
cpType = lens _cpType (\ s a -> s{_cpType = a})

instance AWSRequest CreatePolicy where
        type Rs CreatePolicy = CreatePolicyResponse
        request = postJSON organizations
        response
          = receiveJSON
              (\ s h x ->
                 CreatePolicyResponse' <$>
                   (x .?> "Policy") <*> (pure (fromEnum s)))

instance Hashable CreatePolicy where

instance NFData CreatePolicy where

instance ToHeaders CreatePolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.CreatePolicy" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreatePolicy where
        toJSON CreatePolicy'{..}
          = object
              (catMaybes
                 [Just ("Content" .= _cpContent),
                  Just ("Description" .= _cpDescription),
                  Just ("Name" .= _cpName), Just ("Type" .= _cpType)])

instance ToPath CreatePolicy where
        toPath = const "/"

instance ToQuery CreatePolicy where
        toQuery = const mempty

-- | /See:/ 'createPolicyResponse' smart constructor.
data CreatePolicyResponse = CreatePolicyResponse'
  { _cprsPolicy         :: !(Maybe Policy)
  , _cprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprsPolicy' - A structure that contains details about the newly created policy.
--
-- * 'cprsResponseStatus' - -- | The response status code.
createPolicyResponse
    :: Int -- ^ 'cprsResponseStatus'
    -> CreatePolicyResponse
createPolicyResponse pResponseStatus_ =
  CreatePolicyResponse'
    {_cprsPolicy = Nothing, _cprsResponseStatus = pResponseStatus_}


-- | A structure that contains details about the newly created policy.
cprsPolicy :: Lens' CreatePolicyResponse (Maybe Policy)
cprsPolicy = lens _cprsPolicy (\ s a -> s{_cprsPolicy = a})

-- | -- | The response status code.
cprsResponseStatus :: Lens' CreatePolicyResponse Int
cprsResponseStatus = lens _cprsResponseStatus (\ s a -> s{_cprsResponseStatus = a})

instance NFData CreatePolicyResponse where
