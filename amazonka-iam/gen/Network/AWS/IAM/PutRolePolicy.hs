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
-- Module      : Network.AWS.IAM.PutRolePolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates an inline policy document that is embedded in the specified IAM role.
--
--
-- When you embed an inline policy in a role, the inline policy is used as part of the role's access (permissions) policy. The role's trust policy is created at the same time as the role, using 'CreateRole' . You can update a role's trust policy using 'UpdateAssumeRolePolicy' . For more information about IAM roles, go to <http://docs.aws.amazon.com/IAM/latest/UserGuide/roles-toplevel.html Using Roles to Delegate Permissions and Federate Identities> .
--
-- A role can also have a managed policy attached to it. To attach a managed policy to a role, use 'AttachRolePolicy' . To create a new managed policy, use 'CreatePolicy' . For information about policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
-- For information about limits on the number of inline policies that you can embed with a role, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities> in the /IAM User Guide/ .
--
module Network.AWS.IAM.PutRolePolicy
    (
    -- * Creating a Request
      putRolePolicy
    , PutRolePolicy
    -- * Request Lenses
    , prpRoleName
    , prpPolicyName
    , prpPolicyDocument

    -- * Destructuring the Response
    , putRolePolicyResponse
    , PutRolePolicyResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putRolePolicy' smart constructor.
data PutRolePolicy = PutRolePolicy'
  { _prpRoleName       :: !Text
  , _prpPolicyName     :: !Text
  , _prpPolicyDocument :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutRolePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prpRoleName' - The name of the role to associate the policy with. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'prpPolicyName' - The name of the policy document. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'prpPolicyDocument' - The policy document. The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:     * Any printable ASCII character ranging from the space character (\u0020) through the end of the ASCII character range     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through \u00FF)     * The special characters tab (\u0009), line feed (\u000A), and carriage return (\u000D)
putRolePolicy
    :: Text -- ^ 'prpRoleName'
    -> Text -- ^ 'prpPolicyName'
    -> Text -- ^ 'prpPolicyDocument'
    -> PutRolePolicy
putRolePolicy pRoleName_ pPolicyName_ pPolicyDocument_ =
  PutRolePolicy'
    { _prpRoleName = pRoleName_
    , _prpPolicyName = pPolicyName_
    , _prpPolicyDocument = pPolicyDocument_
    }


-- | The name of the role to associate the policy with. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
prpRoleName :: Lens' PutRolePolicy Text
prpRoleName = lens _prpRoleName (\ s a -> s{_prpRoleName = a})

-- | The name of the policy document. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
prpPolicyName :: Lens' PutRolePolicy Text
prpPolicyName = lens _prpPolicyName (\ s a -> s{_prpPolicyName = a})

-- | The policy document. The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:     * Any printable ASCII character ranging from the space character (\u0020) through the end of the ASCII character range     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through \u00FF)     * The special characters tab (\u0009), line feed (\u000A), and carriage return (\u000D)
prpPolicyDocument :: Lens' PutRolePolicy Text
prpPolicyDocument = lens _prpPolicyDocument (\ s a -> s{_prpPolicyDocument = a})

instance AWSRequest PutRolePolicy where
        type Rs PutRolePolicy = PutRolePolicyResponse
        request = postQuery iam
        response = receiveNull PutRolePolicyResponse'

instance Hashable PutRolePolicy where

instance NFData PutRolePolicy where

instance ToHeaders PutRolePolicy where
        toHeaders = const mempty

instance ToPath PutRolePolicy where
        toPath = const "/"

instance ToQuery PutRolePolicy where
        toQuery PutRolePolicy'{..}
          = mconcat
              ["Action" =: ("PutRolePolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "RoleName" =: _prpRoleName,
               "PolicyName" =: _prpPolicyName,
               "PolicyDocument" =: _prpPolicyDocument]

-- | /See:/ 'putRolePolicyResponse' smart constructor.
data PutRolePolicyResponse =
  PutRolePolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutRolePolicyResponse' with the minimum fields required to make a request.
--
putRolePolicyResponse
    :: PutRolePolicyResponse
putRolePolicyResponse = PutRolePolicyResponse'


instance NFData PutRolePolicyResponse where
