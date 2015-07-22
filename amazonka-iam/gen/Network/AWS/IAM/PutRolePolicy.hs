{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.PutRolePolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Adds (or updates) an inline policy document that is embedded in the
-- specified role.
--
-- When you embed an inline policy in a role, the inline policy is used as
-- the role\'s access (permissions) policy. The role\'s trust policy is
-- created at the same time as the role, using CreateRole. You can update a
-- role\'s trust policy using UpdateAssumeRolePolicy. For more information
-- about roles, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/roles-toplevel.html Using Roles to Delegate Permissions and Federate Identities>.
--
-- A role can also have a managed policy attached to it. To attach a
-- managed policy to a role, use AttachRolePolicy. To create a new managed
-- policy, use CreatePolicy. For information about policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- For information about limits on the number of inline policies that you
-- can embed with a role, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities>
-- in the /Using IAM/ guide.
--
-- Because policy documents can be large, you should use POST rather than
-- GET when calling @PutRolePolicy@. For general information about using
-- the Query API with IAM, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/IAM_UsingQueryAPI.html Making Query Requests>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_PutRolePolicy.html>
module Network.AWS.IAM.PutRolePolicy
    (
    -- * Request
      PutRolePolicy
    -- ** Request constructor
    , putRolePolicy
    -- ** Request lenses
    , prprqRoleName
    , prprqPolicyName
    , prprqPolicyDocument

    -- * Response
    , PutRolePolicyResponse
    -- ** Response constructor
    , putRolePolicyResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'putRolePolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prprqRoleName'
--
-- * 'prprqPolicyName'
--
-- * 'prprqPolicyDocument'
data PutRolePolicy = PutRolePolicy'
    { _prprqRoleName       :: !Text
    , _prprqPolicyName     :: !Text
    , _prprqPolicyDocument :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutRolePolicy' smart constructor.
putRolePolicy :: Text -> Text -> Text -> PutRolePolicy
putRolePolicy pRoleName pPolicyName pPolicyDocument =
    PutRolePolicy'
    { _prprqRoleName = pRoleName
    , _prprqPolicyName = pPolicyName
    , _prprqPolicyDocument = pPolicyDocument
    }

-- | The name of the role to associate the policy with.
prprqRoleName :: Lens' PutRolePolicy Text
prprqRoleName = lens _prprqRoleName (\ s a -> s{_prprqRoleName = a});

-- | The name of the policy document.
prprqPolicyName :: Lens' PutRolePolicy Text
prprqPolicyName = lens _prprqPolicyName (\ s a -> s{_prprqPolicyName = a});

-- | The policy document.
prprqPolicyDocument :: Lens' PutRolePolicy Text
prprqPolicyDocument = lens _prprqPolicyDocument (\ s a -> s{_prprqPolicyDocument = a});

instance AWSRequest PutRolePolicy where
        type Sv PutRolePolicy = IAM
        type Rs PutRolePolicy = PutRolePolicyResponse
        request = post
        response = receiveNull PutRolePolicyResponse'

instance ToHeaders PutRolePolicy where
        toHeaders = const mempty

instance ToPath PutRolePolicy where
        toPath = const "/"

instance ToQuery PutRolePolicy where
        toQuery PutRolePolicy'{..}
          = mconcat
              ["Action" =: ("PutRolePolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "RoleName" =: _prprqRoleName,
               "PolicyName" =: _prprqPolicyName,
               "PolicyDocument" =: _prprqPolicyDocument]

-- | /See:/ 'putRolePolicyResponse' smart constructor.
data PutRolePolicyResponse =
    PutRolePolicyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutRolePolicyResponse' smart constructor.
putRolePolicyResponse :: PutRolePolicyResponse
putRolePolicyResponse = PutRolePolicyResponse'
