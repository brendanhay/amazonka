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
-- Module      : Network.AWS.IAM.CreatePolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new managed policy for your AWS account.
--
-- This operation creates a policy version with a version identifier of
-- 'v1' and sets v1 as the policy\'s default version. For more information
-- about policy versions, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies>
-- in the /IAM User Guide/.
--
-- For more information about managed policies in general, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /IAM User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreatePolicy.html AWS API Reference> for CreatePolicy.
module Network.AWS.IAM.CreatePolicy
    (
    -- * Creating a Request
      createPolicy
    , CreatePolicy
    -- * Request Lenses
    , cpPath
    , cpDescription
    , cpPolicyName
    , cpPolicyDocument

    -- * Destructuring the Response
    , createPolicyResponse
    , CreatePolicyResponse
    -- * Response Lenses
    , cprsPolicy
    , cprsResponseStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createPolicy' smart constructor.
data CreatePolicy = CreatePolicy'
    { _cpPath           :: !(Maybe Text)
    , _cpDescription    :: !(Maybe Text)
    , _cpPolicyName     :: !Text
    , _cpPolicyDocument :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreatePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpPath'
--
-- * 'cpDescription'
--
-- * 'cpPolicyName'
--
-- * 'cpPolicyDocument'
createPolicy
    :: Text -- ^ 'cpPolicyName'
    -> Text -- ^ 'cpPolicyDocument'
    -> CreatePolicy
createPolicy pPolicyName_ pPolicyDocument_ =
    CreatePolicy'
    { _cpPath = Nothing
    , _cpDescription = Nothing
    , _cpPolicyName = pPolicyName_
    , _cpPolicyDocument = pPolicyDocument_
    }

-- | The path for the policy.
--
-- For more information about paths, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /IAM User Guide/.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/).
cpPath :: Lens' CreatePolicy (Maybe Text)
cpPath = lens _cpPath (\ s a -> s{_cpPath = a});

-- | A friendly description of the policy.
--
-- Typically used to store information about the permissions defined in the
-- policy. For example, \"Grants access to production DynamoDB tables.\"
--
-- The policy description is immutable. After a value is assigned, it
-- cannot be changed.
cpDescription :: Lens' CreatePolicy (Maybe Text)
cpDescription = lens _cpDescription (\ s a -> s{_cpDescription = a});

-- | The name of the policy document.
cpPolicyName :: Lens' CreatePolicy Text
cpPolicyName = lens _cpPolicyName (\ s a -> s{_cpPolicyName = a});

-- | The policy document.
cpPolicyDocument :: Lens' CreatePolicy Text
cpPolicyDocument = lens _cpPolicyDocument (\ s a -> s{_cpPolicyDocument = a});

instance AWSRequest CreatePolicy where
        type Rs CreatePolicy = CreatePolicyResponse
        request = postQuery iAM
        response
          = receiveXMLWrapper "CreatePolicyResult"
              (\ s h x ->
                 CreatePolicyResponse' <$>
                   (x .@? "Policy") <*> (pure (fromEnum s)))

instance ToHeaders CreatePolicy where
        toHeaders = const mempty

instance ToPath CreatePolicy where
        toPath = const "/"

instance ToQuery CreatePolicy where
        toQuery CreatePolicy'{..}
          = mconcat
              ["Action" =: ("CreatePolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "Path" =: _cpPath, "Description" =: _cpDescription,
               "PolicyName" =: _cpPolicyName,
               "PolicyDocument" =: _cpPolicyDocument]

-- | Contains the response to a successful < CreatePolicy> request.
--
-- /See:/ 'createPolicyResponse' smart constructor.
data CreatePolicyResponse = CreatePolicyResponse'
    { _cprsPolicy         :: !(Maybe Policy)
    , _cprsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreatePolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprsPolicy'
--
-- * 'cprsResponseStatus'
createPolicyResponse
    :: Int -- ^ 'cprsResponseStatus'
    -> CreatePolicyResponse
createPolicyResponse pResponseStatus_ =
    CreatePolicyResponse'
    { _cprsPolicy = Nothing
    , _cprsResponseStatus = pResponseStatus_
    }

-- | Information about the policy.
cprsPolicy :: Lens' CreatePolicyResponse (Maybe Policy)
cprsPolicy = lens _cprsPolicy (\ s a -> s{_cprsPolicy = a});

-- | The response status code.
cprsResponseStatus :: Lens' CreatePolicyResponse Int
cprsResponseStatus = lens _cprsResponseStatus (\ s a -> s{_cprsResponseStatus = a});
