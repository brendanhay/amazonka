{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.CreatePolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new managed policy for your AWS account.
--
-- This operation creates a policy version with a version identifier of
-- @v1@ and sets v1 as the policy\'s default version. For more information
-- about policy versions, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies>
-- in the /Using IAM/ guide.
--
-- For more information about managed policies in general, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreatePolicy.html>
module Network.AWS.IAM.CreatePolicy
    (
    -- * Request
      CreatePolicy
    -- ** Request constructor
    , createPolicy
    -- ** Request lenses
    , cpPath
    , cpDescription
    , cpPolicyName
    , cpPolicyDocument

    -- * Response
    , CreatePolicyResponse
    -- ** Response constructor
    , createPolicyResponse
    -- ** Response lenses
    , cprsPolicy
    , cprsStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpPath'
--
-- * 'cpDescription'
--
-- * 'cpPolicyName'
--
-- * 'cpPolicyDocument'
data CreatePolicy = CreatePolicy'
    { _cpPath           :: !(Maybe Text)
    , _cpDescription    :: !(Maybe Text)
    , _cpPolicyName     :: !Text
    , _cpPolicyDocument :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreatePolicy' smart constructor.
createPolicy :: Text -> Text -> CreatePolicy
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
-- in the /Using IAM/ guide.
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
        type Sv CreatePolicy = IAM
        type Rs CreatePolicy = CreatePolicyResponse
        request = post "CreatePolicy"
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

-- | Contains the response to a successful CreatePolicy request.
--
-- /See:/ 'createPolicyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cprsPolicy'
--
-- * 'cprsStatus'
data CreatePolicyResponse = CreatePolicyResponse'
    { _cprsPolicy :: !(Maybe Policy)
    , _cprsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreatePolicyResponse' smart constructor.
createPolicyResponse :: Int -> CreatePolicyResponse
createPolicyResponse pStatus_ =
    CreatePolicyResponse'
    { _cprsPolicy = Nothing
    , _cprsStatus = pStatus_
    }

-- | Information about the policy.
cprsPolicy :: Lens' CreatePolicyResponse (Maybe Policy)
cprsPolicy = lens _cprsPolicy (\ s a -> s{_cprsPolicy = a});

-- | FIXME: Undocumented member.
cprsStatus :: Lens' CreatePolicyResponse Int
cprsStatus = lens _cprsStatus (\ s a -> s{_cprsStatus = a});
