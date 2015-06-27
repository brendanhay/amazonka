{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.CreatePolicy
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a new managed policy for your AWS account.
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
    , cprPolicy
    , cprStatus
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
    { _cpPath           :: Maybe Text
    , _cpDescription    :: Maybe Text
    , _cpPolicyName     :: Text
    , _cpPolicyDocument :: Text
    } deriving (Eq,Read,Show)

-- | 'CreatePolicy' smart constructor.
createPolicy :: Text -> Text -> CreatePolicy
createPolicy pPolicyName pPolicyDocument =
    CreatePolicy'
    { _cpPath = Nothing
    , _cpDescription = Nothing
    , _cpPolicyName = pPolicyName
    , _cpPolicyDocument = pPolicyDocument
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
        request = post
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
-- * 'cprPolicy'
--
-- * 'cprStatus'
data CreatePolicyResponse = CreatePolicyResponse'
    { _cprPolicy :: Maybe Policy
    , _cprStatus :: !Int
    } deriving (Eq,Read,Show)

-- | 'CreatePolicyResponse' smart constructor.
createPolicyResponse :: Int -> CreatePolicyResponse
createPolicyResponse pStatus =
    CreatePolicyResponse'
    { _cprPolicy = Nothing
    , _cprStatus = pStatus
    }

-- | Information about the policy.
cprPolicy :: Lens' CreatePolicyResponse (Maybe Policy)
cprPolicy = lens _cprPolicy (\ s a -> s{_cprPolicy = a});

-- | FIXME: Undocumented member.
cprStatus :: Lens' CreatePolicyResponse Int
cprStatus = lens _cprStatus (\ s a -> s{_cprStatus = a});
