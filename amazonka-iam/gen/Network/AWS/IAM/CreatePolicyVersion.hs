{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.CreatePolicyVersion
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of the specified managed policy. To update a
-- managed policy, you create a new policy version. A managed policy can
-- have up to five versions. If the policy has five versions, you must
-- delete an existing version using DeletePolicyVersion before you create a
-- new version.
--
-- Optionally, you can set the new version as the policy\'s default
-- version. The default version is the operative version; that is, the
-- version that is in effect for the IAM users, groups, and roles that the
-- policy is attached to.
--
-- For more information about managed policy versions, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreatePolicyVersion.html>
module Network.AWS.IAM.CreatePolicyVersion
    (
    -- * Request
      CreatePolicyVersion
    -- ** Request constructor
    , createPolicyVersion
    -- ** Request lenses
    , cpvSetAsDefault
    , cpvPolicyARN
    , cpvPolicyDocument

    -- * Response
    , CreatePolicyVersionResponse
    -- ** Response constructor
    , createPolicyVersionResponse
    -- ** Response lenses
    , cpvrsPolicyVersion
    , cpvrsStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createPolicyVersion' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpvSetAsDefault'
--
-- * 'cpvPolicyARN'
--
-- * 'cpvPolicyDocument'
data CreatePolicyVersion = CreatePolicyVersion'
    { _cpvSetAsDefault   :: !(Maybe Bool)
    , _cpvPolicyARN      :: !Text
    , _cpvPolicyDocument :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreatePolicyVersion' smart constructor.
createPolicyVersion :: Text -> Text -> CreatePolicyVersion
createPolicyVersion pPolicyARN_ pPolicyDocument_ =
    CreatePolicyVersion'
    { _cpvSetAsDefault = Nothing
    , _cpvPolicyARN = pPolicyARN_
    , _cpvPolicyDocument = pPolicyDocument_
    }

-- | Specifies whether to set this version as the policy\'s default version.
--
-- When this parameter is @true@, the new policy version becomes the
-- operative version; that is, the version that is in effect for the IAM
-- users, groups, and roles that the policy is attached to.
--
-- For more information about managed policy versions, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies>
-- in the /Using IAM/ guide.
cpvSetAsDefault :: Lens' CreatePolicyVersion (Maybe Bool)
cpvSetAsDefault = lens _cpvSetAsDefault (\ s a -> s{_cpvSetAsDefault = a});

-- | FIXME: Undocumented member.
cpvPolicyARN :: Lens' CreatePolicyVersion Text
cpvPolicyARN = lens _cpvPolicyARN (\ s a -> s{_cpvPolicyARN = a});

-- | The policy document.
cpvPolicyDocument :: Lens' CreatePolicyVersion Text
cpvPolicyDocument = lens _cpvPolicyDocument (\ s a -> s{_cpvPolicyDocument = a});

instance AWSRequest CreatePolicyVersion where
        type Sv CreatePolicyVersion = IAM
        type Rs CreatePolicyVersion =
             CreatePolicyVersionResponse
        request = postQuery
        response
          = receiveXMLWrapper "CreatePolicyVersionResult"
              (\ s h x ->
                 CreatePolicyVersionResponse' <$>
                   (x .@? "PolicyVersion") <*> (pure (fromEnum s)))

instance ToHeaders CreatePolicyVersion where
        toHeaders = const mempty

instance ToPath CreatePolicyVersion where
        toPath = const "/"

instance ToQuery CreatePolicyVersion where
        toQuery CreatePolicyVersion'{..}
          = mconcat
              ["Action" =: ("CreatePolicyVersion" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "SetAsDefault" =: _cpvSetAsDefault,
               "PolicyArn" =: _cpvPolicyARN,
               "PolicyDocument" =: _cpvPolicyDocument]

-- | Contains the response to a successful CreatePolicyVersion request.
--
-- /See:/ 'createPolicyVersionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpvrsPolicyVersion'
--
-- * 'cpvrsStatus'
data CreatePolicyVersionResponse = CreatePolicyVersionResponse'
    { _cpvrsPolicyVersion :: !(Maybe PolicyVersion)
    , _cpvrsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreatePolicyVersionResponse' smart constructor.
createPolicyVersionResponse :: Int -> CreatePolicyVersionResponse
createPolicyVersionResponse pStatus_ =
    CreatePolicyVersionResponse'
    { _cpvrsPolicyVersion = Nothing
    , _cpvrsStatus = pStatus_
    }

-- | Information about the policy version.
cpvrsPolicyVersion :: Lens' CreatePolicyVersionResponse (Maybe PolicyVersion)
cpvrsPolicyVersion = lens _cpvrsPolicyVersion (\ s a -> s{_cpvrsPolicyVersion = a});

-- | FIXME: Undocumented member.
cpvrsStatus :: Lens' CreatePolicyVersionResponse Int
cpvrsStatus = lens _cpvrsStatus (\ s a -> s{_cpvrsStatus = a});
