{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetPolicyVersion
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified version of the specified
-- managed policy, including the policy document.
--
-- To list the available versions for a policy, use ListPolicyVersions.
--
-- This API retrieves information about managed policies. To retrieve
-- information about an inline policy that is embedded in a user, group, or
-- role, use the GetUserPolicy, GetGroupPolicy, or GetRolePolicy API.
--
-- For more information about the types of policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetPolicyVersion.html>
module Network.AWS.IAM.GetPolicyVersion
    (
    -- * Request
      GetPolicyVersion
    -- ** Request constructor
    , getPolicyVersion
    -- ** Request lenses
    , gpvrqPolicyARN
    , gpvrqVersionId

    -- * Response
    , GetPolicyVersionResponse
    -- ** Response constructor
    , getPolicyVersionResponse
    -- ** Response lenses
    , gpvrsPolicyVersion
    , gpvrsStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getPolicyVersion' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gpvrqPolicyARN'
--
-- * 'gpvrqVersionId'
data GetPolicyVersion = GetPolicyVersion'
    { _gpvrqPolicyARN :: !Text
    , _gpvrqVersionId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetPolicyVersion' smart constructor.
getPolicyVersion :: Text -> Text -> GetPolicyVersion
getPolicyVersion pPolicyARN pVersionId =
    GetPolicyVersion'
    { _gpvrqPolicyARN = pPolicyARN
    , _gpvrqVersionId = pVersionId
    }

-- | FIXME: Undocumented member.
gpvrqPolicyARN :: Lens' GetPolicyVersion Text
gpvrqPolicyARN = lens _gpvrqPolicyARN (\ s a -> s{_gpvrqPolicyARN = a});

-- | Identifies the policy version to retrieve.
gpvrqVersionId :: Lens' GetPolicyVersion Text
gpvrqVersionId = lens _gpvrqVersionId (\ s a -> s{_gpvrqVersionId = a});

instance AWSRequest GetPolicyVersion where
        type Sv GetPolicyVersion = IAM
        type Rs GetPolicyVersion = GetPolicyVersionResponse
        request = post
        response
          = receiveXMLWrapper "GetPolicyVersionResult"
              (\ s h x ->
                 GetPolicyVersionResponse' <$>
                   (x .@? "PolicyVersion") <*> (pure (fromEnum s)))

instance ToHeaders GetPolicyVersion where
        toHeaders = const mempty

instance ToPath GetPolicyVersion where
        toPath = const "/"

instance ToQuery GetPolicyVersion where
        toQuery GetPolicyVersion'{..}
          = mconcat
              ["Action" =: ("GetPolicyVersion" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PolicyArn" =: _gpvrqPolicyARN,
               "VersionId" =: _gpvrqVersionId]

-- | Contains the response to a successful GetPolicyVersion request.
--
-- /See:/ 'getPolicyVersionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gpvrsPolicyVersion'
--
-- * 'gpvrsStatus'
data GetPolicyVersionResponse = GetPolicyVersionResponse'
    { _gpvrsPolicyVersion :: !(Maybe PolicyVersion)
    , _gpvrsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetPolicyVersionResponse' smart constructor.
getPolicyVersionResponse :: Int -> GetPolicyVersionResponse
getPolicyVersionResponse pStatus =
    GetPolicyVersionResponse'
    { _gpvrsPolicyVersion = Nothing
    , _gpvrsStatus = pStatus
    }

-- | Information about the policy version.
--
-- For more information about managed policy versions, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies>
-- in the /Using IAM/ guide.
gpvrsPolicyVersion :: Lens' GetPolicyVersionResponse (Maybe PolicyVersion)
gpvrsPolicyVersion = lens _gpvrsPolicyVersion (\ s a -> s{_gpvrsPolicyVersion = a});

-- | FIXME: Undocumented member.
gpvrsStatus :: Lens' GetPolicyVersionResponse Int
gpvrsStatus = lens _gpvrsStatus (\ s a -> s{_gpvrsStatus = a});
