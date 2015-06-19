{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.GetPolicyVersion
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

-- | Retrieves information about the specified version of the specified
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
    , gpvPolicyARN
    , gpvVersionId

    -- * Response
    , GetPolicyVersionResponse
    -- ** Response constructor
    , getPolicyVersionResponse
    -- ** Response lenses
    , gpvrPolicyVersion
    ) where

import Network.AWS.IAM.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getPolicyVersion' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gpvPolicyARN'
--
-- * 'gpvVersionId'
data GetPolicyVersion = GetPolicyVersion'{_gpvPolicyARN :: Text, _gpvVersionId :: Text} deriving (Eq, Read, Show)

-- | 'GetPolicyVersion' smart constructor.
getPolicyVersion :: Text -> Text -> GetPolicyVersion
getPolicyVersion pPolicyARN pVersionId = GetPolicyVersion'{_gpvPolicyARN = pPolicyARN, _gpvVersionId = pVersionId};

-- | FIXME: Undocumented member.
gpvPolicyARN :: Lens' GetPolicyVersion Text
gpvPolicyARN = lens _gpvPolicyARN (\ s a -> s{_gpvPolicyARN = a});

-- | Identifies the policy version to retrieve.
gpvVersionId :: Lens' GetPolicyVersion Text
gpvVersionId = lens _gpvVersionId (\ s a -> s{_gpvVersionId = a});

instance AWSRequest GetPolicyVersion where
        type Sv GetPolicyVersion = IAM
        type Rs GetPolicyVersion = GetPolicyVersionResponse
        request = post
        response
          = receiveXMLWrapper "GetPolicyVersionResult"
              (\ s h x ->
                 GetPolicyVersionResponse' <$>
                   (x .@? "PolicyVersion"))

instance ToHeaders GetPolicyVersion where
        toHeaders = const mempty

instance ToPath GetPolicyVersion where
        toPath = const "/"

instance ToQuery GetPolicyVersion where
        toQuery GetPolicyVersion'{..}
          = mconcat
              ["Action" =: ("GetPolicyVersion" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PolicyArn" =: _gpvPolicyARN,
               "VersionId" =: _gpvVersionId]

-- | /See:/ 'getPolicyVersionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gpvrPolicyVersion'
newtype GetPolicyVersionResponse = GetPolicyVersionResponse'{_gpvrPolicyVersion :: Maybe PolicyVersion} deriving (Eq, Read, Show)

-- | 'GetPolicyVersionResponse' smart constructor.
getPolicyVersionResponse :: GetPolicyVersionResponse
getPolicyVersionResponse = GetPolicyVersionResponse'{_gpvrPolicyVersion = Nothing};

-- | Information about the policy version.
--
-- For more information about managed policy versions, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies>
-- in the /Using IAM/ guide.
gpvrPolicyVersion :: Lens' GetPolicyVersionResponse (Maybe PolicyVersion)
gpvrPolicyVersion = lens _gpvrPolicyVersion (\ s a -> s{_gpvrPolicyVersion = a});
