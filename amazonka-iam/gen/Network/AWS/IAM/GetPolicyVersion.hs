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
-- Module      : Network.AWS.IAM.GetPolicyVersion
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified version of the specified
-- managed policy, including the policy document.
--
-- To list the available versions for a policy, use < ListPolicyVersions>.
--
-- This API retrieves information about managed policies. To retrieve
-- information about an inline policy that is embedded in a user, group, or
-- role, use the < GetUserPolicy>, < GetGroupPolicy>, or < GetRolePolicy>
-- API.
--
-- For more information about the types of policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /IAM User Guide/.
module Network.AWS.IAM.GetPolicyVersion
    (
    -- * Creating a Request
      getPolicyVersion
    , GetPolicyVersion
    -- * Request Lenses
    , gpvPolicyARN
    , gpvVersionId

    -- * Destructuring the Response
    , getPolicyVersionResponse
    , GetPolicyVersionResponse
    -- * Response Lenses
    , gpvrsPolicyVersion
    , gpvrsResponseStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getPolicyVersion' smart constructor.
data GetPolicyVersion = GetPolicyVersion'
    { _gpvPolicyARN :: !Text
    , _gpvVersionId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetPolicyVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpvPolicyARN'
--
-- * 'gpvVersionId'
getPolicyVersion
    :: Text -- ^ 'gpvPolicyARN'
    -> Text -- ^ 'gpvVersionId'
    -> GetPolicyVersion
getPolicyVersion pPolicyARN_ pVersionId_ =
    GetPolicyVersion'
    { _gpvPolicyARN = pPolicyARN_
    , _gpvVersionId = pVersionId_
    }

-- | Undocumented member.
gpvPolicyARN :: Lens' GetPolicyVersion Text
gpvPolicyARN = lens _gpvPolicyARN (\ s a -> s{_gpvPolicyARN = a});

-- | Identifies the policy version to retrieve.
gpvVersionId :: Lens' GetPolicyVersion Text
gpvVersionId = lens _gpvVersionId (\ s a -> s{_gpvVersionId = a});

instance AWSRequest GetPolicyVersion where
        type Rs GetPolicyVersion = GetPolicyVersionResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "GetPolicyVersionResult"
              (\ s h x ->
                 GetPolicyVersionResponse' <$>
                   (x .@? "PolicyVersion") <*> (pure (fromEnum s)))

instance Hashable GetPolicyVersion

instance NFData GetPolicyVersion

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

-- | Contains the response to a successful < GetPolicyVersion> request.
--
-- /See:/ 'getPolicyVersionResponse' smart constructor.
data GetPolicyVersionResponse = GetPolicyVersionResponse'
    { _gpvrsPolicyVersion  :: !(Maybe PolicyVersion)
    , _gpvrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetPolicyVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpvrsPolicyVersion'
--
-- * 'gpvrsResponseStatus'
getPolicyVersionResponse
    :: Int -- ^ 'gpvrsResponseStatus'
    -> GetPolicyVersionResponse
getPolicyVersionResponse pResponseStatus_ =
    GetPolicyVersionResponse'
    { _gpvrsPolicyVersion = Nothing
    , _gpvrsResponseStatus = pResponseStatus_
    }

-- | Information about the policy version.
--
-- For more information about managed policy versions, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies>
-- in the /IAM User Guide/.
gpvrsPolicyVersion :: Lens' GetPolicyVersionResponse (Maybe PolicyVersion)
gpvrsPolicyVersion = lens _gpvrsPolicyVersion (\ s a -> s{_gpvrsPolicyVersion = a});

-- | The response status code.
gpvrsResponseStatus :: Lens' GetPolicyVersionResponse Int
gpvrsResponseStatus = lens _gpvrsResponseStatus (\ s a -> s{_gpvrsResponseStatus = a});

instance NFData GetPolicyVersionResponse
