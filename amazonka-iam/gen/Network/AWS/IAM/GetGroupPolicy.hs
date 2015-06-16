{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.GetGroupPolicy
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

-- | Retrieves the specified inline policy document that is embedded in the
-- specified group.
--
-- A group can also have managed policies attached to it. To retrieve a
-- managed policy document that is attached to a group, use GetPolicy to
-- determine the policy\'s default version, then use GetPolicyVersion to
-- retrieve the policy document.
--
-- For more information about policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetGroupPolicy.html>
module Network.AWS.IAM.GetGroupPolicy
    (
    -- * Request
      GetGroupPolicy
    -- ** Request constructor
    , getGroupPolicy
    -- ** Request lenses
    , ggpGroupName
    , ggpPolicyName

    -- * Response
    , GetGroupPolicyResponse
    -- ** Response constructor
    , getGroupPolicyResponse
    -- ** Response lenses
    , ggprGroupName
    , ggprPolicyName
    , ggprPolicyDocument
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.IAM.Types

-- | /See:/ 'getGroupPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ggpGroupName'
--
-- * 'ggpPolicyName'
data GetGroupPolicy = GetGroupPolicy'{_ggpGroupName :: Text, _ggpPolicyName :: Text} deriving (Eq, Read, Show)

-- | 'GetGroupPolicy' smart constructor.
getGroupPolicy :: Text -> Text -> GetGroupPolicy
getGroupPolicy pGroupName pPolicyName = GetGroupPolicy'{_ggpGroupName = pGroupName, _ggpPolicyName = pPolicyName};

-- | The name of the group the policy is associated with.
ggpGroupName :: Lens' GetGroupPolicy Text
ggpGroupName = lens _ggpGroupName (\ s a -> s{_ggpGroupName = a});

-- | The name of the policy document to get.
ggpPolicyName :: Lens' GetGroupPolicy Text
ggpPolicyName = lens _ggpPolicyName (\ s a -> s{_ggpPolicyName = a});

instance AWSRequest GetGroupPolicy where
        type Sv GetGroupPolicy = IAM
        type Rs GetGroupPolicy = GetGroupPolicyResponse
        request = post
        response
          = receiveXMLWrapper "GetGroupPolicyResult"
              (\ s h x ->
                 GetGroupPolicyResponse' <$>
                   (x .@ "GroupName") <*> (x .@ "PolicyName") <*>
                     (x .@ "PolicyDocument"))

instance ToHeaders GetGroupPolicy where
        toHeaders = const mempty

instance ToPath GetGroupPolicy where
        toPath = const "/"

instance ToQuery GetGroupPolicy where
        toQuery GetGroupPolicy'{..}
          = mconcat
              ["Action" =: ("GetGroupPolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "GroupName" =: _ggpGroupName,
               "PolicyName" =: _ggpPolicyName]

-- | /See:/ 'getGroupPolicyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ggprGroupName'
--
-- * 'ggprPolicyName'
--
-- * 'ggprPolicyDocument'
data GetGroupPolicyResponse = GetGroupPolicyResponse'{_ggprGroupName :: Text, _ggprPolicyName :: Text, _ggprPolicyDocument :: Text} deriving (Eq, Read, Show)

-- | 'GetGroupPolicyResponse' smart constructor.
getGroupPolicyResponse :: Text -> Text -> Text -> GetGroupPolicyResponse
getGroupPolicyResponse pGroupName pPolicyName pPolicyDocument = GetGroupPolicyResponse'{_ggprGroupName = pGroupName, _ggprPolicyName = pPolicyName, _ggprPolicyDocument = pPolicyDocument};

-- | The group the policy is associated with.
ggprGroupName :: Lens' GetGroupPolicyResponse Text
ggprGroupName = lens _ggprGroupName (\ s a -> s{_ggprGroupName = a});

-- | The name of the policy.
ggprPolicyName :: Lens' GetGroupPolicyResponse Text
ggprPolicyName = lens _ggprPolicyName (\ s a -> s{_ggprPolicyName = a});

-- | The policy document.
ggprPolicyDocument :: Lens' GetGroupPolicyResponse Text
ggprPolicyDocument = lens _ggprPolicyDocument (\ s a -> s{_ggprPolicyDocument = a});
