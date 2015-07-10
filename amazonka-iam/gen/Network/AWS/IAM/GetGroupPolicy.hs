{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetGroupPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified inline policy document that is embedded in the
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
    , ggprStatus
    , ggprGroupName
    , ggprPolicyName
    , ggprPolicyDocument
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getGroupPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ggpGroupName'
--
-- * 'ggpPolicyName'
data GetGroupPolicy = GetGroupPolicy'
    { _ggpGroupName  :: !Text
    , _ggpPolicyName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetGroupPolicy' smart constructor.
getGroupPolicy :: Text -> Text -> GetGroupPolicy
getGroupPolicy pGroupName pPolicyName =
    GetGroupPolicy'
    { _ggpGroupName = pGroupName
    , _ggpPolicyName = pPolicyName
    }

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
                   (pure (fromEnum s)) <*> (x .@ "GroupName") <*>
                     (x .@ "PolicyName")
                     <*> (x .@ "PolicyDocument"))

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

-- | Contains the response to a successful GetGroupPolicy request.
--
-- /See:/ 'getGroupPolicyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ggprStatus'
--
-- * 'ggprGroupName'
--
-- * 'ggprPolicyName'
--
-- * 'ggprPolicyDocument'
data GetGroupPolicyResponse = GetGroupPolicyResponse'
    { _ggprStatus         :: !Int
    , _ggprGroupName      :: !Text
    , _ggprPolicyName     :: !Text
    , _ggprPolicyDocument :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetGroupPolicyResponse' smart constructor.
getGroupPolicyResponse :: Int -> Text -> Text -> Text -> GetGroupPolicyResponse
getGroupPolicyResponse pStatus pGroupName pPolicyName pPolicyDocument =
    GetGroupPolicyResponse'
    { _ggprStatus = pStatus
    , _ggprGroupName = pGroupName
    , _ggprPolicyName = pPolicyName
    , _ggprPolicyDocument = pPolicyDocument
    }

-- | FIXME: Undocumented member.
ggprStatus :: Lens' GetGroupPolicyResponse Int
ggprStatus = lens _ggprStatus (\ s a -> s{_ggprStatus = a});

-- | The group the policy is associated with.
ggprGroupName :: Lens' GetGroupPolicyResponse Text
ggprGroupName = lens _ggprGroupName (\ s a -> s{_ggprGroupName = a});

-- | The name of the policy.
ggprPolicyName :: Lens' GetGroupPolicyResponse Text
ggprPolicyName = lens _ggprPolicyName (\ s a -> s{_ggprPolicyName = a});

-- | The policy document.
ggprPolicyDocument :: Lens' GetGroupPolicyResponse Text
ggprPolicyDocument = lens _ggprPolicyDocument (\ s a -> s{_ggprPolicyDocument = a});
