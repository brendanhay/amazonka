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
    , ggprqGroupName
    , ggprqPolicyName

    -- * Response
    , GetGroupPolicyResponse
    -- ** Response constructor
    , getGroupPolicyResponse
    -- ** Response lenses
    , ggprsStatus
    , ggprsGroupName
    , ggprsPolicyName
    , ggprsPolicyDocument
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getGroupPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ggprqGroupName'
--
-- * 'ggprqPolicyName'
data GetGroupPolicy = GetGroupPolicy'
    { _ggprqGroupName  :: !Text
    , _ggprqPolicyName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetGroupPolicy' smart constructor.
getGroupPolicy :: Text -> Text -> GetGroupPolicy
getGroupPolicy pGroupName_ pPolicyName_ =
    GetGroupPolicy'
    { _ggprqGroupName = pGroupName_
    , _ggprqPolicyName = pPolicyName_
    }

-- | The name of the group the policy is associated with.
ggprqGroupName :: Lens' GetGroupPolicy Text
ggprqGroupName = lens _ggprqGroupName (\ s a -> s{_ggprqGroupName = a});

-- | The name of the policy document to get.
ggprqPolicyName :: Lens' GetGroupPolicy Text
ggprqPolicyName = lens _ggprqPolicyName (\ s a -> s{_ggprqPolicyName = a});

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
               "GroupName" =: _ggprqGroupName,
               "PolicyName" =: _ggprqPolicyName]

-- | Contains the response to a successful GetGroupPolicy request.
--
-- /See:/ 'getGroupPolicyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ggprsStatus'
--
-- * 'ggprsGroupName'
--
-- * 'ggprsPolicyName'
--
-- * 'ggprsPolicyDocument'
data GetGroupPolicyResponse = GetGroupPolicyResponse'
    { _ggprsStatus         :: !Int
    , _ggprsGroupName      :: !Text
    , _ggprsPolicyName     :: !Text
    , _ggprsPolicyDocument :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetGroupPolicyResponse' smart constructor.
getGroupPolicyResponse :: Int -> Text -> Text -> Text -> GetGroupPolicyResponse
getGroupPolicyResponse pStatus_ pGroupName_ pPolicyName_ pPolicyDocument_ =
    GetGroupPolicyResponse'
    { _ggprsStatus = pStatus_
    , _ggprsGroupName = pGroupName_
    , _ggprsPolicyName = pPolicyName_
    , _ggprsPolicyDocument = pPolicyDocument_
    }

-- | FIXME: Undocumented member.
ggprsStatus :: Lens' GetGroupPolicyResponse Int
ggprsStatus = lens _ggprsStatus (\ s a -> s{_ggprsStatus = a});

-- | The group the policy is associated with.
ggprsGroupName :: Lens' GetGroupPolicyResponse Text
ggprsGroupName = lens _ggprsGroupName (\ s a -> s{_ggprsGroupName = a});

-- | The name of the policy.
ggprsPolicyName :: Lens' GetGroupPolicyResponse Text
ggprsPolicyName = lens _ggprsPolicyName (\ s a -> s{_ggprsPolicyName = a});

-- | The policy document.
ggprsPolicyDocument :: Lens' GetGroupPolicyResponse Text
ggprsPolicyDocument = lens _ggprsPolicyDocument (\ s a -> s{_ggprsPolicyDocument = a});
