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
-- Module      : Network.AWS.IAM.GetGroupPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetGroupPolicy.html AWS API Reference> for GetGroupPolicy.
module Network.AWS.IAM.GetGroupPolicy
    (
    -- * Creating a Request
      getGroupPolicy
    , GetGroupPolicy
    -- * Request Lenses
    , ggpGroupName
    , ggpPolicyName

    -- * Destructuring the Response
    , getGroupPolicyResponse
    , GetGroupPolicyResponse
    -- * Response Lenses
    , ggprsResponseStatus
    , ggprsGroupName
    , ggprsPolicyName
    , ggprsPolicyDocument
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getGroupPolicy' smart constructor.
data GetGroupPolicy = GetGroupPolicy'
    { _ggpGroupName  :: !Text
    , _ggpPolicyName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetGroupPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggpGroupName'
--
-- * 'ggpPolicyName'
getGroupPolicy
    :: Text -- ^ 'ggpGroupName'
    -> Text -- ^ 'ggpPolicyName'
    -> GetGroupPolicy
getGroupPolicy pGroupName_ pPolicyName_ =
    GetGroupPolicy'
    { _ggpGroupName = pGroupName_
    , _ggpPolicyName = pPolicyName_
    }

-- | The name of the group the policy is associated with.
ggpGroupName :: Lens' GetGroupPolicy Text
ggpGroupName = lens _ggpGroupName (\ s a -> s{_ggpGroupName = a});

-- | The name of the policy document to get.
ggpPolicyName :: Lens' GetGroupPolicy Text
ggpPolicyName = lens _ggpPolicyName (\ s a -> s{_ggpPolicyName = a});

instance AWSRequest GetGroupPolicy where
        type Rs GetGroupPolicy = GetGroupPolicyResponse
        request = postQuery iAM
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
data GetGroupPolicyResponse = GetGroupPolicyResponse'
    { _ggprsResponseStatus :: !Int
    , _ggprsGroupName      :: !Text
    , _ggprsPolicyName     :: !Text
    , _ggprsPolicyDocument :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetGroupPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggprsResponseStatus'
--
-- * 'ggprsGroupName'
--
-- * 'ggprsPolicyName'
--
-- * 'ggprsPolicyDocument'
getGroupPolicyResponse
    :: Int -- ^ 'ggprsResponseStatus'
    -> Text -- ^ 'ggprsGroupName'
    -> Text -- ^ 'ggprsPolicyName'
    -> Text -- ^ 'ggprsPolicyDocument'
    -> GetGroupPolicyResponse
getGroupPolicyResponse pResponseStatus_ pGroupName_ pPolicyName_ pPolicyDocument_ =
    GetGroupPolicyResponse'
    { _ggprsResponseStatus = pResponseStatus_
    , _ggprsGroupName = pGroupName_
    , _ggprsPolicyName = pPolicyName_
    , _ggprsPolicyDocument = pPolicyDocument_
    }

-- | The response status code.
ggprsResponseStatus :: Lens' GetGroupPolicyResponse Int
ggprsResponseStatus = lens _ggprsResponseStatus (\ s a -> s{_ggprsResponseStatus = a});

-- | The group the policy is associated with.
ggprsGroupName :: Lens' GetGroupPolicyResponse Text
ggprsGroupName = lens _ggprsGroupName (\ s a -> s{_ggprsGroupName = a});

-- | The name of the policy.
ggprsPolicyName :: Lens' GetGroupPolicyResponse Text
ggprsPolicyName = lens _ggprsPolicyName (\ s a -> s{_ggprsPolicyName = a});

-- | The policy document.
ggprsPolicyDocument :: Lens' GetGroupPolicyResponse Text
ggprsPolicyDocument = lens _ggprsPolicyDocument (\ s a -> s{_ggprsPolicyDocument = a});
