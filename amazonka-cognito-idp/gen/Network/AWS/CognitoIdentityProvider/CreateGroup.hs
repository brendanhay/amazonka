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
-- Module      : Network.AWS.CognitoIdentityProvider.CreateGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new group in the specified user pool.
--
--
-- Requires developer credentials.
--
module Network.AWS.CognitoIdentityProvider.CreateGroup
    (
    -- * Creating a Request
      createGroup
    , CreateGroup
    -- * Request Lenses
    , cgPrecedence
    , cgDescription
    , cgRoleARN
    , cgGroupName
    , cgUserPoolId

    -- * Destructuring the Response
    , createGroupResponse
    , CreateGroupResponse
    -- * Response Lenses
    , cgrsGroup
    , cgrsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createGroup' smart constructor.
data CreateGroup = CreateGroup'
  { _cgPrecedence  :: !(Maybe Nat)
  , _cgDescription :: !(Maybe Text)
  , _cgRoleARN     :: !(Maybe Text)
  , _cgGroupName   :: !Text
  , _cgUserPoolId  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgPrecedence' - A nonnegative integer value that specifies the precedence of this group relative to the other groups that a user can belong to in the user pool. Zero is the highest precedence value. Groups with lower @Precedence@ values take precedence over groups with higher or null @Precedence@ values. If a user belongs to two or more groups, it is the group with the lowest precedence value whose role ARN will be used in the @cognito:roles@ and @cognito:preferred_role@ claims in the user's tokens. Two groups can have the same @Precedence@ value. If this happens, neither group takes precedence over the other. If two groups with the same @Precedence@ have the same role ARN, that role is used in the @cognito:preferred_role@ claim in tokens for users in each group. If the two groups have different role ARNs, the @cognito:preferred_role@ claim is not set in users' tokens. The default @Precedence@ value is null.
--
-- * 'cgDescription' - A string containing the description of the group.
--
-- * 'cgRoleARN' - The role ARN for the group.
--
-- * 'cgGroupName' - The name of the group. Must be unique.
--
-- * 'cgUserPoolId' - The user pool ID for the user pool.
createGroup
    :: Text -- ^ 'cgGroupName'
    -> Text -- ^ 'cgUserPoolId'
    -> CreateGroup
createGroup pGroupName_ pUserPoolId_ =
  CreateGroup'
    { _cgPrecedence = Nothing
    , _cgDescription = Nothing
    , _cgRoleARN = Nothing
    , _cgGroupName = pGroupName_
    , _cgUserPoolId = pUserPoolId_
    }


-- | A nonnegative integer value that specifies the precedence of this group relative to the other groups that a user can belong to in the user pool. Zero is the highest precedence value. Groups with lower @Precedence@ values take precedence over groups with higher or null @Precedence@ values. If a user belongs to two or more groups, it is the group with the lowest precedence value whose role ARN will be used in the @cognito:roles@ and @cognito:preferred_role@ claims in the user's tokens. Two groups can have the same @Precedence@ value. If this happens, neither group takes precedence over the other. If two groups with the same @Precedence@ have the same role ARN, that role is used in the @cognito:preferred_role@ claim in tokens for users in each group. If the two groups have different role ARNs, the @cognito:preferred_role@ claim is not set in users' tokens. The default @Precedence@ value is null.
cgPrecedence :: Lens' CreateGroup (Maybe Natural)
cgPrecedence = lens _cgPrecedence (\ s a -> s{_cgPrecedence = a}) . mapping _Nat

-- | A string containing the description of the group.
cgDescription :: Lens' CreateGroup (Maybe Text)
cgDescription = lens _cgDescription (\ s a -> s{_cgDescription = a})

-- | The role ARN for the group.
cgRoleARN :: Lens' CreateGroup (Maybe Text)
cgRoleARN = lens _cgRoleARN (\ s a -> s{_cgRoleARN = a})

-- | The name of the group. Must be unique.
cgGroupName :: Lens' CreateGroup Text
cgGroupName = lens _cgGroupName (\ s a -> s{_cgGroupName = a})

-- | The user pool ID for the user pool.
cgUserPoolId :: Lens' CreateGroup Text
cgUserPoolId = lens _cgUserPoolId (\ s a -> s{_cgUserPoolId = a})

instance AWSRequest CreateGroup where
        type Rs CreateGroup = CreateGroupResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 CreateGroupResponse' <$>
                   (x .?> "Group") <*> (pure (fromEnum s)))

instance Hashable CreateGroup where

instance NFData CreateGroup where

instance ToHeaders CreateGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.CreateGroup" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateGroup where
        toJSON CreateGroup'{..}
          = object
              (catMaybes
                 [("Precedence" .=) <$> _cgPrecedence,
                  ("Description" .=) <$> _cgDescription,
                  ("RoleArn" .=) <$> _cgRoleARN,
                  Just ("GroupName" .= _cgGroupName),
                  Just ("UserPoolId" .= _cgUserPoolId)])

instance ToPath CreateGroup where
        toPath = const "/"

instance ToQuery CreateGroup where
        toQuery = const mempty

-- | /See:/ 'createGroupResponse' smart constructor.
data CreateGroupResponse = CreateGroupResponse'
  { _cgrsGroup          :: !(Maybe GroupType)
  , _cgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgrsGroup' - The group object for the group.
--
-- * 'cgrsResponseStatus' - -- | The response status code.
createGroupResponse
    :: Int -- ^ 'cgrsResponseStatus'
    -> CreateGroupResponse
createGroupResponse pResponseStatus_ =
  CreateGroupResponse'
    {_cgrsGroup = Nothing, _cgrsResponseStatus = pResponseStatus_}


-- | The group object for the group.
cgrsGroup :: Lens' CreateGroupResponse (Maybe GroupType)
cgrsGroup = lens _cgrsGroup (\ s a -> s{_cgrsGroup = a})

-- | -- | The response status code.
cgrsResponseStatus :: Lens' CreateGroupResponse Int
cgrsResponseStatus = lens _cgrsResponseStatus (\ s a -> s{_cgrsResponseStatus = a})

instance NFData CreateGroupResponse where
