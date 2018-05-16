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
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified group with the specified attributes.
--
--
-- Requires developer credentials.
--
module Network.AWS.CognitoIdentityProvider.UpdateGroup
    (
    -- * Creating a Request
      updateGroup
    , UpdateGroup
    -- * Request Lenses
    , ugPrecedence
    , ugDescription
    , ugRoleARN
    , ugGroupName
    , ugUserPoolId

    -- * Destructuring the Response
    , updateGroupResponse
    , UpdateGroupResponse
    -- * Response Lenses
    , ugrsGroup
    , ugrsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateGroup' smart constructor.
data UpdateGroup = UpdateGroup'
  { _ugPrecedence  :: !(Maybe Nat)
  , _ugDescription :: !(Maybe Text)
  , _ugRoleARN     :: !(Maybe Text)
  , _ugGroupName   :: !Text
  , _ugUserPoolId  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugPrecedence' - The new precedence value for the group. For more information about this parameter, see .
--
-- * 'ugDescription' - A string containing the new description of the group.
--
-- * 'ugRoleARN' - The new role ARN for the group. This is used for setting the @cognito:roles@ and @cognito:preferred_role@ claims in the token.
--
-- * 'ugGroupName' - The name of the group.
--
-- * 'ugUserPoolId' - The user pool ID for the user pool.
updateGroup
    :: Text -- ^ 'ugGroupName'
    -> Text -- ^ 'ugUserPoolId'
    -> UpdateGroup
updateGroup pGroupName_ pUserPoolId_ =
  UpdateGroup'
    { _ugPrecedence = Nothing
    , _ugDescription = Nothing
    , _ugRoleARN = Nothing
    , _ugGroupName = pGroupName_
    , _ugUserPoolId = pUserPoolId_
    }


-- | The new precedence value for the group. For more information about this parameter, see .
ugPrecedence :: Lens' UpdateGroup (Maybe Natural)
ugPrecedence = lens _ugPrecedence (\ s a -> s{_ugPrecedence = a}) . mapping _Nat

-- | A string containing the new description of the group.
ugDescription :: Lens' UpdateGroup (Maybe Text)
ugDescription = lens _ugDescription (\ s a -> s{_ugDescription = a})

-- | The new role ARN for the group. This is used for setting the @cognito:roles@ and @cognito:preferred_role@ claims in the token.
ugRoleARN :: Lens' UpdateGroup (Maybe Text)
ugRoleARN = lens _ugRoleARN (\ s a -> s{_ugRoleARN = a})

-- | The name of the group.
ugGroupName :: Lens' UpdateGroup Text
ugGroupName = lens _ugGroupName (\ s a -> s{_ugGroupName = a})

-- | The user pool ID for the user pool.
ugUserPoolId :: Lens' UpdateGroup Text
ugUserPoolId = lens _ugUserPoolId (\ s a -> s{_ugUserPoolId = a})

instance AWSRequest UpdateGroup where
        type Rs UpdateGroup = UpdateGroupResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 UpdateGroupResponse' <$>
                   (x .?> "Group") <*> (pure (fromEnum s)))

instance Hashable UpdateGroup where

instance NFData UpdateGroup where

instance ToHeaders UpdateGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.UpdateGroup" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateGroup where
        toJSON UpdateGroup'{..}
          = object
              (catMaybes
                 [("Precedence" .=) <$> _ugPrecedence,
                  ("Description" .=) <$> _ugDescription,
                  ("RoleArn" .=) <$> _ugRoleARN,
                  Just ("GroupName" .= _ugGroupName),
                  Just ("UserPoolId" .= _ugUserPoolId)])

instance ToPath UpdateGroup where
        toPath = const "/"

instance ToQuery UpdateGroup where
        toQuery = const mempty

-- | /See:/ 'updateGroupResponse' smart constructor.
data UpdateGroupResponse = UpdateGroupResponse'
  { _ugrsGroup          :: !(Maybe GroupType)
  , _ugrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugrsGroup' - The group object for the group.
--
-- * 'ugrsResponseStatus' - -- | The response status code.
updateGroupResponse
    :: Int -- ^ 'ugrsResponseStatus'
    -> UpdateGroupResponse
updateGroupResponse pResponseStatus_ =
  UpdateGroupResponse'
    {_ugrsGroup = Nothing, _ugrsResponseStatus = pResponseStatus_}


-- | The group object for the group.
ugrsGroup :: Lens' UpdateGroupResponse (Maybe GroupType)
ugrsGroup = lens _ugrsGroup (\ s a -> s{_ugrsGroup = a})

-- | -- | The response status code.
ugrsResponseStatus :: Lens' UpdateGroupResponse Int
ugrsResponseStatus = lens _ugrsResponseStatus (\ s a -> s{_ugrsResponseStatus = a})

instance NFData UpdateGroupResponse where
