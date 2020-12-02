{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateWorkteam
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing work team with new member definitions or description.
module Network.AWS.SageMaker.UpdateWorkteam
  ( -- * Creating a Request
    updateWorkteam,
    UpdateWorkteam,

    -- * Request Lenses
    uwNotificationConfiguration,
    uwMemberDefinitions,
    uwDescription,
    uwWorkteamName,

    -- * Destructuring the Response
    updateWorkteamResponse,
    UpdateWorkteamResponse,

    -- * Response Lenses
    uwrsResponseStatus,
    uwrsWorkteam,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'updateWorkteam' smart constructor.
data UpdateWorkteam = UpdateWorkteam'
  { _uwNotificationConfiguration ::
      !(Maybe NotificationConfiguration),
    _uwMemberDefinitions :: !(Maybe (List1 MemberDefinition)),
    _uwDescription :: !(Maybe Text),
    _uwWorkteamName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateWorkteam' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uwNotificationConfiguration' - Configures SNS topic notifications for available or expiring work items
--
-- * 'uwMemberDefinitions' - A list of @MemberDefinition@ objects that contains objects that identify the workers that make up the work team.  Workforces can be created using Amazon Cognito or your own OIDC Identity Provider (IdP). For private workforces created using Amazon Cognito use @CognitoMemberDefinition@ . For workforces created using your own OIDC identity provider (IdP) use @OidcMemberDefinition@ . You should not provide input for both of these parameters in a single request. For workforces created using Amazon Cognito, private work teams correspond to Amazon Cognito /user groups/ within the user pool used to create a workforce. All of the @CognitoMemberDefinition@ objects that make up the member definition must have the same @ClientId@ and @UserPool@ values. To add a Amazon Cognito user group to an existing worker pool, see < Adding groups to a User Pool> . For more information about user pools, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito User Pools> . For workforces created using your own OIDC IdP, specify the user groups that you want to include in your private work team in @OidcMemberDefinition@ by listing those groups in @Groups@ . Be aware that user groups that are already in the work team must also be listed in @Groups@ when you make this request to remain on the work team. If you do not include these user groups, they will no longer be associated with the work team you update.
--
-- * 'uwDescription' - An updated description for the work team.
--
-- * 'uwWorkteamName' - The name of the work team to update.
updateWorkteam ::
  -- | 'uwWorkteamName'
  Text ->
  UpdateWorkteam
updateWorkteam pWorkteamName_ =
  UpdateWorkteam'
    { _uwNotificationConfiguration = Nothing,
      _uwMemberDefinitions = Nothing,
      _uwDescription = Nothing,
      _uwWorkteamName = pWorkteamName_
    }

-- | Configures SNS topic notifications for available or expiring work items
uwNotificationConfiguration :: Lens' UpdateWorkteam (Maybe NotificationConfiguration)
uwNotificationConfiguration = lens _uwNotificationConfiguration (\s a -> s {_uwNotificationConfiguration = a})

-- | A list of @MemberDefinition@ objects that contains objects that identify the workers that make up the work team.  Workforces can be created using Amazon Cognito or your own OIDC Identity Provider (IdP). For private workforces created using Amazon Cognito use @CognitoMemberDefinition@ . For workforces created using your own OIDC identity provider (IdP) use @OidcMemberDefinition@ . You should not provide input for both of these parameters in a single request. For workforces created using Amazon Cognito, private work teams correspond to Amazon Cognito /user groups/ within the user pool used to create a workforce. All of the @CognitoMemberDefinition@ objects that make up the member definition must have the same @ClientId@ and @UserPool@ values. To add a Amazon Cognito user group to an existing worker pool, see < Adding groups to a User Pool> . For more information about user pools, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito User Pools> . For workforces created using your own OIDC IdP, specify the user groups that you want to include in your private work team in @OidcMemberDefinition@ by listing those groups in @Groups@ . Be aware that user groups that are already in the work team must also be listed in @Groups@ when you make this request to remain on the work team. If you do not include these user groups, they will no longer be associated with the work team you update.
uwMemberDefinitions :: Lens' UpdateWorkteam (Maybe (NonEmpty MemberDefinition))
uwMemberDefinitions = lens _uwMemberDefinitions (\s a -> s {_uwMemberDefinitions = a}) . mapping _List1

-- | An updated description for the work team.
uwDescription :: Lens' UpdateWorkteam (Maybe Text)
uwDescription = lens _uwDescription (\s a -> s {_uwDescription = a})

-- | The name of the work team to update.
uwWorkteamName :: Lens' UpdateWorkteam Text
uwWorkteamName = lens _uwWorkteamName (\s a -> s {_uwWorkteamName = a})

instance AWSRequest UpdateWorkteam where
  type Rs UpdateWorkteam = UpdateWorkteamResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          UpdateWorkteamResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "Workteam")
      )

instance Hashable UpdateWorkteam

instance NFData UpdateWorkteam

instance ToHeaders UpdateWorkteam where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.UpdateWorkteam" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateWorkteam where
  toJSON UpdateWorkteam' {..} =
    object
      ( catMaybes
          [ ("NotificationConfiguration" .=) <$> _uwNotificationConfiguration,
            ("MemberDefinitions" .=) <$> _uwMemberDefinitions,
            ("Description" .=) <$> _uwDescription,
            Just ("WorkteamName" .= _uwWorkteamName)
          ]
      )

instance ToPath UpdateWorkteam where
  toPath = const "/"

instance ToQuery UpdateWorkteam where
  toQuery = const mempty

-- | /See:/ 'updateWorkteamResponse' smart constructor.
data UpdateWorkteamResponse = UpdateWorkteamResponse'
  { _uwrsResponseStatus ::
      !Int,
    _uwrsWorkteam :: !Workteam
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateWorkteamResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uwrsResponseStatus' - -- | The response status code.
--
-- * 'uwrsWorkteam' - A @Workteam@ object that describes the updated work team.
updateWorkteamResponse ::
  -- | 'uwrsResponseStatus'
  Int ->
  -- | 'uwrsWorkteam'
  Workteam ->
  UpdateWorkteamResponse
updateWorkteamResponse pResponseStatus_ pWorkteam_ =
  UpdateWorkteamResponse'
    { _uwrsResponseStatus = pResponseStatus_,
      _uwrsWorkteam = pWorkteam_
    }

-- | -- | The response status code.
uwrsResponseStatus :: Lens' UpdateWorkteamResponse Int
uwrsResponseStatus = lens _uwrsResponseStatus (\s a -> s {_uwrsResponseStatus = a})

-- | A @Workteam@ object that describes the updated work team.
uwrsWorkteam :: Lens' UpdateWorkteamResponse Workteam
uwrsWorkteam = lens _uwrsWorkteam (\s a -> s {_uwrsWorkteam = a})

instance NFData UpdateWorkteamResponse
