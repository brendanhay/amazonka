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
-- Module      : Network.AWS.SageMaker.CreateWorkteam
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new work team for labeling your data. A work team is defined by one or more Amazon Cognito user pools. You must first create the user pools before you can create a work team.
--
--
-- You cannot create more than 25 work teams in an account and region.
module Network.AWS.SageMaker.CreateWorkteam
  ( -- * Creating a Request
    createWorkteam,
    CreateWorkteam,

    -- * Request Lenses
    cwNotificationConfiguration,
    cwWorkforceName,
    cwTags,
    cwWorkteamName,
    cwMemberDefinitions,
    cwDescription,

    -- * Destructuring the Response
    createWorkteamResponse,
    CreateWorkteamResponse,

    -- * Response Lenses
    cwwrsWorkteamARN,
    cwwrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'createWorkteam' smart constructor.
data CreateWorkteam = CreateWorkteam'
  { _cwNotificationConfiguration ::
      !(Maybe NotificationConfiguration),
    _cwWorkforceName :: !(Maybe Text),
    _cwTags :: !(Maybe [Tag]),
    _cwWorkteamName :: !Text,
    _cwMemberDefinitions :: !(List1 MemberDefinition),
    _cwDescription :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateWorkteam' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwNotificationConfiguration' - Configures notification of workers regarding available or expiring work items.
--
-- * 'cwWorkforceName' - The name of the workforce.
--
-- * 'cwTags' - An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-resource-tags.html Resource Tag> and <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- * 'cwWorkteamName' - The name of the work team. Use this name to identify the work team.
--
-- * 'cwMemberDefinitions' - A list of @MemberDefinition@ objects that contains objects that identify the workers that make up the work team.  Workforces can be created using Amazon Cognito or your own OIDC Identity Provider (IdP). For private workforces created using Amazon Cognito use @CognitoMemberDefinition@ . For workforces created using your own OIDC identity provider (IdP) use @OidcMemberDefinition@ . Do not provide input for both of these parameters in a single request. For workforces created using Amazon Cognito, private work teams correspond to Amazon Cognito /user groups/ within the user pool used to create a workforce. All of the @CognitoMemberDefinition@ objects that make up the member definition must have the same @ClientId@ and @UserPool@ values. To add a Amazon Cognito user group to an existing worker pool, see < Adding groups to a User Pool> . For more information about user pools, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito User Pools> . For workforces created using your own OIDC IdP, specify the user groups that you want to include in your private work team in @OidcMemberDefinition@ by listing those groups in @Groups@ .
--
-- * 'cwDescription' - A description of the work team.
createWorkteam ::
  -- | 'cwWorkteamName'
  Text ->
  -- | 'cwMemberDefinitions'
  NonEmpty MemberDefinition ->
  -- | 'cwDescription'
  Text ->
  CreateWorkteam
createWorkteam pWorkteamName_ pMemberDefinitions_ pDescription_ =
  CreateWorkteam'
    { _cwNotificationConfiguration = Nothing,
      _cwWorkforceName = Nothing,
      _cwTags = Nothing,
      _cwWorkteamName = pWorkteamName_,
      _cwMemberDefinitions = _List1 # pMemberDefinitions_,
      _cwDescription = pDescription_
    }

-- | Configures notification of workers regarding available or expiring work items.
cwNotificationConfiguration :: Lens' CreateWorkteam (Maybe NotificationConfiguration)
cwNotificationConfiguration = lens _cwNotificationConfiguration (\s a -> s {_cwNotificationConfiguration = a})

-- | The name of the workforce.
cwWorkforceName :: Lens' CreateWorkteam (Maybe Text)
cwWorkforceName = lens _cwWorkforceName (\s a -> s {_cwWorkforceName = a})

-- | An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-resource-tags.html Resource Tag> and <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
cwTags :: Lens' CreateWorkteam [Tag]
cwTags = lens _cwTags (\s a -> s {_cwTags = a}) . _Default . _Coerce

-- | The name of the work team. Use this name to identify the work team.
cwWorkteamName :: Lens' CreateWorkteam Text
cwWorkteamName = lens _cwWorkteamName (\s a -> s {_cwWorkteamName = a})

-- | A list of @MemberDefinition@ objects that contains objects that identify the workers that make up the work team.  Workforces can be created using Amazon Cognito or your own OIDC Identity Provider (IdP). For private workforces created using Amazon Cognito use @CognitoMemberDefinition@ . For workforces created using your own OIDC identity provider (IdP) use @OidcMemberDefinition@ . Do not provide input for both of these parameters in a single request. For workforces created using Amazon Cognito, private work teams correspond to Amazon Cognito /user groups/ within the user pool used to create a workforce. All of the @CognitoMemberDefinition@ objects that make up the member definition must have the same @ClientId@ and @UserPool@ values. To add a Amazon Cognito user group to an existing worker pool, see < Adding groups to a User Pool> . For more information about user pools, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito User Pools> . For workforces created using your own OIDC IdP, specify the user groups that you want to include in your private work team in @OidcMemberDefinition@ by listing those groups in @Groups@ .
cwMemberDefinitions :: Lens' CreateWorkteam (NonEmpty MemberDefinition)
cwMemberDefinitions = lens _cwMemberDefinitions (\s a -> s {_cwMemberDefinitions = a}) . _List1

-- | A description of the work team.
cwDescription :: Lens' CreateWorkteam Text
cwDescription = lens _cwDescription (\s a -> s {_cwDescription = a})

instance AWSRequest CreateWorkteam where
  type Rs CreateWorkteam = CreateWorkteamResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          CreateWorkteamResponse'
            <$> (x .?> "WorkteamArn") <*> (pure (fromEnum s))
      )

instance Hashable CreateWorkteam

instance NFData CreateWorkteam

instance ToHeaders CreateWorkteam where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.CreateWorkteam" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateWorkteam where
  toJSON CreateWorkteam' {..} =
    object
      ( catMaybes
          [ ("NotificationConfiguration" .=) <$> _cwNotificationConfiguration,
            ("WorkforceName" .=) <$> _cwWorkforceName,
            ("Tags" .=) <$> _cwTags,
            Just ("WorkteamName" .= _cwWorkteamName),
            Just ("MemberDefinitions" .= _cwMemberDefinitions),
            Just ("Description" .= _cwDescription)
          ]
      )

instance ToPath CreateWorkteam where
  toPath = const "/"

instance ToQuery CreateWorkteam where
  toQuery = const mempty

-- | /See:/ 'createWorkteamResponse' smart constructor.
data CreateWorkteamResponse = CreateWorkteamResponse'
  { _cwwrsWorkteamARN ::
      !(Maybe Text),
    _cwwrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateWorkteamResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwwrsWorkteamARN' - The Amazon Resource Name (ARN) of the work team. You can use this ARN to identify the work team.
--
-- * 'cwwrsResponseStatus' - -- | The response status code.
createWorkteamResponse ::
  -- | 'cwwrsResponseStatus'
  Int ->
  CreateWorkteamResponse
createWorkteamResponse pResponseStatus_ =
  CreateWorkteamResponse'
    { _cwwrsWorkteamARN = Nothing,
      _cwwrsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the work team. You can use this ARN to identify the work team.
cwwrsWorkteamARN :: Lens' CreateWorkteamResponse (Maybe Text)
cwwrsWorkteamARN = lens _cwwrsWorkteamARN (\s a -> s {_cwwrsWorkteamARN = a})

-- | -- | The response status code.
cwwrsResponseStatus :: Lens' CreateWorkteamResponse Int
cwwrsResponseStatus = lens _cwwrsResponseStatus (\s a -> s {_cwwrsResponseStatus = a})

instance NFData CreateWorkteamResponse
