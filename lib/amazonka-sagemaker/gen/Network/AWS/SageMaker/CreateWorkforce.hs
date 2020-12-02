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
-- Module      : Network.AWS.SageMaker.CreateWorkforce
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to create a workforce. This operation will return an error if a workforce already exists in the AWS Region that you specify. You can only create one workforce in each AWS Region per AWS account.
--
--
-- If you want to create a new workforce in an AWS Region where a workforce already exists, use the API operation to delete the existing workforce and then use @CreateWorkforce@ to create a new workforce.
--
-- To create a private workforce using Amazon Cognito, you must specify a Cognito user pool in @CognitoConfig@ . You can also create an Amazon Cognito workforce using the Amazon SageMaker console. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private.html Create a Private Workforce (Amazon Cognito)> .
--
-- To create a private workforce using your own OIDC Identity Provider (IdP), specify your IdP configuration in @OidcConfig@ . Your OIDC IdP must support /groups/ because groups are used by Ground Truth and Amazon A2I to create work teams. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private-oidc.html Create a Private Workforce (OIDC IdP)> .
module Network.AWS.SageMaker.CreateWorkforce
  ( -- * Creating a Request
    createWorkforce,
    CreateWorkforce,

    -- * Request Lenses
    creSourceIPConfig,
    creCognitoConfig,
    creOidcConfig,
    creTags,
    creWorkforceName,

    -- * Destructuring the Response
    createWorkforceResponse,
    CreateWorkforceResponse,

    -- * Response Lenses
    cwrsResponseStatus,
    cwrsWorkforceARN,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'createWorkforce' smart constructor.
data CreateWorkforce = CreateWorkforce'
  { _creSourceIPConfig ::
      !(Maybe SourceIPConfig),
    _creCognitoConfig :: !(Maybe CognitoConfig),
    _creOidcConfig :: !(Maybe OidcConfig),
    _creTags :: !(Maybe [Tag]),
    _creWorkforceName :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateWorkforce' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'creSourceIPConfig' - Undocumented member.
--
-- * 'creCognitoConfig' - Use this parameter to configure an Amazon Cognito private workforce. A single Cognito workforce is created using and corresponds to a single <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito user pool> . Do not use @OidcConfig@ if you specify values for @CognitoConfig@ .
--
-- * 'creOidcConfig' - Use this parameter to configure a private workforce using your own OIDC Identity Provider. Do not use @CognitoConfig@ if you specify values for @OidcConfig@ .
--
-- * 'creTags' - An array of key-value pairs that contain metadata to help you categorize and organize our workforce. Each tag consists of a key and a value, both of which you define.
--
-- * 'creWorkforceName' - The name of the private workforce.
createWorkforce ::
  -- | 'creWorkforceName'
  Text ->
  CreateWorkforce
createWorkforce pWorkforceName_ =
  CreateWorkforce'
    { _creSourceIPConfig = Nothing,
      _creCognitoConfig = Nothing,
      _creOidcConfig = Nothing,
      _creTags = Nothing,
      _creWorkforceName = pWorkforceName_
    }

-- | Undocumented member.
creSourceIPConfig :: Lens' CreateWorkforce (Maybe SourceIPConfig)
creSourceIPConfig = lens _creSourceIPConfig (\s a -> s {_creSourceIPConfig = a})

-- | Use this parameter to configure an Amazon Cognito private workforce. A single Cognito workforce is created using and corresponds to a single <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito user pool> . Do not use @OidcConfig@ if you specify values for @CognitoConfig@ .
creCognitoConfig :: Lens' CreateWorkforce (Maybe CognitoConfig)
creCognitoConfig = lens _creCognitoConfig (\s a -> s {_creCognitoConfig = a})

-- | Use this parameter to configure a private workforce using your own OIDC Identity Provider. Do not use @CognitoConfig@ if you specify values for @OidcConfig@ .
creOidcConfig :: Lens' CreateWorkforce (Maybe OidcConfig)
creOidcConfig = lens _creOidcConfig (\s a -> s {_creOidcConfig = a})

-- | An array of key-value pairs that contain metadata to help you categorize and organize our workforce. Each tag consists of a key and a value, both of which you define.
creTags :: Lens' CreateWorkforce [Tag]
creTags = lens _creTags (\s a -> s {_creTags = a}) . _Default . _Coerce

-- | The name of the private workforce.
creWorkforceName :: Lens' CreateWorkforce Text
creWorkforceName = lens _creWorkforceName (\s a -> s {_creWorkforceName = a})

instance AWSRequest CreateWorkforce where
  type Rs CreateWorkforce = CreateWorkforceResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          CreateWorkforceResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "WorkforceArn")
      )

instance Hashable CreateWorkforce

instance NFData CreateWorkforce

instance ToHeaders CreateWorkforce where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.CreateWorkforce" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateWorkforce where
  toJSON CreateWorkforce' {..} =
    object
      ( catMaybes
          [ ("SourceIpConfig" .=) <$> _creSourceIPConfig,
            ("CognitoConfig" .=) <$> _creCognitoConfig,
            ("OidcConfig" .=) <$> _creOidcConfig,
            ("Tags" .=) <$> _creTags,
            Just ("WorkforceName" .= _creWorkforceName)
          ]
      )

instance ToPath CreateWorkforce where
  toPath = const "/"

instance ToQuery CreateWorkforce where
  toQuery = const mempty

-- | /See:/ 'createWorkforceResponse' smart constructor.
data CreateWorkforceResponse = CreateWorkforceResponse'
  { _cwrsResponseStatus ::
      !Int,
    _cwrsWorkforceARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateWorkforceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwrsResponseStatus' - -- | The response status code.
--
-- * 'cwrsWorkforceARN' - The Amazon Resource Name (ARN) of the workforce.
createWorkforceResponse ::
  -- | 'cwrsResponseStatus'
  Int ->
  -- | 'cwrsWorkforceARN'
  Text ->
  CreateWorkforceResponse
createWorkforceResponse pResponseStatus_ pWorkforceARN_ =
  CreateWorkforceResponse'
    { _cwrsResponseStatus = pResponseStatus_,
      _cwrsWorkforceARN = pWorkforceARN_
    }

-- | -- | The response status code.
cwrsResponseStatus :: Lens' CreateWorkforceResponse Int
cwrsResponseStatus = lens _cwrsResponseStatus (\s a -> s {_cwrsResponseStatus = a})

-- | The Amazon Resource Name (ARN) of the workforce.
cwrsWorkforceARN :: Lens' CreateWorkforceResponse Text
cwrsWorkforceARN = lens _cwrsWorkforceARN (\s a -> s {_cwrsWorkforceARN = a})

instance NFData CreateWorkforceResponse
