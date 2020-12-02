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
-- Module      : Network.AWS.IoT.CreateAuthorizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an authorizer.
module Network.AWS.IoT.CreateAuthorizer
  ( -- * Creating a Request
    createAuthorizer,
    CreateAuthorizer,

    -- * Request Lenses
    caStatus,
    caSigningDisabled,
    caTokenSigningPublicKeys,
    caTokenKeyName,
    caTags,
    caAuthorizerName,
    caAuthorizerFunctionARN,

    -- * Destructuring the Response
    createAuthorizerResponse,
    CreateAuthorizerResponse,

    -- * Response Lenses
    carsAuthorizerName,
    carsAuthorizerARN,
    carsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createAuthorizer' smart constructor.
data CreateAuthorizer = CreateAuthorizer'
  { _caStatus ::
      !(Maybe AuthorizerStatus),
    _caSigningDisabled :: !(Maybe Bool),
    _caTokenSigningPublicKeys :: !(Maybe (Map Text (Text))),
    _caTokenKeyName :: !(Maybe Text),
    _caTags :: !(Maybe [Tag]),
    _caAuthorizerName :: !Text,
    _caAuthorizerFunctionARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateAuthorizer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caStatus' - The status of the create authorizer request.
--
-- * 'caSigningDisabled' - Specifies whether AWS IoT validates the token signature in an authorization request.
--
-- * 'caTokenSigningPublicKeys' - The public keys used to verify the digital signature returned by your custom authentication service.
--
-- * 'caTokenKeyName' - The name of the token key used to extract the token from the HTTP headers.
--
-- * 'caTags' - Metadata which can be used to manage the custom authorizer.
--
-- * 'caAuthorizerName' - The authorizer name.
--
-- * 'caAuthorizerFunctionARN' - The ARN of the authorizer's Lambda function.
createAuthorizer ::
  -- | 'caAuthorizerName'
  Text ->
  -- | 'caAuthorizerFunctionARN'
  Text ->
  CreateAuthorizer
createAuthorizer pAuthorizerName_ pAuthorizerFunctionARN_ =
  CreateAuthorizer'
    { _caStatus = Nothing,
      _caSigningDisabled = Nothing,
      _caTokenSigningPublicKeys = Nothing,
      _caTokenKeyName = Nothing,
      _caTags = Nothing,
      _caAuthorizerName = pAuthorizerName_,
      _caAuthorizerFunctionARN = pAuthorizerFunctionARN_
    }

-- | The status of the create authorizer request.
caStatus :: Lens' CreateAuthorizer (Maybe AuthorizerStatus)
caStatus = lens _caStatus (\s a -> s {_caStatus = a})

-- | Specifies whether AWS IoT validates the token signature in an authorization request.
caSigningDisabled :: Lens' CreateAuthorizer (Maybe Bool)
caSigningDisabled = lens _caSigningDisabled (\s a -> s {_caSigningDisabled = a})

-- | The public keys used to verify the digital signature returned by your custom authentication service.
caTokenSigningPublicKeys :: Lens' CreateAuthorizer (HashMap Text (Text))
caTokenSigningPublicKeys = lens _caTokenSigningPublicKeys (\s a -> s {_caTokenSigningPublicKeys = a}) . _Default . _Map

-- | The name of the token key used to extract the token from the HTTP headers.
caTokenKeyName :: Lens' CreateAuthorizer (Maybe Text)
caTokenKeyName = lens _caTokenKeyName (\s a -> s {_caTokenKeyName = a})

-- | Metadata which can be used to manage the custom authorizer.
caTags :: Lens' CreateAuthorizer [Tag]
caTags = lens _caTags (\s a -> s {_caTags = a}) . _Default . _Coerce

-- | The authorizer name.
caAuthorizerName :: Lens' CreateAuthorizer Text
caAuthorizerName = lens _caAuthorizerName (\s a -> s {_caAuthorizerName = a})

-- | The ARN of the authorizer's Lambda function.
caAuthorizerFunctionARN :: Lens' CreateAuthorizer Text
caAuthorizerFunctionARN = lens _caAuthorizerFunctionARN (\s a -> s {_caAuthorizerFunctionARN = a})

instance AWSRequest CreateAuthorizer where
  type Rs CreateAuthorizer = CreateAuthorizerResponse
  request = postJSON ioT
  response =
    receiveJSON
      ( \s h x ->
          CreateAuthorizerResponse'
            <$> (x .?> "authorizerName")
            <*> (x .?> "authorizerArn")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateAuthorizer

instance NFData CreateAuthorizer

instance ToHeaders CreateAuthorizer where
  toHeaders = const mempty

instance ToJSON CreateAuthorizer where
  toJSON CreateAuthorizer' {..} =
    object
      ( catMaybes
          [ ("status" .=) <$> _caStatus,
            ("signingDisabled" .=) <$> _caSigningDisabled,
            ("tokenSigningPublicKeys" .=) <$> _caTokenSigningPublicKeys,
            ("tokenKeyName" .=) <$> _caTokenKeyName,
            ("tags" .=) <$> _caTags,
            Just ("authorizerFunctionArn" .= _caAuthorizerFunctionARN)
          ]
      )

instance ToPath CreateAuthorizer where
  toPath CreateAuthorizer' {..} =
    mconcat ["/authorizer/", toBS _caAuthorizerName]

instance ToQuery CreateAuthorizer where
  toQuery = const mempty

-- | /See:/ 'createAuthorizerResponse' smart constructor.
data CreateAuthorizerResponse = CreateAuthorizerResponse'
  { _carsAuthorizerName ::
      !(Maybe Text),
    _carsAuthorizerARN :: !(Maybe Text),
    _carsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateAuthorizerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'carsAuthorizerName' - The authorizer's name.
--
-- * 'carsAuthorizerARN' - The authorizer ARN.
--
-- * 'carsResponseStatus' - -- | The response status code.
createAuthorizerResponse ::
  -- | 'carsResponseStatus'
  Int ->
  CreateAuthorizerResponse
createAuthorizerResponse pResponseStatus_ =
  CreateAuthorizerResponse'
    { _carsAuthorizerName = Nothing,
      _carsAuthorizerARN = Nothing,
      _carsResponseStatus = pResponseStatus_
    }

-- | The authorizer's name.
carsAuthorizerName :: Lens' CreateAuthorizerResponse (Maybe Text)
carsAuthorizerName = lens _carsAuthorizerName (\s a -> s {_carsAuthorizerName = a})

-- | The authorizer ARN.
carsAuthorizerARN :: Lens' CreateAuthorizerResponse (Maybe Text)
carsAuthorizerARN = lens _carsAuthorizerARN (\s a -> s {_carsAuthorizerARN = a})

-- | -- | The response status code.
carsResponseStatus :: Lens' CreateAuthorizerResponse Int
carsResponseStatus = lens _carsResponseStatus (\s a -> s {_carsResponseStatus = a})

instance NFData CreateAuthorizerResponse
