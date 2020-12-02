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
-- Module      : Network.AWS.CodeBuild.ImportSourceCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports the source repository credentials for an AWS CodeBuild project that has its source code stored in a GitHub, GitHub Enterprise, or Bitbucket repository.
module Network.AWS.CodeBuild.ImportSourceCredentials
  ( -- * Creating a Request
    importSourceCredentials,
    ImportSourceCredentials,

    -- * Request Lenses
    iscUsername,
    iscShouldOverwrite,
    iscToken,
    iscServerType,
    iscAuthType,

    -- * Destructuring the Response
    importSourceCredentialsResponse,
    ImportSourceCredentialsResponse,

    -- * Response Lenses
    iscrsArn,
    iscrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'importSourceCredentials' smart constructor.
data ImportSourceCredentials = ImportSourceCredentials'
  { _iscUsername ::
      !(Maybe Text),
    _iscShouldOverwrite :: !(Maybe Bool),
    _iscToken :: !(Sensitive Text),
    _iscServerType :: !ServerType,
    _iscAuthType :: !AuthType
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImportSourceCredentials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iscUsername' - The Bitbucket username when the @authType@ is BASIC_AUTH. This parameter is not valid for other types of source providers or connections.
--
-- * 'iscShouldOverwrite' - Set to @false@ to prevent overwriting the repository source credentials. Set to @true@ to overwrite the repository source credentials. The default value is @true@ .
--
-- * 'iscToken' - For GitHub or GitHub Enterprise, this is the personal access token. For Bitbucket, this is the app password.
--
-- * 'iscServerType' - The source provider used for this project.
--
-- * 'iscAuthType' - The type of authentication used to connect to a GitHub, GitHub Enterprise, or Bitbucket repository. An OAUTH connection is not supported by the API and must be created using the AWS CodeBuild console.
importSourceCredentials ::
  -- | 'iscToken'
  Text ->
  -- | 'iscServerType'
  ServerType ->
  -- | 'iscAuthType'
  AuthType ->
  ImportSourceCredentials
importSourceCredentials pToken_ pServerType_ pAuthType_ =
  ImportSourceCredentials'
    { _iscUsername = Nothing,
      _iscShouldOverwrite = Nothing,
      _iscToken = _Sensitive # pToken_,
      _iscServerType = pServerType_,
      _iscAuthType = pAuthType_
    }

-- | The Bitbucket username when the @authType@ is BASIC_AUTH. This parameter is not valid for other types of source providers or connections.
iscUsername :: Lens' ImportSourceCredentials (Maybe Text)
iscUsername = lens _iscUsername (\s a -> s {_iscUsername = a})

-- | Set to @false@ to prevent overwriting the repository source credentials. Set to @true@ to overwrite the repository source credentials. The default value is @true@ .
iscShouldOverwrite :: Lens' ImportSourceCredentials (Maybe Bool)
iscShouldOverwrite = lens _iscShouldOverwrite (\s a -> s {_iscShouldOverwrite = a})

-- | For GitHub or GitHub Enterprise, this is the personal access token. For Bitbucket, this is the app password.
iscToken :: Lens' ImportSourceCredentials Text
iscToken = lens _iscToken (\s a -> s {_iscToken = a}) . _Sensitive

-- | The source provider used for this project.
iscServerType :: Lens' ImportSourceCredentials ServerType
iscServerType = lens _iscServerType (\s a -> s {_iscServerType = a})

-- | The type of authentication used to connect to a GitHub, GitHub Enterprise, or Bitbucket repository. An OAUTH connection is not supported by the API and must be created using the AWS CodeBuild console.
iscAuthType :: Lens' ImportSourceCredentials AuthType
iscAuthType = lens _iscAuthType (\s a -> s {_iscAuthType = a})

instance AWSRequest ImportSourceCredentials where
  type Rs ImportSourceCredentials = ImportSourceCredentialsResponse
  request = postJSON codeBuild
  response =
    receiveJSON
      ( \s h x ->
          ImportSourceCredentialsResponse'
            <$> (x .?> "arn") <*> (pure (fromEnum s))
      )

instance Hashable ImportSourceCredentials

instance NFData ImportSourceCredentials

instance ToHeaders ImportSourceCredentials where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeBuild_20161006.ImportSourceCredentials" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ImportSourceCredentials where
  toJSON ImportSourceCredentials' {..} =
    object
      ( catMaybes
          [ ("username" .=) <$> _iscUsername,
            ("shouldOverwrite" .=) <$> _iscShouldOverwrite,
            Just ("token" .= _iscToken),
            Just ("serverType" .= _iscServerType),
            Just ("authType" .= _iscAuthType)
          ]
      )

instance ToPath ImportSourceCredentials where
  toPath = const "/"

instance ToQuery ImportSourceCredentials where
  toQuery = const mempty

-- | /See:/ 'importSourceCredentialsResponse' smart constructor.
data ImportSourceCredentialsResponse = ImportSourceCredentialsResponse'
  { _iscrsArn ::
      !(Maybe Text),
    _iscrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImportSourceCredentialsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iscrsArn' - The Amazon Resource Name (ARN) of the token.
--
-- * 'iscrsResponseStatus' - -- | The response status code.
importSourceCredentialsResponse ::
  -- | 'iscrsResponseStatus'
  Int ->
  ImportSourceCredentialsResponse
importSourceCredentialsResponse pResponseStatus_ =
  ImportSourceCredentialsResponse'
    { _iscrsArn = Nothing,
      _iscrsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the token.
iscrsArn :: Lens' ImportSourceCredentialsResponse (Maybe Text)
iscrsArn = lens _iscrsArn (\s a -> s {_iscrsArn = a})

-- | -- | The response status code.
iscrsResponseStatus :: Lens' ImportSourceCredentialsResponse Int
iscrsResponseStatus = lens _iscrsResponseStatus (\s a -> s {_iscrsResponseStatus = a})

instance NFData ImportSourceCredentialsResponse
