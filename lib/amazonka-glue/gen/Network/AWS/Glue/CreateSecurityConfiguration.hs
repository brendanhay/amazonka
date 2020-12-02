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
-- Module      : Network.AWS.Glue.CreateSecurityConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new security configuration. A security configuration is a set of security properties that can be used by AWS Glue. You can use a security configuration to encrypt data at rest. For information about using security configurations in AWS Glue, see <https://docs.aws.amazon.com/glue/latest/dg/encryption-security-configuration.html Encrypting Data Written by Crawlers, Jobs, and Development Endpoints> .
module Network.AWS.Glue.CreateSecurityConfiguration
  ( -- * Creating a Request
    createSecurityConfiguration,
    CreateSecurityConfiguration,

    -- * Request Lenses
    cscName,
    cscEncryptionConfiguration,

    -- * Destructuring the Response
    createSecurityConfigurationResponse,
    CreateSecurityConfigurationResponse,

    -- * Response Lenses
    cscrsName,
    cscrsCreatedTimestamp,
    cscrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createSecurityConfiguration' smart constructor.
data CreateSecurityConfiguration = CreateSecurityConfiguration'
  { _cscName ::
      !Text,
    _cscEncryptionConfiguration ::
      !EncryptionConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateSecurityConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cscName' - The name for the new security configuration.
--
-- * 'cscEncryptionConfiguration' - The encryption configuration for the new security configuration.
createSecurityConfiguration ::
  -- | 'cscName'
  Text ->
  -- | 'cscEncryptionConfiguration'
  EncryptionConfiguration ->
  CreateSecurityConfiguration
createSecurityConfiguration pName_ pEncryptionConfiguration_ =
  CreateSecurityConfiguration'
    { _cscName = pName_,
      _cscEncryptionConfiguration = pEncryptionConfiguration_
    }

-- | The name for the new security configuration.
cscName :: Lens' CreateSecurityConfiguration Text
cscName = lens _cscName (\s a -> s {_cscName = a})

-- | The encryption configuration for the new security configuration.
cscEncryptionConfiguration :: Lens' CreateSecurityConfiguration EncryptionConfiguration
cscEncryptionConfiguration = lens _cscEncryptionConfiguration (\s a -> s {_cscEncryptionConfiguration = a})

instance AWSRequest CreateSecurityConfiguration where
  type
    Rs CreateSecurityConfiguration =
      CreateSecurityConfigurationResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          CreateSecurityConfigurationResponse'
            <$> (x .?> "Name")
            <*> (x .?> "CreatedTimestamp")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateSecurityConfiguration

instance NFData CreateSecurityConfiguration

instance ToHeaders CreateSecurityConfiguration where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSGlue.CreateSecurityConfiguration" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateSecurityConfiguration where
  toJSON CreateSecurityConfiguration' {..} =
    object
      ( catMaybes
          [ Just ("Name" .= _cscName),
            Just ("EncryptionConfiguration" .= _cscEncryptionConfiguration)
          ]
      )

instance ToPath CreateSecurityConfiguration where
  toPath = const "/"

instance ToQuery CreateSecurityConfiguration where
  toQuery = const mempty

-- | /See:/ 'createSecurityConfigurationResponse' smart constructor.
data CreateSecurityConfigurationResponse = CreateSecurityConfigurationResponse'
  { _cscrsName ::
      !(Maybe Text),
    _cscrsCreatedTimestamp ::
      !(Maybe POSIX),
    _cscrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateSecurityConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cscrsName' - The name assigned to the new security configuration.
--
-- * 'cscrsCreatedTimestamp' - The time at which the new security configuration was created.
--
-- * 'cscrsResponseStatus' - -- | The response status code.
createSecurityConfigurationResponse ::
  -- | 'cscrsResponseStatus'
  Int ->
  CreateSecurityConfigurationResponse
createSecurityConfigurationResponse pResponseStatus_ =
  CreateSecurityConfigurationResponse'
    { _cscrsName = Nothing,
      _cscrsCreatedTimestamp = Nothing,
      _cscrsResponseStatus = pResponseStatus_
    }

-- | The name assigned to the new security configuration.
cscrsName :: Lens' CreateSecurityConfigurationResponse (Maybe Text)
cscrsName = lens _cscrsName (\s a -> s {_cscrsName = a})

-- | The time at which the new security configuration was created.
cscrsCreatedTimestamp :: Lens' CreateSecurityConfigurationResponse (Maybe UTCTime)
cscrsCreatedTimestamp = lens _cscrsCreatedTimestamp (\s a -> s {_cscrsCreatedTimestamp = a}) . mapping _Time

-- | -- | The response status code.
cscrsResponseStatus :: Lens' CreateSecurityConfigurationResponse Int
cscrsResponseStatus = lens _cscrsResponseStatus (\s a -> s {_cscrsResponseStatus = a})

instance NFData CreateSecurityConfigurationResponse
