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
-- Module      : Network.AWS.IoT.CreateDomainConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a domain configuration.
module Network.AWS.IoT.CreateDomainConfiguration
  ( -- * Creating a Request
    createDomainConfiguration,
    CreateDomainConfiguration,

    -- * Request Lenses
    cdcAuthorizerConfig,
    cdcServerCertificateARNs,
    cdcDomainName,
    cdcServiceType,
    cdcValidationCertificateARN,
    cdcTags,
    cdcDomainConfigurationName,

    -- * Destructuring the Response
    createDomainConfigurationResponse,
    CreateDomainConfigurationResponse,

    -- * Response Lenses
    cdcrsDomainConfigurationName,
    cdcrsDomainConfigurationARN,
    cdcrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDomainConfiguration' smart constructor.
data CreateDomainConfiguration = CreateDomainConfiguration'
  { _cdcAuthorizerConfig ::
      !(Maybe AuthorizerConfig),
    _cdcServerCertificateARNs ::
      !(Maybe [Text]),
    _cdcDomainName :: !(Maybe Text),
    _cdcServiceType :: !(Maybe ServiceType),
    _cdcValidationCertificateARN ::
      !(Maybe Text),
    _cdcTags :: !(Maybe [Tag]),
    _cdcDomainConfigurationName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDomainConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdcAuthorizerConfig' - An object that specifies the authorization service for a domain.
--
-- * 'cdcServerCertificateARNs' - The ARNs of the certificates that AWS IoT passes to the device during the TLS handshake. Currently you can specify only one certificate ARN. This value is not required for AWS-managed domains.
--
-- * 'cdcDomainName' - The name of the domain.
--
-- * 'cdcServiceType' - The type of service delivered by the endpoint.
--
-- * 'cdcValidationCertificateARN' - The certificate used to validate the server certificate and prove domain name ownership. This certificate must be signed by a public certificate authority. This value is not required for AWS-managed domains.
--
-- * 'cdcTags' - Metadata which can be used to manage the domain configuration.
--
-- * 'cdcDomainConfigurationName' - The name of the domain configuration. This value must be unique to a region.
createDomainConfiguration ::
  -- | 'cdcDomainConfigurationName'
  Text ->
  CreateDomainConfiguration
createDomainConfiguration pDomainConfigurationName_ =
  CreateDomainConfiguration'
    { _cdcAuthorizerConfig = Nothing,
      _cdcServerCertificateARNs = Nothing,
      _cdcDomainName = Nothing,
      _cdcServiceType = Nothing,
      _cdcValidationCertificateARN = Nothing,
      _cdcTags = Nothing,
      _cdcDomainConfigurationName = pDomainConfigurationName_
    }

-- | An object that specifies the authorization service for a domain.
cdcAuthorizerConfig :: Lens' CreateDomainConfiguration (Maybe AuthorizerConfig)
cdcAuthorizerConfig = lens _cdcAuthorizerConfig (\s a -> s {_cdcAuthorizerConfig = a})

-- | The ARNs of the certificates that AWS IoT passes to the device during the TLS handshake. Currently you can specify only one certificate ARN. This value is not required for AWS-managed domains.
cdcServerCertificateARNs :: Lens' CreateDomainConfiguration [Text]
cdcServerCertificateARNs = lens _cdcServerCertificateARNs (\s a -> s {_cdcServerCertificateARNs = a}) . _Default . _Coerce

-- | The name of the domain.
cdcDomainName :: Lens' CreateDomainConfiguration (Maybe Text)
cdcDomainName = lens _cdcDomainName (\s a -> s {_cdcDomainName = a})

-- | The type of service delivered by the endpoint.
cdcServiceType :: Lens' CreateDomainConfiguration (Maybe ServiceType)
cdcServiceType = lens _cdcServiceType (\s a -> s {_cdcServiceType = a})

-- | The certificate used to validate the server certificate and prove domain name ownership. This certificate must be signed by a public certificate authority. This value is not required for AWS-managed domains.
cdcValidationCertificateARN :: Lens' CreateDomainConfiguration (Maybe Text)
cdcValidationCertificateARN = lens _cdcValidationCertificateARN (\s a -> s {_cdcValidationCertificateARN = a})

-- | Metadata which can be used to manage the domain configuration.
cdcTags :: Lens' CreateDomainConfiguration [Tag]
cdcTags = lens _cdcTags (\s a -> s {_cdcTags = a}) . _Default . _Coerce

-- | The name of the domain configuration. This value must be unique to a region.
cdcDomainConfigurationName :: Lens' CreateDomainConfiguration Text
cdcDomainConfigurationName = lens _cdcDomainConfigurationName (\s a -> s {_cdcDomainConfigurationName = a})

instance AWSRequest CreateDomainConfiguration where
  type
    Rs CreateDomainConfiguration =
      CreateDomainConfigurationResponse
  request = postJSON ioT
  response =
    receiveJSON
      ( \s h x ->
          CreateDomainConfigurationResponse'
            <$> (x .?> "domainConfigurationName")
            <*> (x .?> "domainConfigurationArn")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateDomainConfiguration

instance NFData CreateDomainConfiguration

instance ToHeaders CreateDomainConfiguration where
  toHeaders = const mempty

instance ToJSON CreateDomainConfiguration where
  toJSON CreateDomainConfiguration' {..} =
    object
      ( catMaybes
          [ ("authorizerConfig" .=) <$> _cdcAuthorizerConfig,
            ("serverCertificateArns" .=) <$> _cdcServerCertificateARNs,
            ("domainName" .=) <$> _cdcDomainName,
            ("serviceType" .=) <$> _cdcServiceType,
            ("validationCertificateArn" .=) <$> _cdcValidationCertificateARN,
            ("tags" .=) <$> _cdcTags
          ]
      )

instance ToPath CreateDomainConfiguration where
  toPath CreateDomainConfiguration' {..} =
    mconcat
      ["/domainConfigurations/", toBS _cdcDomainConfigurationName]

instance ToQuery CreateDomainConfiguration where
  toQuery = const mempty

-- | /See:/ 'createDomainConfigurationResponse' smart constructor.
data CreateDomainConfigurationResponse = CreateDomainConfigurationResponse'
  { _cdcrsDomainConfigurationName ::
      !(Maybe Text),
    _cdcrsDomainConfigurationARN ::
      !(Maybe Text),
    _cdcrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDomainConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdcrsDomainConfigurationName' - The name of the domain configuration.
--
-- * 'cdcrsDomainConfigurationARN' - The ARN of the domain configuration.
--
-- * 'cdcrsResponseStatus' - -- | The response status code.
createDomainConfigurationResponse ::
  -- | 'cdcrsResponseStatus'
  Int ->
  CreateDomainConfigurationResponse
createDomainConfigurationResponse pResponseStatus_ =
  CreateDomainConfigurationResponse'
    { _cdcrsDomainConfigurationName =
        Nothing,
      _cdcrsDomainConfigurationARN = Nothing,
      _cdcrsResponseStatus = pResponseStatus_
    }

-- | The name of the domain configuration.
cdcrsDomainConfigurationName :: Lens' CreateDomainConfigurationResponse (Maybe Text)
cdcrsDomainConfigurationName = lens _cdcrsDomainConfigurationName (\s a -> s {_cdcrsDomainConfigurationName = a})

-- | The ARN of the domain configuration.
cdcrsDomainConfigurationARN :: Lens' CreateDomainConfigurationResponse (Maybe Text)
cdcrsDomainConfigurationARN = lens _cdcrsDomainConfigurationARN (\s a -> s {_cdcrsDomainConfigurationARN = a})

-- | -- | The response status code.
cdcrsResponseStatus :: Lens' CreateDomainConfigurationResponse Int
cdcrsResponseStatus = lens _cdcrsResponseStatus (\s a -> s {_cdcrsResponseStatus = a})

instance NFData CreateDomainConfigurationResponse
