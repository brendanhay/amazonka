{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateDomainConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a domain configuration.
--
-- The domain configuration feature is in public preview and is subject to
-- change.
module Network.AWS.IoT.CreateDomainConfiguration
  ( -- * Creating a Request
    CreateDomainConfiguration (..),
    newCreateDomainConfiguration,

    -- * Request Lenses
    createDomainConfiguration_serverCertificateArns,
    createDomainConfiguration_authorizerConfig,
    createDomainConfiguration_domainName,
    createDomainConfiguration_tags,
    createDomainConfiguration_validationCertificateArn,
    createDomainConfiguration_serviceType,
    createDomainConfiguration_domainConfigurationName,

    -- * Destructuring the Response
    CreateDomainConfigurationResponse (..),
    newCreateDomainConfigurationResponse,

    -- * Response Lenses
    createDomainConfigurationResponse_domainConfigurationArn,
    createDomainConfigurationResponse_domainConfigurationName,
    createDomainConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDomainConfiguration' smart constructor.
data CreateDomainConfiguration = CreateDomainConfiguration'
  { -- | The ARNs of the certificates that AWS IoT passes to the device during
    -- the TLS handshake. Currently you can specify only one certificate ARN.
    -- This value is not required for AWS-managed domains.
    serverCertificateArns :: Core.Maybe [Core.Text],
    -- | An object that specifies the authorization service for a domain.
    authorizerConfig :: Core.Maybe AuthorizerConfig,
    -- | The name of the domain.
    domainName :: Core.Maybe Core.Text,
    -- | Metadata which can be used to manage the domain configuration.
    --
    -- For URI Request parameters use format: ...key1=value1&key2=value2...
    --
    -- For the CLI command-line parameter use format: &&tags
    -- \"key1=value1&key2=value2...\"
    --
    -- For the cli-input-json file use format: \"tags\":
    -- \"key1=value1&key2=value2...\"
    tags :: Core.Maybe [Tag],
    -- | The certificate used to validate the server certificate and prove domain
    -- name ownership. This certificate must be signed by a public certificate
    -- authority. This value is not required for AWS-managed domains.
    validationCertificateArn :: Core.Maybe Core.Text,
    -- | The type of service delivered by the endpoint.
    --
    -- AWS IoT Core currently supports only the @DATA@ service type.
    serviceType :: Core.Maybe ServiceType,
    -- | The name of the domain configuration. This value must be unique to a
    -- region.
    domainConfigurationName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDomainConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverCertificateArns', 'createDomainConfiguration_serverCertificateArns' - The ARNs of the certificates that AWS IoT passes to the device during
-- the TLS handshake. Currently you can specify only one certificate ARN.
-- This value is not required for AWS-managed domains.
--
-- 'authorizerConfig', 'createDomainConfiguration_authorizerConfig' - An object that specifies the authorization service for a domain.
--
-- 'domainName', 'createDomainConfiguration_domainName' - The name of the domain.
--
-- 'tags', 'createDomainConfiguration_tags' - Metadata which can be used to manage the domain configuration.
--
-- For URI Request parameters use format: ...key1=value1&key2=value2...
--
-- For the CLI command-line parameter use format: &&tags
-- \"key1=value1&key2=value2...\"
--
-- For the cli-input-json file use format: \"tags\":
-- \"key1=value1&key2=value2...\"
--
-- 'validationCertificateArn', 'createDomainConfiguration_validationCertificateArn' - The certificate used to validate the server certificate and prove domain
-- name ownership. This certificate must be signed by a public certificate
-- authority. This value is not required for AWS-managed domains.
--
-- 'serviceType', 'createDomainConfiguration_serviceType' - The type of service delivered by the endpoint.
--
-- AWS IoT Core currently supports only the @DATA@ service type.
--
-- 'domainConfigurationName', 'createDomainConfiguration_domainConfigurationName' - The name of the domain configuration. This value must be unique to a
-- region.
newCreateDomainConfiguration ::
  -- | 'domainConfigurationName'
  Core.Text ->
  CreateDomainConfiguration
newCreateDomainConfiguration
  pDomainConfigurationName_ =
    CreateDomainConfiguration'
      { serverCertificateArns =
          Core.Nothing,
        authorizerConfig = Core.Nothing,
        domainName = Core.Nothing,
        tags = Core.Nothing,
        validationCertificateArn = Core.Nothing,
        serviceType = Core.Nothing,
        domainConfigurationName =
          pDomainConfigurationName_
      }

-- | The ARNs of the certificates that AWS IoT passes to the device during
-- the TLS handshake. Currently you can specify only one certificate ARN.
-- This value is not required for AWS-managed domains.
createDomainConfiguration_serverCertificateArns :: Lens.Lens' CreateDomainConfiguration (Core.Maybe [Core.Text])
createDomainConfiguration_serverCertificateArns = Lens.lens (\CreateDomainConfiguration' {serverCertificateArns} -> serverCertificateArns) (\s@CreateDomainConfiguration' {} a -> s {serverCertificateArns = a} :: CreateDomainConfiguration) Core.. Lens.mapping Lens._Coerce

-- | An object that specifies the authorization service for a domain.
createDomainConfiguration_authorizerConfig :: Lens.Lens' CreateDomainConfiguration (Core.Maybe AuthorizerConfig)
createDomainConfiguration_authorizerConfig = Lens.lens (\CreateDomainConfiguration' {authorizerConfig} -> authorizerConfig) (\s@CreateDomainConfiguration' {} a -> s {authorizerConfig = a} :: CreateDomainConfiguration)

-- | The name of the domain.
createDomainConfiguration_domainName :: Lens.Lens' CreateDomainConfiguration (Core.Maybe Core.Text)
createDomainConfiguration_domainName = Lens.lens (\CreateDomainConfiguration' {domainName} -> domainName) (\s@CreateDomainConfiguration' {} a -> s {domainName = a} :: CreateDomainConfiguration)

-- | Metadata which can be used to manage the domain configuration.
--
-- For URI Request parameters use format: ...key1=value1&key2=value2...
--
-- For the CLI command-line parameter use format: &&tags
-- \"key1=value1&key2=value2...\"
--
-- For the cli-input-json file use format: \"tags\":
-- \"key1=value1&key2=value2...\"
createDomainConfiguration_tags :: Lens.Lens' CreateDomainConfiguration (Core.Maybe [Tag])
createDomainConfiguration_tags = Lens.lens (\CreateDomainConfiguration' {tags} -> tags) (\s@CreateDomainConfiguration' {} a -> s {tags = a} :: CreateDomainConfiguration) Core.. Lens.mapping Lens._Coerce

-- | The certificate used to validate the server certificate and prove domain
-- name ownership. This certificate must be signed by a public certificate
-- authority. This value is not required for AWS-managed domains.
createDomainConfiguration_validationCertificateArn :: Lens.Lens' CreateDomainConfiguration (Core.Maybe Core.Text)
createDomainConfiguration_validationCertificateArn = Lens.lens (\CreateDomainConfiguration' {validationCertificateArn} -> validationCertificateArn) (\s@CreateDomainConfiguration' {} a -> s {validationCertificateArn = a} :: CreateDomainConfiguration)

-- | The type of service delivered by the endpoint.
--
-- AWS IoT Core currently supports only the @DATA@ service type.
createDomainConfiguration_serviceType :: Lens.Lens' CreateDomainConfiguration (Core.Maybe ServiceType)
createDomainConfiguration_serviceType = Lens.lens (\CreateDomainConfiguration' {serviceType} -> serviceType) (\s@CreateDomainConfiguration' {} a -> s {serviceType = a} :: CreateDomainConfiguration)

-- | The name of the domain configuration. This value must be unique to a
-- region.
createDomainConfiguration_domainConfigurationName :: Lens.Lens' CreateDomainConfiguration Core.Text
createDomainConfiguration_domainConfigurationName = Lens.lens (\CreateDomainConfiguration' {domainConfigurationName} -> domainConfigurationName) (\s@CreateDomainConfiguration' {} a -> s {domainConfigurationName = a} :: CreateDomainConfiguration)

instance Core.AWSRequest CreateDomainConfiguration where
  type
    AWSResponse CreateDomainConfiguration =
      CreateDomainConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDomainConfigurationResponse'
            Core.<$> (x Core..?> "domainConfigurationArn")
            Core.<*> (x Core..?> "domainConfigurationName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateDomainConfiguration

instance Core.NFData CreateDomainConfiguration

instance Core.ToHeaders CreateDomainConfiguration where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON CreateDomainConfiguration where
  toJSON CreateDomainConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("serverCertificateArns" Core..=)
              Core.<$> serverCertificateArns,
            ("authorizerConfig" Core..=)
              Core.<$> authorizerConfig,
            ("domainName" Core..=) Core.<$> domainName,
            ("tags" Core..=) Core.<$> tags,
            ("validationCertificateArn" Core..=)
              Core.<$> validationCertificateArn,
            ("serviceType" Core..=) Core.<$> serviceType
          ]
      )

instance Core.ToPath CreateDomainConfiguration where
  toPath CreateDomainConfiguration' {..} =
    Core.mconcat
      [ "/domainConfigurations/",
        Core.toBS domainConfigurationName
      ]

instance Core.ToQuery CreateDomainConfiguration where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateDomainConfigurationResponse' smart constructor.
data CreateDomainConfigurationResponse = CreateDomainConfigurationResponse'
  { -- | The ARN of the domain configuration.
    domainConfigurationArn :: Core.Maybe Core.Text,
    -- | The name of the domain configuration.
    domainConfigurationName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDomainConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainConfigurationArn', 'createDomainConfigurationResponse_domainConfigurationArn' - The ARN of the domain configuration.
--
-- 'domainConfigurationName', 'createDomainConfigurationResponse_domainConfigurationName' - The name of the domain configuration.
--
-- 'httpStatus', 'createDomainConfigurationResponse_httpStatus' - The response's http status code.
newCreateDomainConfigurationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateDomainConfigurationResponse
newCreateDomainConfigurationResponse pHttpStatus_ =
  CreateDomainConfigurationResponse'
    { domainConfigurationArn =
        Core.Nothing,
      domainConfigurationName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the domain configuration.
createDomainConfigurationResponse_domainConfigurationArn :: Lens.Lens' CreateDomainConfigurationResponse (Core.Maybe Core.Text)
createDomainConfigurationResponse_domainConfigurationArn = Lens.lens (\CreateDomainConfigurationResponse' {domainConfigurationArn} -> domainConfigurationArn) (\s@CreateDomainConfigurationResponse' {} a -> s {domainConfigurationArn = a} :: CreateDomainConfigurationResponse)

-- | The name of the domain configuration.
createDomainConfigurationResponse_domainConfigurationName :: Lens.Lens' CreateDomainConfigurationResponse (Core.Maybe Core.Text)
createDomainConfigurationResponse_domainConfigurationName = Lens.lens (\CreateDomainConfigurationResponse' {domainConfigurationName} -> domainConfigurationName) (\s@CreateDomainConfigurationResponse' {} a -> s {domainConfigurationName = a} :: CreateDomainConfigurationResponse)

-- | The response's http status code.
createDomainConfigurationResponse_httpStatus :: Lens.Lens' CreateDomainConfigurationResponse Core.Int
createDomainConfigurationResponse_httpStatus = Lens.lens (\CreateDomainConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateDomainConfigurationResponse' {} a -> s {httpStatus = a} :: CreateDomainConfigurationResponse)

instance
  Core.NFData
    CreateDomainConfigurationResponse
