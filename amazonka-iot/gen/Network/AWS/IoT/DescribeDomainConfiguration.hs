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
-- Module      : Network.AWS.IoT.DescribeDomainConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets summary information about a domain configuration.
--
-- The domain configuration feature is in public preview and is subject to
-- change.
module Network.AWS.IoT.DescribeDomainConfiguration
  ( -- * Creating a Request
    DescribeDomainConfiguration (..),
    newDescribeDomainConfiguration,

    -- * Request Lenses
    describeDomainConfiguration_domainConfigurationName,

    -- * Destructuring the Response
    DescribeDomainConfigurationResponse (..),
    newDescribeDomainConfigurationResponse,

    -- * Response Lenses
    describeDomainConfigurationResponse_domainConfigurationStatus,
    describeDomainConfigurationResponse_authorizerConfig,
    describeDomainConfigurationResponse_serverCertificates,
    describeDomainConfigurationResponse_domainConfigurationArn,
    describeDomainConfigurationResponse_domainName,
    describeDomainConfigurationResponse_domainConfigurationName,
    describeDomainConfigurationResponse_lastStatusChangeDate,
    describeDomainConfigurationResponse_domainType,
    describeDomainConfigurationResponse_serviceType,
    describeDomainConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeDomainConfiguration' smart constructor.
data DescribeDomainConfiguration = DescribeDomainConfiguration'
  { -- | The name of the domain configuration.
    domainConfigurationName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDomainConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainConfigurationName', 'describeDomainConfiguration_domainConfigurationName' - The name of the domain configuration.
newDescribeDomainConfiguration ::
  -- | 'domainConfigurationName'
  Core.Text ->
  DescribeDomainConfiguration
newDescribeDomainConfiguration
  pDomainConfigurationName_ =
    DescribeDomainConfiguration'
      { domainConfigurationName =
          pDomainConfigurationName_
      }

-- | The name of the domain configuration.
describeDomainConfiguration_domainConfigurationName :: Lens.Lens' DescribeDomainConfiguration Core.Text
describeDomainConfiguration_domainConfigurationName = Lens.lens (\DescribeDomainConfiguration' {domainConfigurationName} -> domainConfigurationName) (\s@DescribeDomainConfiguration' {} a -> s {domainConfigurationName = a} :: DescribeDomainConfiguration)

instance Core.AWSRequest DescribeDomainConfiguration where
  type
    AWSResponse DescribeDomainConfiguration =
      DescribeDomainConfigurationResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDomainConfigurationResponse'
            Core.<$> (x Core..?> "domainConfigurationStatus")
            Core.<*> (x Core..?> "authorizerConfig")
            Core.<*> ( x Core..?> "serverCertificates"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "domainConfigurationArn")
            Core.<*> (x Core..?> "domainName")
            Core.<*> (x Core..?> "domainConfigurationName")
            Core.<*> (x Core..?> "lastStatusChangeDate")
            Core.<*> (x Core..?> "domainType")
            Core.<*> (x Core..?> "serviceType")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeDomainConfiguration

instance Core.NFData DescribeDomainConfiguration

instance Core.ToHeaders DescribeDomainConfiguration where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeDomainConfiguration where
  toPath DescribeDomainConfiguration' {..} =
    Core.mconcat
      [ "/domainConfigurations/",
        Core.toBS domainConfigurationName
      ]

instance Core.ToQuery DescribeDomainConfiguration where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeDomainConfigurationResponse' smart constructor.
data DescribeDomainConfigurationResponse = DescribeDomainConfigurationResponse'
  { -- | A Boolean value that specifies the current state of the domain
    -- configuration.
    domainConfigurationStatus :: Core.Maybe DomainConfigurationStatus,
    -- | An object that specifies the authorization service for a domain.
    authorizerConfig :: Core.Maybe AuthorizerConfig,
    -- | A list containing summary information about the server certificate
    -- included in the domain configuration.
    serverCertificates :: Core.Maybe [ServerCertificateSummary],
    -- | The ARN of the domain configuration.
    domainConfigurationArn :: Core.Maybe Core.Text,
    -- | The name of the domain.
    domainName :: Core.Maybe Core.Text,
    -- | The name of the domain configuration.
    domainConfigurationName :: Core.Maybe Core.Text,
    -- | The date and time the domain configuration\'s status was last changed.
    lastStatusChangeDate :: Core.Maybe Core.POSIX,
    -- | The type of the domain.
    domainType :: Core.Maybe DomainType,
    -- | The type of service delivered by the endpoint.
    serviceType :: Core.Maybe ServiceType,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDomainConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainConfigurationStatus', 'describeDomainConfigurationResponse_domainConfigurationStatus' - A Boolean value that specifies the current state of the domain
-- configuration.
--
-- 'authorizerConfig', 'describeDomainConfigurationResponse_authorizerConfig' - An object that specifies the authorization service for a domain.
--
-- 'serverCertificates', 'describeDomainConfigurationResponse_serverCertificates' - A list containing summary information about the server certificate
-- included in the domain configuration.
--
-- 'domainConfigurationArn', 'describeDomainConfigurationResponse_domainConfigurationArn' - The ARN of the domain configuration.
--
-- 'domainName', 'describeDomainConfigurationResponse_domainName' - The name of the domain.
--
-- 'domainConfigurationName', 'describeDomainConfigurationResponse_domainConfigurationName' - The name of the domain configuration.
--
-- 'lastStatusChangeDate', 'describeDomainConfigurationResponse_lastStatusChangeDate' - The date and time the domain configuration\'s status was last changed.
--
-- 'domainType', 'describeDomainConfigurationResponse_domainType' - The type of the domain.
--
-- 'serviceType', 'describeDomainConfigurationResponse_serviceType' - The type of service delivered by the endpoint.
--
-- 'httpStatus', 'describeDomainConfigurationResponse_httpStatus' - The response's http status code.
newDescribeDomainConfigurationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeDomainConfigurationResponse
newDescribeDomainConfigurationResponse pHttpStatus_ =
  DescribeDomainConfigurationResponse'
    { domainConfigurationStatus =
        Core.Nothing,
      authorizerConfig = Core.Nothing,
      serverCertificates = Core.Nothing,
      domainConfigurationArn = Core.Nothing,
      domainName = Core.Nothing,
      domainConfigurationName = Core.Nothing,
      lastStatusChangeDate = Core.Nothing,
      domainType = Core.Nothing,
      serviceType = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A Boolean value that specifies the current state of the domain
-- configuration.
describeDomainConfigurationResponse_domainConfigurationStatus :: Lens.Lens' DescribeDomainConfigurationResponse (Core.Maybe DomainConfigurationStatus)
describeDomainConfigurationResponse_domainConfigurationStatus = Lens.lens (\DescribeDomainConfigurationResponse' {domainConfigurationStatus} -> domainConfigurationStatus) (\s@DescribeDomainConfigurationResponse' {} a -> s {domainConfigurationStatus = a} :: DescribeDomainConfigurationResponse)

-- | An object that specifies the authorization service for a domain.
describeDomainConfigurationResponse_authorizerConfig :: Lens.Lens' DescribeDomainConfigurationResponse (Core.Maybe AuthorizerConfig)
describeDomainConfigurationResponse_authorizerConfig = Lens.lens (\DescribeDomainConfigurationResponse' {authorizerConfig} -> authorizerConfig) (\s@DescribeDomainConfigurationResponse' {} a -> s {authorizerConfig = a} :: DescribeDomainConfigurationResponse)

-- | A list containing summary information about the server certificate
-- included in the domain configuration.
describeDomainConfigurationResponse_serverCertificates :: Lens.Lens' DescribeDomainConfigurationResponse (Core.Maybe [ServerCertificateSummary])
describeDomainConfigurationResponse_serverCertificates = Lens.lens (\DescribeDomainConfigurationResponse' {serverCertificates} -> serverCertificates) (\s@DescribeDomainConfigurationResponse' {} a -> s {serverCertificates = a} :: DescribeDomainConfigurationResponse) Core.. Lens.mapping Lens._Coerce

-- | The ARN of the domain configuration.
describeDomainConfigurationResponse_domainConfigurationArn :: Lens.Lens' DescribeDomainConfigurationResponse (Core.Maybe Core.Text)
describeDomainConfigurationResponse_domainConfigurationArn = Lens.lens (\DescribeDomainConfigurationResponse' {domainConfigurationArn} -> domainConfigurationArn) (\s@DescribeDomainConfigurationResponse' {} a -> s {domainConfigurationArn = a} :: DescribeDomainConfigurationResponse)

-- | The name of the domain.
describeDomainConfigurationResponse_domainName :: Lens.Lens' DescribeDomainConfigurationResponse (Core.Maybe Core.Text)
describeDomainConfigurationResponse_domainName = Lens.lens (\DescribeDomainConfigurationResponse' {domainName} -> domainName) (\s@DescribeDomainConfigurationResponse' {} a -> s {domainName = a} :: DescribeDomainConfigurationResponse)

-- | The name of the domain configuration.
describeDomainConfigurationResponse_domainConfigurationName :: Lens.Lens' DescribeDomainConfigurationResponse (Core.Maybe Core.Text)
describeDomainConfigurationResponse_domainConfigurationName = Lens.lens (\DescribeDomainConfigurationResponse' {domainConfigurationName} -> domainConfigurationName) (\s@DescribeDomainConfigurationResponse' {} a -> s {domainConfigurationName = a} :: DescribeDomainConfigurationResponse)

-- | The date and time the domain configuration\'s status was last changed.
describeDomainConfigurationResponse_lastStatusChangeDate :: Lens.Lens' DescribeDomainConfigurationResponse (Core.Maybe Core.UTCTime)
describeDomainConfigurationResponse_lastStatusChangeDate = Lens.lens (\DescribeDomainConfigurationResponse' {lastStatusChangeDate} -> lastStatusChangeDate) (\s@DescribeDomainConfigurationResponse' {} a -> s {lastStatusChangeDate = a} :: DescribeDomainConfigurationResponse) Core.. Lens.mapping Core._Time

-- | The type of the domain.
describeDomainConfigurationResponse_domainType :: Lens.Lens' DescribeDomainConfigurationResponse (Core.Maybe DomainType)
describeDomainConfigurationResponse_domainType = Lens.lens (\DescribeDomainConfigurationResponse' {domainType} -> domainType) (\s@DescribeDomainConfigurationResponse' {} a -> s {domainType = a} :: DescribeDomainConfigurationResponse)

-- | The type of service delivered by the endpoint.
describeDomainConfigurationResponse_serviceType :: Lens.Lens' DescribeDomainConfigurationResponse (Core.Maybe ServiceType)
describeDomainConfigurationResponse_serviceType = Lens.lens (\DescribeDomainConfigurationResponse' {serviceType} -> serviceType) (\s@DescribeDomainConfigurationResponse' {} a -> s {serviceType = a} :: DescribeDomainConfigurationResponse)

-- | The response's http status code.
describeDomainConfigurationResponse_httpStatus :: Lens.Lens' DescribeDomainConfigurationResponse Core.Int
describeDomainConfigurationResponse_httpStatus = Lens.lens (\DescribeDomainConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeDomainConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeDomainConfigurationResponse)

instance
  Core.NFData
    DescribeDomainConfigurationResponse
