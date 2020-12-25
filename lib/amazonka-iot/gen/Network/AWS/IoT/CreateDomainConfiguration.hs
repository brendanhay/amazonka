{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
  ( -- * Creating a request
    CreateDomainConfiguration (..),
    mkCreateDomainConfiguration,

    -- ** Request lenses
    cdcDomainConfigurationName,
    cdcAuthorizerConfig,
    cdcDomainName,
    cdcServerCertificateArns,
    cdcServiceType,
    cdcTags,
    cdcValidationCertificateArn,

    -- * Destructuring the response
    CreateDomainConfigurationResponse (..),
    mkCreateDomainConfigurationResponse,

    -- ** Response lenses
    cdcrrsDomainConfigurationArn,
    cdcrrsDomainConfigurationName,
    cdcrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDomainConfiguration' smart constructor.
data CreateDomainConfiguration = CreateDomainConfiguration'
  { -- | The name of the domain configuration. This value must be unique to a region.
    domainConfigurationName :: Types.DomainConfigurationName,
    -- | An object that specifies the authorization service for a domain.
    authorizerConfig :: Core.Maybe Types.AuthorizerConfig,
    -- | The name of the domain.
    domainName :: Core.Maybe Types.DomainName,
    -- | The ARNs of the certificates that AWS IoT passes to the device during the TLS handshake. Currently you can specify only one certificate ARN. This value is not required for AWS-managed domains.
    serverCertificateArns :: Core.Maybe [Types.AcmCertificateArn],
    -- | The type of service delivered by the endpoint.
    serviceType :: Core.Maybe Types.ServiceType,
    -- | Metadata which can be used to manage the domain configuration.
    tags :: Core.Maybe [Types.Tag],
    -- | The certificate used to validate the server certificate and prove domain name ownership. This certificate must be signed by a public certificate authority. This value is not required for AWS-managed domains.
    validationCertificateArn :: Core.Maybe Types.AcmCertificateArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDomainConfiguration' value with any optional fields omitted.
mkCreateDomainConfiguration ::
  -- | 'domainConfigurationName'
  Types.DomainConfigurationName ->
  CreateDomainConfiguration
mkCreateDomainConfiguration domainConfigurationName =
  CreateDomainConfiguration'
    { domainConfigurationName,
      authorizerConfig = Core.Nothing,
      domainName = Core.Nothing,
      serverCertificateArns = Core.Nothing,
      serviceType = Core.Nothing,
      tags = Core.Nothing,
      validationCertificateArn = Core.Nothing
    }

-- | The name of the domain configuration. This value must be unique to a region.
--
-- /Note:/ Consider using 'domainConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcDomainConfigurationName :: Lens.Lens' CreateDomainConfiguration Types.DomainConfigurationName
cdcDomainConfigurationName = Lens.field @"domainConfigurationName"
{-# DEPRECATED cdcDomainConfigurationName "Use generic-lens or generic-optics with 'domainConfigurationName' instead." #-}

-- | An object that specifies the authorization service for a domain.
--
-- /Note:/ Consider using 'authorizerConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcAuthorizerConfig :: Lens.Lens' CreateDomainConfiguration (Core.Maybe Types.AuthorizerConfig)
cdcAuthorizerConfig = Lens.field @"authorizerConfig"
{-# DEPRECATED cdcAuthorizerConfig "Use generic-lens or generic-optics with 'authorizerConfig' instead." #-}

-- | The name of the domain.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcDomainName :: Lens.Lens' CreateDomainConfiguration (Core.Maybe Types.DomainName)
cdcDomainName = Lens.field @"domainName"
{-# DEPRECATED cdcDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The ARNs of the certificates that AWS IoT passes to the device during the TLS handshake. Currently you can specify only one certificate ARN. This value is not required for AWS-managed domains.
--
-- /Note:/ Consider using 'serverCertificateArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcServerCertificateArns :: Lens.Lens' CreateDomainConfiguration (Core.Maybe [Types.AcmCertificateArn])
cdcServerCertificateArns = Lens.field @"serverCertificateArns"
{-# DEPRECATED cdcServerCertificateArns "Use generic-lens or generic-optics with 'serverCertificateArns' instead." #-}

-- | The type of service delivered by the endpoint.
--
-- /Note:/ Consider using 'serviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcServiceType :: Lens.Lens' CreateDomainConfiguration (Core.Maybe Types.ServiceType)
cdcServiceType = Lens.field @"serviceType"
{-# DEPRECATED cdcServiceType "Use generic-lens or generic-optics with 'serviceType' instead." #-}

-- | Metadata which can be used to manage the domain configuration.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcTags :: Lens.Lens' CreateDomainConfiguration (Core.Maybe [Types.Tag])
cdcTags = Lens.field @"tags"
{-# DEPRECATED cdcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The certificate used to validate the server certificate and prove domain name ownership. This certificate must be signed by a public certificate authority. This value is not required for AWS-managed domains.
--
-- /Note:/ Consider using 'validationCertificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcValidationCertificateArn :: Lens.Lens' CreateDomainConfiguration (Core.Maybe Types.AcmCertificateArn)
cdcValidationCertificateArn = Lens.field @"validationCertificateArn"
{-# DEPRECATED cdcValidationCertificateArn "Use generic-lens or generic-optics with 'validationCertificateArn' instead." #-}

instance Core.FromJSON CreateDomainConfiguration where
  toJSON CreateDomainConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("authorizerConfig" Core..=) Core.<$> authorizerConfig,
            ("domainName" Core..=) Core.<$> domainName,
            ("serverCertificateArns" Core..=) Core.<$> serverCertificateArns,
            ("serviceType" Core..=) Core.<$> serviceType,
            ("tags" Core..=) Core.<$> tags,
            ("validationCertificateArn" Core..=)
              Core.<$> validationCertificateArn
          ]
      )

instance Core.AWSRequest CreateDomainConfiguration where
  type
    Rs CreateDomainConfiguration =
      CreateDomainConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/domainConfigurations/"
                Core.<> (Core.toText domainConfigurationName)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDomainConfigurationResponse'
            Core.<$> (x Core..:? "domainConfigurationArn")
            Core.<*> (x Core..:? "domainConfigurationName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateDomainConfigurationResponse' smart constructor.
data CreateDomainConfigurationResponse = CreateDomainConfigurationResponse'
  { -- | The ARN of the domain configuration.
    domainConfigurationArn :: Core.Maybe Types.DomainConfigurationArn,
    -- | The name of the domain configuration.
    domainConfigurationName :: Core.Maybe Types.DomainConfigurationName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDomainConfigurationResponse' value with any optional fields omitted.
mkCreateDomainConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateDomainConfigurationResponse
mkCreateDomainConfigurationResponse responseStatus =
  CreateDomainConfigurationResponse'
    { domainConfigurationArn =
        Core.Nothing,
      domainConfigurationName = Core.Nothing,
      responseStatus
    }

-- | The ARN of the domain configuration.
--
-- /Note:/ Consider using 'domainConfigurationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcrrsDomainConfigurationArn :: Lens.Lens' CreateDomainConfigurationResponse (Core.Maybe Types.DomainConfigurationArn)
cdcrrsDomainConfigurationArn = Lens.field @"domainConfigurationArn"
{-# DEPRECATED cdcrrsDomainConfigurationArn "Use generic-lens or generic-optics with 'domainConfigurationArn' instead." #-}

-- | The name of the domain configuration.
--
-- /Note:/ Consider using 'domainConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcrrsDomainConfigurationName :: Lens.Lens' CreateDomainConfigurationResponse (Core.Maybe Types.DomainConfigurationName)
cdcrrsDomainConfigurationName = Lens.field @"domainConfigurationName"
{-# DEPRECATED cdcrrsDomainConfigurationName "Use generic-lens or generic-optics with 'domainConfigurationName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcrrsResponseStatus :: Lens.Lens' CreateDomainConfigurationResponse Core.Int
cdcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cdcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
