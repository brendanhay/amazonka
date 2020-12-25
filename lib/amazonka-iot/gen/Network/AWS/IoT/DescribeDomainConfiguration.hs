{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeDomainConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets summary information about a domain configuration.
module Network.AWS.IoT.DescribeDomainConfiguration
  ( -- * Creating a request
    DescribeDomainConfiguration (..),
    mkDescribeDomainConfiguration,

    -- ** Request lenses
    ddcDomainConfigurationName,

    -- * Destructuring the response
    DescribeDomainConfigurationResponse (..),
    mkDescribeDomainConfigurationResponse,

    -- ** Response lenses
    ddcrrsAuthorizerConfig,
    ddcrrsDomainConfigurationArn,
    ddcrrsDomainConfigurationName,
    ddcrrsDomainConfigurationStatus,
    ddcrrsDomainName,
    ddcrrsDomainType,
    ddcrrsLastStatusChangeDate,
    ddcrrsServerCertificates,
    ddcrrsServiceType,
    ddcrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeDomainConfiguration' smart constructor.
newtype DescribeDomainConfiguration = DescribeDomainConfiguration'
  { -- | The name of the domain configuration.
    domainConfigurationName :: Types.DomainConfigurationName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDomainConfiguration' value with any optional fields omitted.
mkDescribeDomainConfiguration ::
  -- | 'domainConfigurationName'
  Types.DomainConfigurationName ->
  DescribeDomainConfiguration
mkDescribeDomainConfiguration domainConfigurationName =
  DescribeDomainConfiguration' {domainConfigurationName}

-- | The name of the domain configuration.
--
-- /Note:/ Consider using 'domainConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcDomainConfigurationName :: Lens.Lens' DescribeDomainConfiguration Types.DomainConfigurationName
ddcDomainConfigurationName = Lens.field @"domainConfigurationName"
{-# DEPRECATED ddcDomainConfigurationName "Use generic-lens or generic-optics with 'domainConfigurationName' instead." #-}

instance Core.AWSRequest DescribeDomainConfiguration where
  type
    Rs DescribeDomainConfiguration =
      DescribeDomainConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/domainConfigurations/"
                Core.<> (Core.toText domainConfigurationName)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDomainConfigurationResponse'
            Core.<$> (x Core..:? "authorizerConfig")
            Core.<*> (x Core..:? "domainConfigurationArn")
            Core.<*> (x Core..:? "domainConfigurationName")
            Core.<*> (x Core..:? "domainConfigurationStatus")
            Core.<*> (x Core..:? "domainName")
            Core.<*> (x Core..:? "domainType")
            Core.<*> (x Core..:? "lastStatusChangeDate")
            Core.<*> (x Core..:? "serverCertificates")
            Core.<*> (x Core..:? "serviceType")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeDomainConfigurationResponse' smart constructor.
data DescribeDomainConfigurationResponse = DescribeDomainConfigurationResponse'
  { -- | An object that specifies the authorization service for a domain.
    authorizerConfig :: Core.Maybe Types.AuthorizerConfig,
    -- | The ARN of the domain configuration.
    domainConfigurationArn :: Core.Maybe Types.DomainConfigurationArn,
    -- | The name of the domain configuration.
    domainConfigurationName :: Core.Maybe Types.DomainConfigurationName,
    -- | A Boolean value that specifies the current state of the domain configuration.
    domainConfigurationStatus :: Core.Maybe Types.DomainConfigurationStatus,
    -- | The name of the domain.
    domainName :: Core.Maybe Types.DomainName,
    -- | The type of the domain.
    domainType :: Core.Maybe Types.DomainType,
    -- | The date and time the domain configuration's status was last changed.
    lastStatusChangeDate :: Core.Maybe Core.NominalDiffTime,
    -- | A list containing summary information about the server certificate included in the domain configuration.
    serverCertificates :: Core.Maybe [Types.ServerCertificateSummary],
    -- | The type of service delivered by the endpoint.
    serviceType :: Core.Maybe Types.ServiceType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeDomainConfigurationResponse' value with any optional fields omitted.
mkDescribeDomainConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeDomainConfigurationResponse
mkDescribeDomainConfigurationResponse responseStatus =
  DescribeDomainConfigurationResponse'
    { authorizerConfig =
        Core.Nothing,
      domainConfigurationArn = Core.Nothing,
      domainConfigurationName = Core.Nothing,
      domainConfigurationStatus = Core.Nothing,
      domainName = Core.Nothing,
      domainType = Core.Nothing,
      lastStatusChangeDate = Core.Nothing,
      serverCertificates = Core.Nothing,
      serviceType = Core.Nothing,
      responseStatus
    }

-- | An object that specifies the authorization service for a domain.
--
-- /Note:/ Consider using 'authorizerConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrrsAuthorizerConfig :: Lens.Lens' DescribeDomainConfigurationResponse (Core.Maybe Types.AuthorizerConfig)
ddcrrsAuthorizerConfig = Lens.field @"authorizerConfig"
{-# DEPRECATED ddcrrsAuthorizerConfig "Use generic-lens or generic-optics with 'authorizerConfig' instead." #-}

-- | The ARN of the domain configuration.
--
-- /Note:/ Consider using 'domainConfigurationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrrsDomainConfigurationArn :: Lens.Lens' DescribeDomainConfigurationResponse (Core.Maybe Types.DomainConfigurationArn)
ddcrrsDomainConfigurationArn = Lens.field @"domainConfigurationArn"
{-# DEPRECATED ddcrrsDomainConfigurationArn "Use generic-lens or generic-optics with 'domainConfigurationArn' instead." #-}

-- | The name of the domain configuration.
--
-- /Note:/ Consider using 'domainConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrrsDomainConfigurationName :: Lens.Lens' DescribeDomainConfigurationResponse (Core.Maybe Types.DomainConfigurationName)
ddcrrsDomainConfigurationName = Lens.field @"domainConfigurationName"
{-# DEPRECATED ddcrrsDomainConfigurationName "Use generic-lens or generic-optics with 'domainConfigurationName' instead." #-}

-- | A Boolean value that specifies the current state of the domain configuration.
--
-- /Note:/ Consider using 'domainConfigurationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrrsDomainConfigurationStatus :: Lens.Lens' DescribeDomainConfigurationResponse (Core.Maybe Types.DomainConfigurationStatus)
ddcrrsDomainConfigurationStatus = Lens.field @"domainConfigurationStatus"
{-# DEPRECATED ddcrrsDomainConfigurationStatus "Use generic-lens or generic-optics with 'domainConfigurationStatus' instead." #-}

-- | The name of the domain.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrrsDomainName :: Lens.Lens' DescribeDomainConfigurationResponse (Core.Maybe Types.DomainName)
ddcrrsDomainName = Lens.field @"domainName"
{-# DEPRECATED ddcrrsDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The type of the domain.
--
-- /Note:/ Consider using 'domainType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrrsDomainType :: Lens.Lens' DescribeDomainConfigurationResponse (Core.Maybe Types.DomainType)
ddcrrsDomainType = Lens.field @"domainType"
{-# DEPRECATED ddcrrsDomainType "Use generic-lens or generic-optics with 'domainType' instead." #-}

-- | The date and time the domain configuration's status was last changed.
--
-- /Note:/ Consider using 'lastStatusChangeDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrrsLastStatusChangeDate :: Lens.Lens' DescribeDomainConfigurationResponse (Core.Maybe Core.NominalDiffTime)
ddcrrsLastStatusChangeDate = Lens.field @"lastStatusChangeDate"
{-# DEPRECATED ddcrrsLastStatusChangeDate "Use generic-lens or generic-optics with 'lastStatusChangeDate' instead." #-}

-- | A list containing summary information about the server certificate included in the domain configuration.
--
-- /Note:/ Consider using 'serverCertificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrrsServerCertificates :: Lens.Lens' DescribeDomainConfigurationResponse (Core.Maybe [Types.ServerCertificateSummary])
ddcrrsServerCertificates = Lens.field @"serverCertificates"
{-# DEPRECATED ddcrrsServerCertificates "Use generic-lens or generic-optics with 'serverCertificates' instead." #-}

-- | The type of service delivered by the endpoint.
--
-- /Note:/ Consider using 'serviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrrsServiceType :: Lens.Lens' DescribeDomainConfigurationResponse (Core.Maybe Types.ServiceType)
ddcrrsServiceType = Lens.field @"serviceType"
{-# DEPRECATED ddcrrsServiceType "Use generic-lens or generic-optics with 'serviceType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrrsResponseStatus :: Lens.Lens' DescribeDomainConfigurationResponse Core.Int
ddcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
