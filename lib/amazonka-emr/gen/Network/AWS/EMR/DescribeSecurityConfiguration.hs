{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.DescribeSecurityConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides the details of a security configuration by returning the configuration JSON.
module Network.AWS.EMR.DescribeSecurityConfiguration
  ( -- * Creating a request
    DescribeSecurityConfiguration (..),
    mkDescribeSecurityConfiguration,

    -- ** Request lenses
    dName,

    -- * Destructuring the response
    DescribeSecurityConfigurationResponse (..),
    mkDescribeSecurityConfigurationResponse,

    -- ** Response lenses
    dscrfrsCreationDateTime,
    dscrfrsName,
    dscrfrsSecurityConfiguration,
    dscrfrsResponseStatus,
  )
where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeSecurityConfiguration' smart constructor.
newtype DescribeSecurityConfiguration = DescribeSecurityConfiguration'
  { -- | The name of the security configuration.
    name :: Types.XmlString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSecurityConfiguration' value with any optional fields omitted.
mkDescribeSecurityConfiguration ::
  -- | 'name'
  Types.XmlString ->
  DescribeSecurityConfiguration
mkDescribeSecurityConfiguration name =
  DescribeSecurityConfiguration' {name}

-- | The name of the security configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' DescribeSecurityConfiguration Types.XmlString
dName = Lens.field @"name"
{-# DEPRECATED dName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON DescribeSecurityConfiguration where
  toJSON DescribeSecurityConfiguration {..} =
    Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest DescribeSecurityConfiguration where
  type
    Rs DescribeSecurityConfiguration =
      DescribeSecurityConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "ElasticMapReduce.DescribeSecurityConfiguration")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSecurityConfigurationResponse'
            Core.<$> (x Core..:? "CreationDateTime")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "SecurityConfiguration")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeSecurityConfigurationResponse' smart constructor.
data DescribeSecurityConfigurationResponse = DescribeSecurityConfigurationResponse'
  { -- | The date and time the security configuration was created
    creationDateTime :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the security configuration.
    name :: Core.Maybe Types.XmlString,
    -- | The security configuration details in JSON format.
    securityConfiguration :: Core.Maybe Types.SecurityConfiguration,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeSecurityConfigurationResponse' value with any optional fields omitted.
mkDescribeSecurityConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeSecurityConfigurationResponse
mkDescribeSecurityConfigurationResponse responseStatus =
  DescribeSecurityConfigurationResponse'
    { creationDateTime =
        Core.Nothing,
      name = Core.Nothing,
      securityConfiguration = Core.Nothing,
      responseStatus
    }

-- | The date and time the security configuration was created
--
-- /Note:/ Consider using 'creationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrfrsCreationDateTime :: Lens.Lens' DescribeSecurityConfigurationResponse (Core.Maybe Core.NominalDiffTime)
dscrfrsCreationDateTime = Lens.field @"creationDateTime"
{-# DEPRECATED dscrfrsCreationDateTime "Use generic-lens or generic-optics with 'creationDateTime' instead." #-}

-- | The name of the security configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrfrsName :: Lens.Lens' DescribeSecurityConfigurationResponse (Core.Maybe Types.XmlString)
dscrfrsName = Lens.field @"name"
{-# DEPRECATED dscrfrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The security configuration details in JSON format.
--
-- /Note:/ Consider using 'securityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrfrsSecurityConfiguration :: Lens.Lens' DescribeSecurityConfigurationResponse (Core.Maybe Types.SecurityConfiguration)
dscrfrsSecurityConfiguration = Lens.field @"securityConfiguration"
{-# DEPRECATED dscrfrsSecurityConfiguration "Use generic-lens or generic-optics with 'securityConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrfrsResponseStatus :: Lens.Lens' DescribeSecurityConfigurationResponse Core.Int
dscrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dscrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
