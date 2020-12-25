{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.CreateSecurityConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a security configuration, which is stored in the service and can be specified when a cluster is created.
module Network.AWS.EMR.CreateSecurityConfiguration
  ( -- * Creating a request
    CreateSecurityConfiguration (..),
    mkCreateSecurityConfiguration,

    -- ** Request lenses
    cscName,
    cscSecurityConfiguration,

    -- * Destructuring the response
    CreateSecurityConfigurationResponse (..),
    mkCreateSecurityConfigurationResponse,

    -- ** Response lenses
    cscrrsName,
    cscrrsCreationDateTime,
    cscrrsResponseStatus,
  )
where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateSecurityConfiguration' smart constructor.
data CreateSecurityConfiguration = CreateSecurityConfiguration'
  { -- | The name of the security configuration.
    name :: Types.XmlString,
    -- | The security configuration details in JSON format. For JSON parameters and examples, see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-security-configurations.html Use Security Configurations to Set Up Cluster Security> in the /Amazon EMR Management Guide/ .
    securityConfiguration :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSecurityConfiguration' value with any optional fields omitted.
mkCreateSecurityConfiguration ::
  -- | 'name'
  Types.XmlString ->
  -- | 'securityConfiguration'
  Types.String ->
  CreateSecurityConfiguration
mkCreateSecurityConfiguration name securityConfiguration =
  CreateSecurityConfiguration' {name, securityConfiguration}

-- | The name of the security configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscName :: Lens.Lens' CreateSecurityConfiguration Types.XmlString
cscName = Lens.field @"name"
{-# DEPRECATED cscName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The security configuration details in JSON format. For JSON parameters and examples, see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-security-configurations.html Use Security Configurations to Set Up Cluster Security> in the /Amazon EMR Management Guide/ .
--
-- /Note:/ Consider using 'securityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscSecurityConfiguration :: Lens.Lens' CreateSecurityConfiguration Types.String
cscSecurityConfiguration = Lens.field @"securityConfiguration"
{-# DEPRECATED cscSecurityConfiguration "Use generic-lens or generic-optics with 'securityConfiguration' instead." #-}

instance Core.FromJSON CreateSecurityConfiguration where
  toJSON CreateSecurityConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("SecurityConfiguration" Core..= securityConfiguration)
          ]
      )

instance Core.AWSRequest CreateSecurityConfiguration where
  type
    Rs CreateSecurityConfiguration =
      CreateSecurityConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "ElasticMapReduce.CreateSecurityConfiguration")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSecurityConfigurationResponse'
            Core.<$> (x Core..: "Name")
            Core.<*> (x Core..: "CreationDateTime")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateSecurityConfigurationResponse' smart constructor.
data CreateSecurityConfigurationResponse = CreateSecurityConfigurationResponse'
  { -- | The name of the security configuration.
    name :: Types.XmlString,
    -- | The date and time the security configuration was created.
    creationDateTime :: Core.NominalDiffTime,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateSecurityConfigurationResponse' value with any optional fields omitted.
mkCreateSecurityConfigurationResponse ::
  -- | 'name'
  Types.XmlString ->
  -- | 'creationDateTime'
  Core.NominalDiffTime ->
  -- | 'responseStatus'
  Core.Int ->
  CreateSecurityConfigurationResponse
mkCreateSecurityConfigurationResponse
  name
  creationDateTime
  responseStatus =
    CreateSecurityConfigurationResponse'
      { name,
        creationDateTime,
        responseStatus
      }

-- | The name of the security configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscrrsName :: Lens.Lens' CreateSecurityConfigurationResponse Types.XmlString
cscrrsName = Lens.field @"name"
{-# DEPRECATED cscrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The date and time the security configuration was created.
--
-- /Note:/ Consider using 'creationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscrrsCreationDateTime :: Lens.Lens' CreateSecurityConfigurationResponse Core.NominalDiffTime
cscrrsCreationDateTime = Lens.field @"creationDateTime"
{-# DEPRECATED cscrrsCreationDateTime "Use generic-lens or generic-optics with 'creationDateTime' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscrrsResponseStatus :: Lens.Lens' CreateSecurityConfigurationResponse Core.Int
cscrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cscrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
