{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
  ( -- * Creating a request
    CreateSecurityConfiguration (..),
    mkCreateSecurityConfiguration,

    -- ** Request lenses
    cscName,
    cscEncryptionConfiguration,

    -- * Destructuring the response
    CreateSecurityConfigurationResponse (..),
    mkCreateSecurityConfigurationResponse,

    -- ** Response lenses
    cscrrsCreatedTimestamp,
    cscrrsName,
    cscrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateSecurityConfiguration' smart constructor.
data CreateSecurityConfiguration = CreateSecurityConfiguration'
  { -- | The name for the new security configuration.
    name :: Types.NameString,
    -- | The encryption configuration for the new security configuration.
    encryptionConfiguration :: Types.EncryptionConfiguration
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSecurityConfiguration' value with any optional fields omitted.
mkCreateSecurityConfiguration ::
  -- | 'name'
  Types.NameString ->
  -- | 'encryptionConfiguration'
  Types.EncryptionConfiguration ->
  CreateSecurityConfiguration
mkCreateSecurityConfiguration name encryptionConfiguration =
  CreateSecurityConfiguration' {name, encryptionConfiguration}

-- | The name for the new security configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscName :: Lens.Lens' CreateSecurityConfiguration Types.NameString
cscName = Lens.field @"name"
{-# DEPRECATED cscName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The encryption configuration for the new security configuration.
--
-- /Note:/ Consider using 'encryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscEncryptionConfiguration :: Lens.Lens' CreateSecurityConfiguration Types.EncryptionConfiguration
cscEncryptionConfiguration = Lens.field @"encryptionConfiguration"
{-# DEPRECATED cscEncryptionConfiguration "Use generic-lens or generic-optics with 'encryptionConfiguration' instead." #-}

instance Core.FromJSON CreateSecurityConfiguration where
  toJSON CreateSecurityConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just
              ("EncryptionConfiguration" Core..= encryptionConfiguration)
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
          Core.pure ("X-Amz-Target", "AWSGlue.CreateSecurityConfiguration")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSecurityConfigurationResponse'
            Core.<$> (x Core..:? "CreatedTimestamp")
            Core.<*> (x Core..:? "Name")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateSecurityConfigurationResponse' smart constructor.
data CreateSecurityConfigurationResponse = CreateSecurityConfigurationResponse'
  { -- | The time at which the new security configuration was created.
    createdTimestamp :: Core.Maybe Core.NominalDiffTime,
    -- | The name assigned to the new security configuration.
    name :: Core.Maybe Types.Name,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateSecurityConfigurationResponse' value with any optional fields omitted.
mkCreateSecurityConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateSecurityConfigurationResponse
mkCreateSecurityConfigurationResponse responseStatus =
  CreateSecurityConfigurationResponse'
    { createdTimestamp =
        Core.Nothing,
      name = Core.Nothing,
      responseStatus
    }

-- | The time at which the new security configuration was created.
--
-- /Note:/ Consider using 'createdTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscrrsCreatedTimestamp :: Lens.Lens' CreateSecurityConfigurationResponse (Core.Maybe Core.NominalDiffTime)
cscrrsCreatedTimestamp = Lens.field @"createdTimestamp"
{-# DEPRECATED cscrrsCreatedTimestamp "Use generic-lens or generic-optics with 'createdTimestamp' instead." #-}

-- | The name assigned to the new security configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscrrsName :: Lens.Lens' CreateSecurityConfigurationResponse (Core.Maybe Types.Name)
cscrrsName = Lens.field @"name"
{-# DEPRECATED cscrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscrrsResponseStatus :: Lens.Lens' CreateSecurityConfigurationResponse Core.Int
cscrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cscrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
