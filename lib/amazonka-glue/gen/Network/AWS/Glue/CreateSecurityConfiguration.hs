{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateSecurityConfiguration (..)
    , mkCreateSecurityConfiguration
    -- ** Request lenses
    , cscName
    , cscEncryptionConfiguration

    -- * Destructuring the response
    , CreateSecurityConfigurationResponse (..)
    , mkCreateSecurityConfigurationResponse
    -- ** Response lenses
    , cscrrsCreatedTimestamp
    , cscrrsName
    , cscrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateSecurityConfiguration' smart constructor.
data CreateSecurityConfiguration = CreateSecurityConfiguration'
  { name :: Types.NameString
    -- ^ The name for the new security configuration.
  , encryptionConfiguration :: Types.EncryptionConfiguration
    -- ^ The encryption configuration for the new security configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSecurityConfiguration' value with any optional fields omitted.
mkCreateSecurityConfiguration
    :: Types.NameString -- ^ 'name'
    -> Types.EncryptionConfiguration -- ^ 'encryptionConfiguration'
    -> CreateSecurityConfiguration
mkCreateSecurityConfiguration name encryptionConfiguration
  = CreateSecurityConfiguration'{name, encryptionConfiguration}

-- | The name for the new security configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscName :: Lens.Lens' CreateSecurityConfiguration Types.NameString
cscName = Lens.field @"name"
{-# INLINEABLE cscName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The encryption configuration for the new security configuration.
--
-- /Note:/ Consider using 'encryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscEncryptionConfiguration :: Lens.Lens' CreateSecurityConfiguration Types.EncryptionConfiguration
cscEncryptionConfiguration = Lens.field @"encryptionConfiguration"
{-# INLINEABLE cscEncryptionConfiguration #-}
{-# DEPRECATED encryptionConfiguration "Use generic-lens or generic-optics with 'encryptionConfiguration' instead"  #-}

instance Core.ToQuery CreateSecurityConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateSecurityConfiguration where
        toHeaders CreateSecurityConfiguration{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.CreateSecurityConfiguration")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateSecurityConfiguration where
        toJSON CreateSecurityConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just
                    ("EncryptionConfiguration" Core..= encryptionConfiguration)])

instance Core.AWSRequest CreateSecurityConfiguration where
        type Rs CreateSecurityConfiguration =
             CreateSecurityConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateSecurityConfigurationResponse' Core.<$>
                   (x Core..:? "CreatedTimestamp") Core.<*> x Core..:? "Name" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateSecurityConfigurationResponse' smart constructor.
data CreateSecurityConfigurationResponse = CreateSecurityConfigurationResponse'
  { createdTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The time at which the new security configuration was created.
  , name :: Core.Maybe Types.Name
    -- ^ The name assigned to the new security configuration.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateSecurityConfigurationResponse' value with any optional fields omitted.
mkCreateSecurityConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateSecurityConfigurationResponse
mkCreateSecurityConfigurationResponse responseStatus
  = CreateSecurityConfigurationResponse'{createdTimestamp =
                                           Core.Nothing,
                                         name = Core.Nothing, responseStatus}

-- | The time at which the new security configuration was created.
--
-- /Note:/ Consider using 'createdTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscrrsCreatedTimestamp :: Lens.Lens' CreateSecurityConfigurationResponse (Core.Maybe Core.NominalDiffTime)
cscrrsCreatedTimestamp = Lens.field @"createdTimestamp"
{-# INLINEABLE cscrrsCreatedTimestamp #-}
{-# DEPRECATED createdTimestamp "Use generic-lens or generic-optics with 'createdTimestamp' instead"  #-}

-- | The name assigned to the new security configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscrrsName :: Lens.Lens' CreateSecurityConfigurationResponse (Core.Maybe Types.Name)
cscrrsName = Lens.field @"name"
{-# INLINEABLE cscrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscrrsResponseStatus :: Lens.Lens' CreateSecurityConfigurationResponse Core.Int
cscrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cscrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
