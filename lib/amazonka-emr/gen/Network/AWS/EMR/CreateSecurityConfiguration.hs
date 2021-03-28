{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateSecurityConfiguration (..)
    , mkCreateSecurityConfiguration
    -- ** Request lenses
    , cscName
    , cscSecurityConfiguration

    -- * Destructuring the response
    , CreateSecurityConfigurationResponse (..)
    , mkCreateSecurityConfigurationResponse
    -- ** Response lenses
    , cscrrsName
    , cscrrsCreationDateTime
    , cscrrsResponseStatus
    ) where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateSecurityConfiguration' smart constructor.
data CreateSecurityConfiguration = CreateSecurityConfiguration'
  { name :: Types.XmlString
    -- ^ The name of the security configuration.
  , securityConfiguration :: Core.Text
    -- ^ The security configuration details in JSON format. For JSON parameters and examples, see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-security-configurations.html Use Security Configurations to Set Up Cluster Security> in the /Amazon EMR Management Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSecurityConfiguration' value with any optional fields omitted.
mkCreateSecurityConfiguration
    :: Types.XmlString -- ^ 'name'
    -> Core.Text -- ^ 'securityConfiguration'
    -> CreateSecurityConfiguration
mkCreateSecurityConfiguration name securityConfiguration
  = CreateSecurityConfiguration'{name, securityConfiguration}

-- | The name of the security configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscName :: Lens.Lens' CreateSecurityConfiguration Types.XmlString
cscName = Lens.field @"name"
{-# INLINEABLE cscName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The security configuration details in JSON format. For JSON parameters and examples, see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-security-configurations.html Use Security Configurations to Set Up Cluster Security> in the /Amazon EMR Management Guide/ .
--
-- /Note:/ Consider using 'securityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscSecurityConfiguration :: Lens.Lens' CreateSecurityConfiguration Core.Text
cscSecurityConfiguration = Lens.field @"securityConfiguration"
{-# INLINEABLE cscSecurityConfiguration #-}
{-# DEPRECATED securityConfiguration "Use generic-lens or generic-optics with 'securityConfiguration' instead"  #-}

instance Core.ToQuery CreateSecurityConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateSecurityConfiguration where
        toHeaders CreateSecurityConfiguration{..}
          = Core.pure
              ("X-Amz-Target", "ElasticMapReduce.CreateSecurityConfiguration")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateSecurityConfiguration where
        toJSON CreateSecurityConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("SecurityConfiguration" Core..= securityConfiguration)])

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
                   (x Core..: "Name") Core.<*> x Core..: "CreationDateTime" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateSecurityConfigurationResponse' smart constructor.
data CreateSecurityConfigurationResponse = CreateSecurityConfigurationResponse'
  { name :: Types.XmlString
    -- ^ The name of the security configuration.
  , creationDateTime :: Core.NominalDiffTime
    -- ^ The date and time the security configuration was created.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateSecurityConfigurationResponse' value with any optional fields omitted.
mkCreateSecurityConfigurationResponse
    :: Types.XmlString -- ^ 'name'
    -> Core.NominalDiffTime -- ^ 'creationDateTime'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateSecurityConfigurationResponse
mkCreateSecurityConfigurationResponse name creationDateTime
  responseStatus
  = CreateSecurityConfigurationResponse'{name, creationDateTime,
                                         responseStatus}

-- | The name of the security configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscrrsName :: Lens.Lens' CreateSecurityConfigurationResponse Types.XmlString
cscrrsName = Lens.field @"name"
{-# INLINEABLE cscrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The date and time the security configuration was created.
--
-- /Note:/ Consider using 'creationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscrrsCreationDateTime :: Lens.Lens' CreateSecurityConfigurationResponse Core.NominalDiffTime
cscrrsCreationDateTime = Lens.field @"creationDateTime"
{-# INLINEABLE cscrrsCreationDateTime #-}
{-# DEPRECATED creationDateTime "Use generic-lens or generic-optics with 'creationDateTime' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscrrsResponseStatus :: Lens.Lens' CreateSecurityConfigurationResponse Core.Int
cscrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cscrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
