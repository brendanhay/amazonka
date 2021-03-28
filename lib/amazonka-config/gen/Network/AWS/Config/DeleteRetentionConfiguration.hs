{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeleteRetentionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the retention configuration.
module Network.AWS.Config.DeleteRetentionConfiguration
    (
    -- * Creating a request
      DeleteRetentionConfiguration (..)
    , mkDeleteRetentionConfiguration
    -- ** Request lenses
    , drcRetentionConfigurationName

    -- * Destructuring the response
    , DeleteRetentionConfigurationResponse (..)
    , mkDeleteRetentionConfigurationResponse
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteRetentionConfiguration' smart constructor.
newtype DeleteRetentionConfiguration = DeleteRetentionConfiguration'
  { retentionConfigurationName :: Types.RetentionConfigurationName
    -- ^ The name of the retention configuration to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRetentionConfiguration' value with any optional fields omitted.
mkDeleteRetentionConfiguration
    :: Types.RetentionConfigurationName -- ^ 'retentionConfigurationName'
    -> DeleteRetentionConfiguration
mkDeleteRetentionConfiguration retentionConfigurationName
  = DeleteRetentionConfiguration'{retentionConfigurationName}

-- | The name of the retention configuration to delete.
--
-- /Note:/ Consider using 'retentionConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcRetentionConfigurationName :: Lens.Lens' DeleteRetentionConfiguration Types.RetentionConfigurationName
drcRetentionConfigurationName = Lens.field @"retentionConfigurationName"
{-# INLINEABLE drcRetentionConfigurationName #-}
{-# DEPRECATED retentionConfigurationName "Use generic-lens or generic-optics with 'retentionConfigurationName' instead"  #-}

instance Core.ToQuery DeleteRetentionConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteRetentionConfiguration where
        toHeaders DeleteRetentionConfiguration{..}
          = Core.pure
              ("X-Amz-Target",
               "StarlingDoveService.DeleteRetentionConfiguration")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteRetentionConfiguration where
        toJSON DeleteRetentionConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("RetentionConfigurationName" Core..= retentionConfigurationName)])

instance Core.AWSRequest DeleteRetentionConfiguration where
        type Rs DeleteRetentionConfiguration =
             DeleteRetentionConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeleteRetentionConfigurationResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteRetentionConfigurationResponse' smart constructor.
data DeleteRetentionConfigurationResponse = DeleteRetentionConfigurationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRetentionConfigurationResponse' value with any optional fields omitted.
mkDeleteRetentionConfigurationResponse
    :: DeleteRetentionConfigurationResponse
mkDeleteRetentionConfigurationResponse
  = DeleteRetentionConfigurationResponse'
