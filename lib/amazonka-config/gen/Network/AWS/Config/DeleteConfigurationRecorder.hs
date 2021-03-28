{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeleteConfigurationRecorder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the configuration recorder.
--
-- After the configuration recorder is deleted, AWS Config will not record resource configuration changes until you create a new configuration recorder.
-- This action does not delete the configuration information that was previously recorded. You will be able to access the previously recorded information by using the @GetResourceConfigHistory@ action, but you will not be able to access this information in the AWS Config console until you create a new configuration recorder.
module Network.AWS.Config.DeleteConfigurationRecorder
    (
    -- * Creating a request
      DeleteConfigurationRecorder (..)
    , mkDeleteConfigurationRecorder
    -- ** Request lenses
    , dcrConfigurationRecorderName

    -- * Destructuring the response
    , DeleteConfigurationRecorderResponse (..)
    , mkDeleteConfigurationRecorderResponse
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request object for the @DeleteConfigurationRecorder@ action.
--
-- /See:/ 'mkDeleteConfigurationRecorder' smart constructor.
newtype DeleteConfigurationRecorder = DeleteConfigurationRecorder'
  { configurationRecorderName :: Types.ConfigurationRecorderName
    -- ^ The name of the configuration recorder to be deleted. You can retrieve the name of your configuration recorder by using the @DescribeConfigurationRecorders@ action.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConfigurationRecorder' value with any optional fields omitted.
mkDeleteConfigurationRecorder
    :: Types.ConfigurationRecorderName -- ^ 'configurationRecorderName'
    -> DeleteConfigurationRecorder
mkDeleteConfigurationRecorder configurationRecorderName
  = DeleteConfigurationRecorder'{configurationRecorderName}

-- | The name of the configuration recorder to be deleted. You can retrieve the name of your configuration recorder by using the @DescribeConfigurationRecorders@ action.
--
-- /Note:/ Consider using 'configurationRecorderName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrConfigurationRecorderName :: Lens.Lens' DeleteConfigurationRecorder Types.ConfigurationRecorderName
dcrConfigurationRecorderName = Lens.field @"configurationRecorderName"
{-# INLINEABLE dcrConfigurationRecorderName #-}
{-# DEPRECATED configurationRecorderName "Use generic-lens or generic-optics with 'configurationRecorderName' instead"  #-}

instance Core.ToQuery DeleteConfigurationRecorder where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteConfigurationRecorder where
        toHeaders DeleteConfigurationRecorder{..}
          = Core.pure
              ("X-Amz-Target", "StarlingDoveService.DeleteConfigurationRecorder")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteConfigurationRecorder where
        toJSON DeleteConfigurationRecorder{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("ConfigurationRecorderName" Core..= configurationRecorderName)])

instance Core.AWSRequest DeleteConfigurationRecorder where
        type Rs DeleteConfigurationRecorder =
             DeleteConfigurationRecorderResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeleteConfigurationRecorderResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteConfigurationRecorderResponse' smart constructor.
data DeleteConfigurationRecorderResponse = DeleteConfigurationRecorderResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConfigurationRecorderResponse' value with any optional fields omitted.
mkDeleteConfigurationRecorderResponse
    :: DeleteConfigurationRecorderResponse
mkDeleteConfigurationRecorderResponse
  = DeleteConfigurationRecorderResponse'
