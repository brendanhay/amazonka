{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.PutConfigurationRecorder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new configuration recorder to record the selected resource configurations.
--
-- You can use this action to change the role @roleARN@ or the @recordingGroup@ of an existing recorder. To change the role, call the action on the existing configuration recorder and specify a role.
module Network.AWS.Config.PutConfigurationRecorder
    (
    -- * Creating a request
      PutConfigurationRecorder (..)
    , mkPutConfigurationRecorder
    -- ** Request lenses
    , pcrConfigurationRecorder

    -- * Destructuring the response
    , PutConfigurationRecorderResponse (..)
    , mkPutConfigurationRecorderResponse
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the 'PutConfigurationRecorder' action.
--
-- /See:/ 'mkPutConfigurationRecorder' smart constructor.
newtype PutConfigurationRecorder = PutConfigurationRecorder'
  { configurationRecorder :: Types.ConfigurationRecorder
    -- ^ The configuration recorder object that records each configuration change made to the resources.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutConfigurationRecorder' value with any optional fields omitted.
mkPutConfigurationRecorder
    :: Types.ConfigurationRecorder -- ^ 'configurationRecorder'
    -> PutConfigurationRecorder
mkPutConfigurationRecorder configurationRecorder
  = PutConfigurationRecorder'{configurationRecorder}

-- | The configuration recorder object that records each configuration change made to the resources.
--
-- /Note:/ Consider using 'configurationRecorder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrConfigurationRecorder :: Lens.Lens' PutConfigurationRecorder Types.ConfigurationRecorder
pcrConfigurationRecorder = Lens.field @"configurationRecorder"
{-# INLINEABLE pcrConfigurationRecorder #-}
{-# DEPRECATED configurationRecorder "Use generic-lens or generic-optics with 'configurationRecorder' instead"  #-}

instance Core.ToQuery PutConfigurationRecorder where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutConfigurationRecorder where
        toHeaders PutConfigurationRecorder{..}
          = Core.pure
              ("X-Amz-Target", "StarlingDoveService.PutConfigurationRecorder")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutConfigurationRecorder where
        toJSON PutConfigurationRecorder{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("ConfigurationRecorder" Core..= configurationRecorder)])

instance Core.AWSRequest PutConfigurationRecorder where
        type Rs PutConfigurationRecorder = PutConfigurationRecorderResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull PutConfigurationRecorderResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutConfigurationRecorderResponse' smart constructor.
data PutConfigurationRecorderResponse = PutConfigurationRecorderResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutConfigurationRecorderResponse' value with any optional fields omitted.
mkPutConfigurationRecorderResponse
    :: PutConfigurationRecorderResponse
mkPutConfigurationRecorderResponse
  = PutConfigurationRecorderResponse'
