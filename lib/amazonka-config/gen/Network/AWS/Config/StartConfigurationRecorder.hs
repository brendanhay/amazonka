{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.StartConfigurationRecorder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts recording configurations of the AWS resources you have selected to record in your AWS account.
--
-- You must have created at least one delivery channel to successfully start the configuration recorder.
module Network.AWS.Config.StartConfigurationRecorder
    (
    -- * Creating a request
      StartConfigurationRecorder (..)
    , mkStartConfigurationRecorder
    -- ** Request lenses
    , sConfigurationRecorderName

    -- * Destructuring the response
    , StartConfigurationRecorderResponse (..)
    , mkStartConfigurationRecorderResponse
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the 'StartConfigurationRecorder' action.
--
-- /See:/ 'mkStartConfigurationRecorder' smart constructor.
newtype StartConfigurationRecorder = StartConfigurationRecorder'
  { configurationRecorderName :: Types.ConfigurationRecorderName
    -- ^ The name of the recorder object that records each configuration change made to the resources.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartConfigurationRecorder' value with any optional fields omitted.
mkStartConfigurationRecorder
    :: Types.ConfigurationRecorderName -- ^ 'configurationRecorderName'
    -> StartConfigurationRecorder
mkStartConfigurationRecorder configurationRecorderName
  = StartConfigurationRecorder'{configurationRecorderName}

-- | The name of the recorder object that records each configuration change made to the resources.
--
-- /Note:/ Consider using 'configurationRecorderName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sConfigurationRecorderName :: Lens.Lens' StartConfigurationRecorder Types.ConfigurationRecorderName
sConfigurationRecorderName = Lens.field @"configurationRecorderName"
{-# INLINEABLE sConfigurationRecorderName #-}
{-# DEPRECATED configurationRecorderName "Use generic-lens or generic-optics with 'configurationRecorderName' instead"  #-}

instance Core.ToQuery StartConfigurationRecorder where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartConfigurationRecorder where
        toHeaders StartConfigurationRecorder{..}
          = Core.pure
              ("X-Amz-Target", "StarlingDoveService.StartConfigurationRecorder")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartConfigurationRecorder where
        toJSON StartConfigurationRecorder{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("ConfigurationRecorderName" Core..= configurationRecorderName)])

instance Core.AWSRequest StartConfigurationRecorder where
        type Rs StartConfigurationRecorder =
             StartConfigurationRecorderResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull StartConfigurationRecorderResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartConfigurationRecorderResponse' smart constructor.
data StartConfigurationRecorderResponse = StartConfigurationRecorderResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartConfigurationRecorderResponse' value with any optional fields omitted.
mkStartConfigurationRecorderResponse
    :: StartConfigurationRecorderResponse
mkStartConfigurationRecorderResponse
  = StartConfigurationRecorderResponse'
