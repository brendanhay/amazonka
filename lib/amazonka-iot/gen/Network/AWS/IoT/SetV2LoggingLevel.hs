{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.SetV2LoggingLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the logging level.
module Network.AWS.IoT.SetV2LoggingLevel
    (
    -- * Creating a request
      SetV2LoggingLevel (..)
    , mkSetV2LoggingLevel
    -- ** Request lenses
    , svllLogTarget
    , svllLogLevel

    -- * Destructuring the response
    , SetV2LoggingLevelResponse (..)
    , mkSetV2LoggingLevelResponse
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSetV2LoggingLevel' smart constructor.
data SetV2LoggingLevel = SetV2LoggingLevel'
  { logTarget :: Types.LogTarget
    -- ^ The log target.
  , logLevel :: Types.LogLevel
    -- ^ The log level.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetV2LoggingLevel' value with any optional fields omitted.
mkSetV2LoggingLevel
    :: Types.LogTarget -- ^ 'logTarget'
    -> Types.LogLevel -- ^ 'logLevel'
    -> SetV2LoggingLevel
mkSetV2LoggingLevel logTarget logLevel
  = SetV2LoggingLevel'{logTarget, logLevel}

-- | The log target.
--
-- /Note:/ Consider using 'logTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svllLogTarget :: Lens.Lens' SetV2LoggingLevel Types.LogTarget
svllLogTarget = Lens.field @"logTarget"
{-# INLINEABLE svllLogTarget #-}
{-# DEPRECATED logTarget "Use generic-lens or generic-optics with 'logTarget' instead"  #-}

-- | The log level.
--
-- /Note:/ Consider using 'logLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svllLogLevel :: Lens.Lens' SetV2LoggingLevel Types.LogLevel
svllLogLevel = Lens.field @"logLevel"
{-# INLINEABLE svllLogLevel #-}
{-# DEPRECATED logLevel "Use generic-lens or generic-optics with 'logLevel' instead"  #-}

instance Core.ToQuery SetV2LoggingLevel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SetV2LoggingLevel where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON SetV2LoggingLevel where
        toJSON SetV2LoggingLevel{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("logTarget" Core..= logTarget),
                  Core.Just ("logLevel" Core..= logLevel)])

instance Core.AWSRequest SetV2LoggingLevel where
        type Rs SetV2LoggingLevel = SetV2LoggingLevelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/v2LoggingLevel",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull SetV2LoggingLevelResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSetV2LoggingLevelResponse' smart constructor.
data SetV2LoggingLevelResponse = SetV2LoggingLevelResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetV2LoggingLevelResponse' value with any optional fields omitted.
mkSetV2LoggingLevelResponse
    :: SetV2LoggingLevelResponse
mkSetV2LoggingLevelResponse = SetV2LoggingLevelResponse'
