{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.UpdateMultiplexProgram
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a program in a multiplex.
module Network.AWS.MediaLive.UpdateMultiplexProgram
    (
    -- * Creating a request
      UpdateMultiplexProgram (..)
    , mkUpdateMultiplexProgram
    -- ** Request lenses
    , umpMultiplexId
    , umpProgramName
    , umpMultiplexProgramSettings

    -- * Destructuring the response
    , UpdateMultiplexProgramResponse (..)
    , mkUpdateMultiplexProgramResponse
    -- ** Response lenses
    , umprrsMultiplexProgram
    , umprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to update a program in a multiplex.
--
-- /See:/ 'mkUpdateMultiplexProgram' smart constructor.
data UpdateMultiplexProgram = UpdateMultiplexProgram'
  { multiplexId :: Core.Text
    -- ^ The ID of the multiplex of the program to update.
  , programName :: Core.Text
    -- ^ The name of the program to update.
  , multiplexProgramSettings :: Core.Maybe Types.MultiplexProgramSettings
    -- ^ The new settings for a multiplex program.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMultiplexProgram' value with any optional fields omitted.
mkUpdateMultiplexProgram
    :: Core.Text -- ^ 'multiplexId'
    -> Core.Text -- ^ 'programName'
    -> UpdateMultiplexProgram
mkUpdateMultiplexProgram multiplexId programName
  = UpdateMultiplexProgram'{multiplexId, programName,
                            multiplexProgramSettings = Core.Nothing}

-- | The ID of the multiplex of the program to update.
--
-- /Note:/ Consider using 'multiplexId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umpMultiplexId :: Lens.Lens' UpdateMultiplexProgram Core.Text
umpMultiplexId = Lens.field @"multiplexId"
{-# INLINEABLE umpMultiplexId #-}
{-# DEPRECATED multiplexId "Use generic-lens or generic-optics with 'multiplexId' instead"  #-}

-- | The name of the program to update.
--
-- /Note:/ Consider using 'programName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umpProgramName :: Lens.Lens' UpdateMultiplexProgram Core.Text
umpProgramName = Lens.field @"programName"
{-# INLINEABLE umpProgramName #-}
{-# DEPRECATED programName "Use generic-lens or generic-optics with 'programName' instead"  #-}

-- | The new settings for a multiplex program.
--
-- /Note:/ Consider using 'multiplexProgramSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umpMultiplexProgramSettings :: Lens.Lens' UpdateMultiplexProgram (Core.Maybe Types.MultiplexProgramSettings)
umpMultiplexProgramSettings = Lens.field @"multiplexProgramSettings"
{-# INLINEABLE umpMultiplexProgramSettings #-}
{-# DEPRECATED multiplexProgramSettings "Use generic-lens or generic-optics with 'multiplexProgramSettings' instead"  #-}

instance Core.ToQuery UpdateMultiplexProgram where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateMultiplexProgram where
        toHeaders UpdateMultiplexProgram{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateMultiplexProgram where
        toJSON UpdateMultiplexProgram{..}
          = Core.object
              (Core.catMaybes
                 [("multiplexProgramSettings" Core..=) Core.<$>
                    multiplexProgramSettings])

instance Core.AWSRequest UpdateMultiplexProgram where
        type Rs UpdateMultiplexProgram = UpdateMultiplexProgramResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/prod/multiplexes/" Core.<> Core.toText multiplexId Core.<>
                             "/programs/"
                             Core.<> Core.toText programName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateMultiplexProgramResponse' Core.<$>
                   (x Core..:? "multiplexProgram") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Placeholder documentation for UpdateMultiplexProgramResponse
--
-- /See:/ 'mkUpdateMultiplexProgramResponse' smart constructor.
data UpdateMultiplexProgramResponse = UpdateMultiplexProgramResponse'
  { multiplexProgram :: Core.Maybe Types.MultiplexProgram
    -- ^ The updated multiplex program.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMultiplexProgramResponse' value with any optional fields omitted.
mkUpdateMultiplexProgramResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateMultiplexProgramResponse
mkUpdateMultiplexProgramResponse responseStatus
  = UpdateMultiplexProgramResponse'{multiplexProgram = Core.Nothing,
                                    responseStatus}

-- | The updated multiplex program.
--
-- /Note:/ Consider using 'multiplexProgram' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umprrsMultiplexProgram :: Lens.Lens' UpdateMultiplexProgramResponse (Core.Maybe Types.MultiplexProgram)
umprrsMultiplexProgram = Lens.field @"multiplexProgram"
{-# INLINEABLE umprrsMultiplexProgram #-}
{-# DEPRECATED multiplexProgram "Use generic-lens or generic-optics with 'multiplexProgram' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umprrsResponseStatus :: Lens.Lens' UpdateMultiplexProgramResponse Core.Int
umprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE umprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
