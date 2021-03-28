{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.CreateMultiplexProgram
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new program in the multiplex.
module Network.AWS.MediaLive.CreateMultiplexProgram
    (
    -- * Creating a request
      CreateMultiplexProgram (..)
    , mkCreateMultiplexProgram
    -- ** Request lenses
    , cmpMultiplexId
    , cmpRequestId
    , cmpMultiplexProgramSettings
    , cmpProgramName

    -- * Destructuring the response
    , CreateMultiplexProgramResponse (..)
    , mkCreateMultiplexProgramResponse
    -- ** Response lenses
    , cmprrsMultiplexProgram
    , cmprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to create a program in a multiplex.
--
-- /See:/ 'mkCreateMultiplexProgram' smart constructor.
data CreateMultiplexProgram = CreateMultiplexProgram'
  { multiplexId :: Core.Text
    -- ^ ID of the multiplex where the program is to be created.
  , requestId :: Core.Text
    -- ^ Unique request ID. This prevents retries from creating multiple
--
-- resources.
  , multiplexProgramSettings :: Types.MultiplexProgramSettings
    -- ^ The settings for this multiplex program.
  , programName :: Core.Text
    -- ^ Name of multiplex program.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateMultiplexProgram' value with any optional fields omitted.
mkCreateMultiplexProgram
    :: Core.Text -- ^ 'multiplexId'
    -> Core.Text -- ^ 'requestId'
    -> Types.MultiplexProgramSettings -- ^ 'multiplexProgramSettings'
    -> Core.Text -- ^ 'programName'
    -> CreateMultiplexProgram
mkCreateMultiplexProgram multiplexId requestId
  multiplexProgramSettings programName
  = CreateMultiplexProgram'{multiplexId, requestId,
                            multiplexProgramSettings, programName}

-- | ID of the multiplex where the program is to be created.
--
-- /Note:/ Consider using 'multiplexId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmpMultiplexId :: Lens.Lens' CreateMultiplexProgram Core.Text
cmpMultiplexId = Lens.field @"multiplexId"
{-# INLINEABLE cmpMultiplexId #-}
{-# DEPRECATED multiplexId "Use generic-lens or generic-optics with 'multiplexId' instead"  #-}

-- | Unique request ID. This prevents retries from creating multiple
--
-- resources.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmpRequestId :: Lens.Lens' CreateMultiplexProgram Core.Text
cmpRequestId = Lens.field @"requestId"
{-# INLINEABLE cmpRequestId #-}
{-# DEPRECATED requestId "Use generic-lens or generic-optics with 'requestId' instead"  #-}

-- | The settings for this multiplex program.
--
-- /Note:/ Consider using 'multiplexProgramSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmpMultiplexProgramSettings :: Lens.Lens' CreateMultiplexProgram Types.MultiplexProgramSettings
cmpMultiplexProgramSettings = Lens.field @"multiplexProgramSettings"
{-# INLINEABLE cmpMultiplexProgramSettings #-}
{-# DEPRECATED multiplexProgramSettings "Use generic-lens or generic-optics with 'multiplexProgramSettings' instead"  #-}

-- | Name of multiplex program.
--
-- /Note:/ Consider using 'programName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmpProgramName :: Lens.Lens' CreateMultiplexProgram Core.Text
cmpProgramName = Lens.field @"programName"
{-# INLINEABLE cmpProgramName #-}
{-# DEPRECATED programName "Use generic-lens or generic-optics with 'programName' instead"  #-}

instance Core.ToQuery CreateMultiplexProgram where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateMultiplexProgram where
        toHeaders CreateMultiplexProgram{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateMultiplexProgram where
        toJSON CreateMultiplexProgram{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("requestId" Core..= requestId),
                  Core.Just
                    ("multiplexProgramSettings" Core..= multiplexProgramSettings),
                  Core.Just ("programName" Core..= programName)])

instance Core.AWSRequest CreateMultiplexProgram where
        type Rs CreateMultiplexProgram = CreateMultiplexProgramResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/prod/multiplexes/" Core.<> Core.toText multiplexId Core.<>
                             "/programs",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateMultiplexProgramResponse' Core.<$>
                   (x Core..:? "multiplexProgram") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Placeholder documentation for CreateMultiplexProgramResponse
--
-- /See:/ 'mkCreateMultiplexProgramResponse' smart constructor.
data CreateMultiplexProgramResponse = CreateMultiplexProgramResponse'
  { multiplexProgram :: Core.Maybe Types.MultiplexProgram
    -- ^ The newly created multiplex program.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateMultiplexProgramResponse' value with any optional fields omitted.
mkCreateMultiplexProgramResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateMultiplexProgramResponse
mkCreateMultiplexProgramResponse responseStatus
  = CreateMultiplexProgramResponse'{multiplexProgram = Core.Nothing,
                                    responseStatus}

-- | The newly created multiplex program.
--
-- /Note:/ Consider using 'multiplexProgram' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmprrsMultiplexProgram :: Lens.Lens' CreateMultiplexProgramResponse (Core.Maybe Types.MultiplexProgram)
cmprrsMultiplexProgram = Lens.field @"multiplexProgram"
{-# INLINEABLE cmprrsMultiplexProgram #-}
{-# DEPRECATED multiplexProgram "Use generic-lens or generic-optics with 'multiplexProgram' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmprrsResponseStatus :: Lens.Lens' CreateMultiplexProgramResponse Core.Int
cmprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cmprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
