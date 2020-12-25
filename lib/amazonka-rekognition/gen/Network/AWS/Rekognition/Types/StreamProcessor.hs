{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.StreamProcessor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.StreamProcessor
  ( StreamProcessor (..),

    -- * Smart constructor
    mkStreamProcessor,

    -- * Lenses
    spName,
    spStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.StreamProcessorName as Types
import qualified Network.AWS.Rekognition.Types.StreamProcessorStatus as Types

-- | An object that recognizes faces in a streaming video. An Amazon Rekognition stream processor is created by a call to 'CreateStreamProcessor' . The request parameters for @CreateStreamProcessor@ describe the Kinesis video stream source for the streaming video, face recognition parameters, and where to stream the analysis resullts.
--
-- /See:/ 'mkStreamProcessor' smart constructor.
data StreamProcessor = StreamProcessor'
  { -- | Name of the Amazon Rekognition stream processor.
    name :: Core.Maybe Types.StreamProcessorName,
    -- | Current status of the Amazon Rekognition stream processor.
    status :: Core.Maybe Types.StreamProcessorStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StreamProcessor' value with any optional fields omitted.
mkStreamProcessor ::
  StreamProcessor
mkStreamProcessor =
  StreamProcessor' {name = Core.Nothing, status = Core.Nothing}

-- | Name of the Amazon Rekognition stream processor.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spName :: Lens.Lens' StreamProcessor (Core.Maybe Types.StreamProcessorName)
spName = Lens.field @"name"
{-# DEPRECATED spName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Current status of the Amazon Rekognition stream processor.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spStatus :: Lens.Lens' StreamProcessor (Core.Maybe Types.StreamProcessorStatus)
spStatus = Lens.field @"status"
{-# DEPRECATED spStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON StreamProcessor where
  parseJSON =
    Core.withObject "StreamProcessor" Core.$
      \x ->
        StreamProcessor'
          Core.<$> (x Core..:? "Name") Core.<*> (x Core..:? "Status")
