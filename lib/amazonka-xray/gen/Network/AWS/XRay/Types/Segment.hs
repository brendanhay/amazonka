{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.Segment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.Segment
  ( Segment (..),

    -- * Smart constructor
    mkSegment,

    -- * Lenses
    sDocument,
    sId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.Id as Types
import qualified Network.AWS.XRay.Types.SegmentDocument as Types

-- | A segment from a trace that has been ingested by the X-Ray service. The segment can be compiled from documents uploaded with 'PutTraceSegments' , or an @inferred@ segment for a downstream service, generated from a subsegment sent by the service that called it.
--
-- For the full segment document schema, see <https://docs.aws.amazon.com/xray/latest/devguide/xray-api-segmentdocuments.html AWS X-Ray Segment Documents> in the /AWS X-Ray Developer Guide/ .
--
-- /See:/ 'mkSegment' smart constructor.
data Segment = Segment'
  { -- | The segment document.
    document :: Core.Maybe Types.SegmentDocument,
    -- | The segment's ID.
    id :: Core.Maybe Types.Id
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Segment' value with any optional fields omitted.
mkSegment ::
  Segment
mkSegment = Segment' {document = Core.Nothing, id = Core.Nothing}

-- | The segment document.
--
-- /Note:/ Consider using 'document' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDocument :: Lens.Lens' Segment (Core.Maybe Types.SegmentDocument)
sDocument = Lens.field @"document"
{-# DEPRECATED sDocument "Use generic-lens or generic-optics with 'document' instead." #-}

-- | The segment's ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sId :: Lens.Lens' Segment (Core.Maybe Types.Id)
sId = Lens.field @"id"
{-# DEPRECATED sId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.FromJSON Segment where
  parseJSON =
    Core.withObject "Segment" Core.$
      \x ->
        Segment'
          Core.<$> (x Core..:? "Document") Core.<*> (x Core..:? "Id")
