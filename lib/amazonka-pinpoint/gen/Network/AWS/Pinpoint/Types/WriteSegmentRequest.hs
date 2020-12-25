{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.WriteSegmentRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.WriteSegmentRequest
  ( WriteSegmentRequest (..),

    -- * Smart constructor
    mkWriteSegmentRequest,

    -- * Lenses
    wsrDimensions,
    wsrName,
    wsrSegmentGroups,
    wsrTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.SegmentDimensions as Types
import qualified Network.AWS.Pinpoint.Types.SegmentGroupList as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the configuration, dimension, and other settings for a segment. A WriteSegmentRequest object can include a Dimensions object or a SegmentGroups object, but not both.
--
-- /See:/ 'mkWriteSegmentRequest' smart constructor.
data WriteSegmentRequest = WriteSegmentRequest'
  { -- | The criteria that define the dimensions for the segment.
    dimensions :: Core.Maybe Types.SegmentDimensions,
    -- | The name of the segment.
    name :: Core.Maybe Core.Text,
    -- | The segment group to use and the dimensions to apply to the group's base segments in order to build the segment. A segment group can consist of zero or more base segments. Your request can include only one segment group.
    segmentGroups :: Core.Maybe Types.SegmentGroupList,
    -- | A string-to-string map of key-value pairs that defines the tags to associate with the segment. Each tag consists of a required tag key and an associated tag value.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WriteSegmentRequest' value with any optional fields omitted.
mkWriteSegmentRequest ::
  WriteSegmentRequest
mkWriteSegmentRequest =
  WriteSegmentRequest'
    { dimensions = Core.Nothing,
      name = Core.Nothing,
      segmentGroups = Core.Nothing,
      tags = Core.Nothing
    }

-- | The criteria that define the dimensions for the segment.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wsrDimensions :: Lens.Lens' WriteSegmentRequest (Core.Maybe Types.SegmentDimensions)
wsrDimensions = Lens.field @"dimensions"
{-# DEPRECATED wsrDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

-- | The name of the segment.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wsrName :: Lens.Lens' WriteSegmentRequest (Core.Maybe Core.Text)
wsrName = Lens.field @"name"
{-# DEPRECATED wsrName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The segment group to use and the dimensions to apply to the group's base segments in order to build the segment. A segment group can consist of zero or more base segments. Your request can include only one segment group.
--
-- /Note:/ Consider using 'segmentGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wsrSegmentGroups :: Lens.Lens' WriteSegmentRequest (Core.Maybe Types.SegmentGroupList)
wsrSegmentGroups = Lens.field @"segmentGroups"
{-# DEPRECATED wsrSegmentGroups "Use generic-lens or generic-optics with 'segmentGroups' instead." #-}

-- | A string-to-string map of key-value pairs that defines the tags to associate with the segment. Each tag consists of a required tag key and an associated tag value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wsrTags :: Lens.Lens' WriteSegmentRequest (Core.Maybe (Core.HashMap Core.Text Core.Text))
wsrTags = Lens.field @"tags"
{-# DEPRECATED wsrTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON WriteSegmentRequest where
  toJSON WriteSegmentRequest {..} =
    Core.object
      ( Core.catMaybes
          [ ("Dimensions" Core..=) Core.<$> dimensions,
            ("Name" Core..=) Core.<$> name,
            ("SegmentGroups" Core..=) Core.<$> segmentGroups,
            ("tags" Core..=) Core.<$> tags
          ]
      )
