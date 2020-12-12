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
    wsrSegmentGroups,
    wsrName,
    wsrDimensions,
    wsrTags,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.SegmentDimensions
import Network.AWS.Pinpoint.Types.SegmentGroupList
import qualified Network.AWS.Prelude as Lude

-- | Specifies the configuration, dimension, and other settings for a segment. A WriteSegmentRequest object can include a Dimensions object or a SegmentGroups object, but not both.
--
-- /See:/ 'mkWriteSegmentRequest' smart constructor.
data WriteSegmentRequest = WriteSegmentRequest'
  { segmentGroups ::
      Lude.Maybe SegmentGroupList,
    name :: Lude.Maybe Lude.Text,
    dimensions :: Lude.Maybe SegmentDimensions,
    tags ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WriteSegmentRequest' with the minimum fields required to make a request.
--
-- * 'dimensions' - The criteria that define the dimensions for the segment.
-- * 'name' - The name of the segment.
-- * 'segmentGroups' - The segment group to use and the dimensions to apply to the group's base segments in order to build the segment. A segment group can consist of zero or more base segments. Your request can include only one segment group.
-- * 'tags' - A string-to-string map of key-value pairs that defines the tags to associate with the segment. Each tag consists of a required tag key and an associated tag value.
mkWriteSegmentRequest ::
  WriteSegmentRequest
mkWriteSegmentRequest =
  WriteSegmentRequest'
    { segmentGroups = Lude.Nothing,
      name = Lude.Nothing,
      dimensions = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The segment group to use and the dimensions to apply to the group's base segments in order to build the segment. A segment group can consist of zero or more base segments. Your request can include only one segment group.
--
-- /Note:/ Consider using 'segmentGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wsrSegmentGroups :: Lens.Lens' WriteSegmentRequest (Lude.Maybe SegmentGroupList)
wsrSegmentGroups = Lens.lens (segmentGroups :: WriteSegmentRequest -> Lude.Maybe SegmentGroupList) (\s a -> s {segmentGroups = a} :: WriteSegmentRequest)
{-# DEPRECATED wsrSegmentGroups "Use generic-lens or generic-optics with 'segmentGroups' instead." #-}

-- | The name of the segment.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wsrName :: Lens.Lens' WriteSegmentRequest (Lude.Maybe Lude.Text)
wsrName = Lens.lens (name :: WriteSegmentRequest -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: WriteSegmentRequest)
{-# DEPRECATED wsrName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The criteria that define the dimensions for the segment.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wsrDimensions :: Lens.Lens' WriteSegmentRequest (Lude.Maybe SegmentDimensions)
wsrDimensions = Lens.lens (dimensions :: WriteSegmentRequest -> Lude.Maybe SegmentDimensions) (\s a -> s {dimensions = a} :: WriteSegmentRequest)
{-# DEPRECATED wsrDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

-- | A string-to-string map of key-value pairs that defines the tags to associate with the segment. Each tag consists of a required tag key and an associated tag value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wsrTags :: Lens.Lens' WriteSegmentRequest (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
wsrTags = Lens.lens (tags :: WriteSegmentRequest -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: WriteSegmentRequest)
{-# DEPRECATED wsrTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.ToJSON WriteSegmentRequest where
  toJSON WriteSegmentRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SegmentGroups" Lude..=) Lude.<$> segmentGroups,
            ("Name" Lude..=) Lude.<$> name,
            ("Dimensions" Lude..=) Lude.<$> dimensions,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )
