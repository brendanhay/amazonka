{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.WriteSegmentRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.WriteSegmentRequest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.SegmentDimensions
import Network.AWS.Pinpoint.Types.SegmentGroupList
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the configuration, dimension, and other settings for a
-- segment. A WriteSegmentRequest object can include a Dimensions object or
-- a SegmentGroups object, but not both.
--
-- /See:/ 'newWriteSegmentRequest' smart constructor.
data WriteSegmentRequest = WriteSegmentRequest'
  { -- | The segment group to use and the dimensions to apply to the group\'s
    -- base segments in order to build the segment. A segment group can consist
    -- of zero or more base segments. Your request can include only one segment
    -- group.
    segmentGroups :: Prelude.Maybe SegmentGroupList,
    -- | The name of the segment.
    name :: Prelude.Maybe Prelude.Text,
    -- | A string-to-string map of key-value pairs that defines the tags to
    -- associate with the segment. Each tag consists of a required tag key and
    -- an associated tag value.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The criteria that define the dimensions for the segment.
    dimensions :: Prelude.Maybe SegmentDimensions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WriteSegmentRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'segmentGroups', 'writeSegmentRequest_segmentGroups' - The segment group to use and the dimensions to apply to the group\'s
-- base segments in order to build the segment. A segment group can consist
-- of zero or more base segments. Your request can include only one segment
-- group.
--
-- 'name', 'writeSegmentRequest_name' - The name of the segment.
--
-- 'tags', 'writeSegmentRequest_tags' - A string-to-string map of key-value pairs that defines the tags to
-- associate with the segment. Each tag consists of a required tag key and
-- an associated tag value.
--
-- 'dimensions', 'writeSegmentRequest_dimensions' - The criteria that define the dimensions for the segment.
newWriteSegmentRequest ::
  WriteSegmentRequest
newWriteSegmentRequest =
  WriteSegmentRequest'
    { segmentGroups =
        Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing,
      dimensions = Prelude.Nothing
    }

-- | The segment group to use and the dimensions to apply to the group\'s
-- base segments in order to build the segment. A segment group can consist
-- of zero or more base segments. Your request can include only one segment
-- group.
writeSegmentRequest_segmentGroups :: Lens.Lens' WriteSegmentRequest (Prelude.Maybe SegmentGroupList)
writeSegmentRequest_segmentGroups = Lens.lens (\WriteSegmentRequest' {segmentGroups} -> segmentGroups) (\s@WriteSegmentRequest' {} a -> s {segmentGroups = a} :: WriteSegmentRequest)

-- | The name of the segment.
writeSegmentRequest_name :: Lens.Lens' WriteSegmentRequest (Prelude.Maybe Prelude.Text)
writeSegmentRequest_name = Lens.lens (\WriteSegmentRequest' {name} -> name) (\s@WriteSegmentRequest' {} a -> s {name = a} :: WriteSegmentRequest)

-- | A string-to-string map of key-value pairs that defines the tags to
-- associate with the segment. Each tag consists of a required tag key and
-- an associated tag value.
writeSegmentRequest_tags :: Lens.Lens' WriteSegmentRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
writeSegmentRequest_tags = Lens.lens (\WriteSegmentRequest' {tags} -> tags) (\s@WriteSegmentRequest' {} a -> s {tags = a} :: WriteSegmentRequest) Prelude.. Lens.mapping Lens._Coerce

-- | The criteria that define the dimensions for the segment.
writeSegmentRequest_dimensions :: Lens.Lens' WriteSegmentRequest (Prelude.Maybe SegmentDimensions)
writeSegmentRequest_dimensions = Lens.lens (\WriteSegmentRequest' {dimensions} -> dimensions) (\s@WriteSegmentRequest' {} a -> s {dimensions = a} :: WriteSegmentRequest)

instance Prelude.Hashable WriteSegmentRequest

instance Prelude.NFData WriteSegmentRequest

instance Core.ToJSON WriteSegmentRequest where
  toJSON WriteSegmentRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SegmentGroups" Core..=) Prelude.<$> segmentGroups,
            ("Name" Core..=) Prelude.<$> name,
            ("tags" Core..=) Prelude.<$> tags,
            ("Dimensions" Core..=) Prelude.<$> dimensions
          ]
      )
