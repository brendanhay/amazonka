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
-- Module      : Amazonka.Pinpoint.Types.WriteSegmentRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.WriteSegmentRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.SegmentDimensions
import Amazonka.Pinpoint.Types.SegmentGroupList
import qualified Amazonka.Prelude as Prelude

-- | Specifies the configuration, dimension, and other settings for a
-- segment. A WriteSegmentRequest object can include a Dimensions object or
-- a SegmentGroups object, but not both.
--
-- /See:/ 'newWriteSegmentRequest' smart constructor.
data WriteSegmentRequest = WriteSegmentRequest'
  { -- | The criteria that define the dimensions for the segment.
    dimensions :: Prelude.Maybe SegmentDimensions,
    -- | The name of the segment.
    name :: Prelude.Maybe Prelude.Text,
    -- | The segment group to use and the dimensions to apply to the group\'s
    -- base segments in order to build the segment. A segment group can consist
    -- of zero or more base segments. Your request can include only one segment
    -- group.
    segmentGroups :: Prelude.Maybe SegmentGroupList,
    -- | A string-to-string map of key-value pairs that defines the tags to
    -- associate with the segment. Each tag consists of a required tag key and
    -- an associated tag value.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'dimensions', 'writeSegmentRequest_dimensions' - The criteria that define the dimensions for the segment.
--
-- 'name', 'writeSegmentRequest_name' - The name of the segment.
--
-- 'segmentGroups', 'writeSegmentRequest_segmentGroups' - The segment group to use and the dimensions to apply to the group\'s
-- base segments in order to build the segment. A segment group can consist
-- of zero or more base segments. Your request can include only one segment
-- group.
--
-- 'tags', 'writeSegmentRequest_tags' - A string-to-string map of key-value pairs that defines the tags to
-- associate with the segment. Each tag consists of a required tag key and
-- an associated tag value.
newWriteSegmentRequest ::
  WriteSegmentRequest
newWriteSegmentRequest =
  WriteSegmentRequest'
    { dimensions = Prelude.Nothing,
      name = Prelude.Nothing,
      segmentGroups = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The criteria that define the dimensions for the segment.
writeSegmentRequest_dimensions :: Lens.Lens' WriteSegmentRequest (Prelude.Maybe SegmentDimensions)
writeSegmentRequest_dimensions = Lens.lens (\WriteSegmentRequest' {dimensions} -> dimensions) (\s@WriteSegmentRequest' {} a -> s {dimensions = a} :: WriteSegmentRequest)

-- | The name of the segment.
writeSegmentRequest_name :: Lens.Lens' WriteSegmentRequest (Prelude.Maybe Prelude.Text)
writeSegmentRequest_name = Lens.lens (\WriteSegmentRequest' {name} -> name) (\s@WriteSegmentRequest' {} a -> s {name = a} :: WriteSegmentRequest)

-- | The segment group to use and the dimensions to apply to the group\'s
-- base segments in order to build the segment. A segment group can consist
-- of zero or more base segments. Your request can include only one segment
-- group.
writeSegmentRequest_segmentGroups :: Lens.Lens' WriteSegmentRequest (Prelude.Maybe SegmentGroupList)
writeSegmentRequest_segmentGroups = Lens.lens (\WriteSegmentRequest' {segmentGroups} -> segmentGroups) (\s@WriteSegmentRequest' {} a -> s {segmentGroups = a} :: WriteSegmentRequest)

-- | A string-to-string map of key-value pairs that defines the tags to
-- associate with the segment. Each tag consists of a required tag key and
-- an associated tag value.
writeSegmentRequest_tags :: Lens.Lens' WriteSegmentRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
writeSegmentRequest_tags = Lens.lens (\WriteSegmentRequest' {tags} -> tags) (\s@WriteSegmentRequest' {} a -> s {tags = a} :: WriteSegmentRequest) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable WriteSegmentRequest where
  hashWithSalt _salt WriteSegmentRequest' {..} =
    _salt `Prelude.hashWithSalt` dimensions
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` segmentGroups
      `Prelude.hashWithSalt` tags

instance Prelude.NFData WriteSegmentRequest where
  rnf WriteSegmentRequest' {..} =
    Prelude.rnf dimensions
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf segmentGroups
      `Prelude.seq` Prelude.rnf tags

instance Data.ToJSON WriteSegmentRequest where
  toJSON WriteSegmentRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Dimensions" Data..=) Prelude.<$> dimensions,
            ("Name" Data..=) Prelude.<$> name,
            ("SegmentGroups" Data..=) Prelude.<$> segmentGroups,
            ("tags" Data..=) Prelude.<$> tags
          ]
      )
