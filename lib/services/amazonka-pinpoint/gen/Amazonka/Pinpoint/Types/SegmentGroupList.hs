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
-- Module      : Amazonka.Pinpoint.Types.SegmentGroupList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.SegmentGroupList where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.Include
import Amazonka.Pinpoint.Types.SegmentGroup
import qualified Amazonka.Prelude as Prelude

-- | Specifies the settings that define the relationships between segment
-- groups for a segment.
--
-- /See:/ 'newSegmentGroupList' smart constructor.
data SegmentGroupList = SegmentGroupList'
  { -- | An array that defines the set of segment criteria to evaluate when
    -- handling segment groups for the segment.
    groups :: Prelude.Maybe [SegmentGroup],
    -- | Specifies how to handle multiple segment groups for the segment. For
    -- example, if the segment includes three segment groups, whether the
    -- resulting segment includes endpoints that match all, any, or none of the
    -- segment groups.
    include :: Prelude.Maybe Include
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SegmentGroupList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'segmentGroupList_groups' - An array that defines the set of segment criteria to evaluate when
-- handling segment groups for the segment.
--
-- 'include', 'segmentGroupList_include' - Specifies how to handle multiple segment groups for the segment. For
-- example, if the segment includes three segment groups, whether the
-- resulting segment includes endpoints that match all, any, or none of the
-- segment groups.
newSegmentGroupList ::
  SegmentGroupList
newSegmentGroupList =
  SegmentGroupList'
    { groups = Prelude.Nothing,
      include = Prelude.Nothing
    }

-- | An array that defines the set of segment criteria to evaluate when
-- handling segment groups for the segment.
segmentGroupList_groups :: Lens.Lens' SegmentGroupList (Prelude.Maybe [SegmentGroup])
segmentGroupList_groups = Lens.lens (\SegmentGroupList' {groups} -> groups) (\s@SegmentGroupList' {} a -> s {groups = a} :: SegmentGroupList) Prelude.. Lens.mapping Lens.coerced

-- | Specifies how to handle multiple segment groups for the segment. For
-- example, if the segment includes three segment groups, whether the
-- resulting segment includes endpoints that match all, any, or none of the
-- segment groups.
segmentGroupList_include :: Lens.Lens' SegmentGroupList (Prelude.Maybe Include)
segmentGroupList_include = Lens.lens (\SegmentGroupList' {include} -> include) (\s@SegmentGroupList' {} a -> s {include = a} :: SegmentGroupList)

instance Data.FromJSON SegmentGroupList where
  parseJSON =
    Data.withObject
      "SegmentGroupList"
      ( \x ->
          SegmentGroupList'
            Prelude.<$> (x Data..:? "Groups" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Include")
      )

instance Prelude.Hashable SegmentGroupList where
  hashWithSalt _salt SegmentGroupList' {..} =
    _salt
      `Prelude.hashWithSalt` groups
      `Prelude.hashWithSalt` include

instance Prelude.NFData SegmentGroupList where
  rnf SegmentGroupList' {..} =
    Prelude.rnf groups
      `Prelude.seq` Prelude.rnf include

instance Data.ToJSON SegmentGroupList where
  toJSON SegmentGroupList' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Groups" Data..=) Prelude.<$> groups,
            ("Include" Data..=) Prelude.<$> include
          ]
      )
