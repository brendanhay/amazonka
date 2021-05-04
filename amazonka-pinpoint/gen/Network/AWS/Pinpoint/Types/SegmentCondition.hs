{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Pinpoint.Types.SegmentCondition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SegmentCondition where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies a segment to associate with an activity in a journey.
--
-- /See:/ 'newSegmentCondition' smart constructor.
data SegmentCondition = SegmentCondition'
  { -- | The unique identifier for the segment to associate with the activity.
    segmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SegmentCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'segmentId', 'segmentCondition_segmentId' - The unique identifier for the segment to associate with the activity.
newSegmentCondition ::
  -- | 'segmentId'
  Prelude.Text ->
  SegmentCondition
newSegmentCondition pSegmentId_ =
  SegmentCondition' {segmentId = pSegmentId_}

-- | The unique identifier for the segment to associate with the activity.
segmentCondition_segmentId :: Lens.Lens' SegmentCondition Prelude.Text
segmentCondition_segmentId = Lens.lens (\SegmentCondition' {segmentId} -> segmentId) (\s@SegmentCondition' {} a -> s {segmentId = a} :: SegmentCondition)

instance Prelude.FromJSON SegmentCondition where
  parseJSON =
    Prelude.withObject
      "SegmentCondition"
      ( \x ->
          SegmentCondition'
            Prelude.<$> (x Prelude..: "SegmentId")
      )

instance Prelude.Hashable SegmentCondition

instance Prelude.NFData SegmentCondition

instance Prelude.ToJSON SegmentCondition where
  toJSON SegmentCondition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("SegmentId" Prelude..= segmentId)]
      )
