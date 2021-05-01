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
-- Module      : Network.AWS.Pinpoint.Types.SegmentReference
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SegmentReference where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the segment identifier and version of a segment.
--
-- /See:/ 'newSegmentReference' smart constructor.
data SegmentReference = SegmentReference'
  { -- | The version number of the segment.
    version :: Prelude.Maybe Prelude.Int,
    -- | The unique identifier for the segment.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SegmentReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'segmentReference_version' - The version number of the segment.
--
-- 'id', 'segmentReference_id' - The unique identifier for the segment.
newSegmentReference ::
  -- | 'id'
  Prelude.Text ->
  SegmentReference
newSegmentReference pId_ =
  SegmentReference'
    { version = Prelude.Nothing,
      id = pId_
    }

-- | The version number of the segment.
segmentReference_version :: Lens.Lens' SegmentReference (Prelude.Maybe Prelude.Int)
segmentReference_version = Lens.lens (\SegmentReference' {version} -> version) (\s@SegmentReference' {} a -> s {version = a} :: SegmentReference)

-- | The unique identifier for the segment.
segmentReference_id :: Lens.Lens' SegmentReference Prelude.Text
segmentReference_id = Lens.lens (\SegmentReference' {id} -> id) (\s@SegmentReference' {} a -> s {id = a} :: SegmentReference)

instance Prelude.FromJSON SegmentReference where
  parseJSON =
    Prelude.withObject
      "SegmentReference"
      ( \x ->
          SegmentReference'
            Prelude.<$> (x Prelude..:? "Version")
            Prelude.<*> (x Prelude..: "Id")
      )

instance Prelude.Hashable SegmentReference

instance Prelude.NFData SegmentReference

instance Prelude.ToJSON SegmentReference where
  toJSON SegmentReference' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Version" Prelude..=) Prelude.<$> version,
            Prelude.Just ("Id" Prelude..= id)
          ]
      )
