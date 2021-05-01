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
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.ClipFragmentSelector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.ClipFragmentSelector where

import Network.AWS.KinesisVideoArchivedMedia.Types.ClipFragmentSelectorType
import Network.AWS.KinesisVideoArchivedMedia.Types.ClipTimestampRange
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the timestamp range and timestamp origin of a range of
-- fragments.
--
-- Fragments that have duplicate producer timestamps are deduplicated. This
-- means that if producers are producing a stream of fragments with
-- producer timestamps that are approximately equal to the true clock time,
-- the clip will contain all of the fragments within the requested
-- timestamp range. If some fragments are ingested within the same time
-- range and very different points in time, only the oldest ingested
-- collection of fragments are returned.
--
-- /See:/ 'newClipFragmentSelector' smart constructor.
data ClipFragmentSelector = ClipFragmentSelector'
  { -- | The origin of the timestamps to use (Server or Producer).
    fragmentSelectorType :: ClipFragmentSelectorType,
    -- | The range of timestamps to return.
    timestampRange :: ClipTimestampRange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ClipFragmentSelector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fragmentSelectorType', 'clipFragmentSelector_fragmentSelectorType' - The origin of the timestamps to use (Server or Producer).
--
-- 'timestampRange', 'clipFragmentSelector_timestampRange' - The range of timestamps to return.
newClipFragmentSelector ::
  -- | 'fragmentSelectorType'
  ClipFragmentSelectorType ->
  -- | 'timestampRange'
  ClipTimestampRange ->
  ClipFragmentSelector
newClipFragmentSelector
  pFragmentSelectorType_
  pTimestampRange_ =
    ClipFragmentSelector'
      { fragmentSelectorType =
          pFragmentSelectorType_,
        timestampRange = pTimestampRange_
      }

-- | The origin of the timestamps to use (Server or Producer).
clipFragmentSelector_fragmentSelectorType :: Lens.Lens' ClipFragmentSelector ClipFragmentSelectorType
clipFragmentSelector_fragmentSelectorType = Lens.lens (\ClipFragmentSelector' {fragmentSelectorType} -> fragmentSelectorType) (\s@ClipFragmentSelector' {} a -> s {fragmentSelectorType = a} :: ClipFragmentSelector)

-- | The range of timestamps to return.
clipFragmentSelector_timestampRange :: Lens.Lens' ClipFragmentSelector ClipTimestampRange
clipFragmentSelector_timestampRange = Lens.lens (\ClipFragmentSelector' {timestampRange} -> timestampRange) (\s@ClipFragmentSelector' {} a -> s {timestampRange = a} :: ClipFragmentSelector)

instance Prelude.Hashable ClipFragmentSelector

instance Prelude.NFData ClipFragmentSelector

instance Prelude.ToJSON ClipFragmentSelector where
  toJSON ClipFragmentSelector' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "FragmentSelectorType"
                  Prelude..= fragmentSelectorType
              ),
            Prelude.Just
              ("TimestampRange" Prelude..= timestampRange)
          ]
      )
