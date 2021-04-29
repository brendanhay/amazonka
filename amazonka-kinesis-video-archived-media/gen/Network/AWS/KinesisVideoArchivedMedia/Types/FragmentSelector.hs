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
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.FragmentSelector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.FragmentSelector where

import Network.AWS.KinesisVideoArchivedMedia.Types.FragmentSelectorType
import Network.AWS.KinesisVideoArchivedMedia.Types.TimestampRange
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the timestamp range and timestamp origin of a range of
-- fragments.
--
-- Only fragments with a start timestamp greater than or equal to the given
-- start time and less than or equal to the end time are returned. For
-- example, if a stream contains fragments with the following start
-- timestamps:
--
-- -   00:00:00
--
-- -   00:00:02
--
-- -   00:00:04
--
-- -   00:00:06
--
-- A fragment selector range with a start time of 00:00:01 and end time of
-- 00:00:04 would return the fragments with start times of 00:00:02 and
-- 00:00:04.
--
-- /See:/ 'newFragmentSelector' smart constructor.
data FragmentSelector = FragmentSelector'
  { -- | The origin of the timestamps to use (Server or Producer).
    fragmentSelectorType :: FragmentSelectorType,
    -- | The range of timestamps to return.
    timestampRange :: TimestampRange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FragmentSelector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fragmentSelectorType', 'fragmentSelector_fragmentSelectorType' - The origin of the timestamps to use (Server or Producer).
--
-- 'timestampRange', 'fragmentSelector_timestampRange' - The range of timestamps to return.
newFragmentSelector ::
  -- | 'fragmentSelectorType'
  FragmentSelectorType ->
  -- | 'timestampRange'
  TimestampRange ->
  FragmentSelector
newFragmentSelector
  pFragmentSelectorType_
  pTimestampRange_ =
    FragmentSelector'
      { fragmentSelectorType =
          pFragmentSelectorType_,
        timestampRange = pTimestampRange_
      }

-- | The origin of the timestamps to use (Server or Producer).
fragmentSelector_fragmentSelectorType :: Lens.Lens' FragmentSelector FragmentSelectorType
fragmentSelector_fragmentSelectorType = Lens.lens (\FragmentSelector' {fragmentSelectorType} -> fragmentSelectorType) (\s@FragmentSelector' {} a -> s {fragmentSelectorType = a} :: FragmentSelector)

-- | The range of timestamps to return.
fragmentSelector_timestampRange :: Lens.Lens' FragmentSelector TimestampRange
fragmentSelector_timestampRange = Lens.lens (\FragmentSelector' {timestampRange} -> timestampRange) (\s@FragmentSelector' {} a -> s {timestampRange = a} :: FragmentSelector)

instance Prelude.Hashable FragmentSelector

instance Prelude.NFData FragmentSelector

instance Prelude.ToJSON FragmentSelector where
  toJSON FragmentSelector' {..} =
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
