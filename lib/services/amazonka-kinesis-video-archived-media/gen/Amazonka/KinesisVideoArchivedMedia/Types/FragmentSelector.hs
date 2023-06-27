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
-- Module      : Amazonka.KinesisVideoArchivedMedia.Types.FragmentSelector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideoArchivedMedia.Types.FragmentSelector where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideoArchivedMedia.Types.FragmentSelectorType
import Amazonka.KinesisVideoArchivedMedia.Types.TimestampRange
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.Hashable FragmentSelector where
  hashWithSalt _salt FragmentSelector' {..} =
    _salt
      `Prelude.hashWithSalt` fragmentSelectorType
      `Prelude.hashWithSalt` timestampRange

instance Prelude.NFData FragmentSelector where
  rnf FragmentSelector' {..} =
    Prelude.rnf fragmentSelectorType
      `Prelude.seq` Prelude.rnf timestampRange

instance Data.ToJSON FragmentSelector where
  toJSON FragmentSelector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "FragmentSelectorType"
                  Data..= fragmentSelectorType
              ),
            Prelude.Just
              ("TimestampRange" Data..= timestampRange)
          ]
      )
