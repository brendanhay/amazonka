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
-- Module      : Amazonka.LicenseManager.Types.DatetimeRange
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.DatetimeRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a time range, in ISO8601-UTC format.
--
-- /See:/ 'newDatetimeRange' smart constructor.
data DatetimeRange = DatetimeRange'
  { -- | End of the time range.
    end :: Prelude.Maybe Prelude.Text,
    -- | Start of the time range.
    begin :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatetimeRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'end', 'datetimeRange_end' - End of the time range.
--
-- 'begin', 'datetimeRange_begin' - Start of the time range.
newDatetimeRange ::
  -- | 'begin'
  Prelude.Text ->
  DatetimeRange
newDatetimeRange pBegin_ =
  DatetimeRange'
    { end = Prelude.Nothing,
      begin = pBegin_
    }

-- | End of the time range.
datetimeRange_end :: Lens.Lens' DatetimeRange (Prelude.Maybe Prelude.Text)
datetimeRange_end = Lens.lens (\DatetimeRange' {end} -> end) (\s@DatetimeRange' {} a -> s {end = a} :: DatetimeRange)

-- | Start of the time range.
datetimeRange_begin :: Lens.Lens' DatetimeRange Prelude.Text
datetimeRange_begin = Lens.lens (\DatetimeRange' {begin} -> begin) (\s@DatetimeRange' {} a -> s {begin = a} :: DatetimeRange)

instance Core.FromJSON DatetimeRange where
  parseJSON =
    Core.withObject
      "DatetimeRange"
      ( \x ->
          DatetimeRange'
            Prelude.<$> (x Core..:? "End") Prelude.<*> (x Core..: "Begin")
      )

instance Prelude.Hashable DatetimeRange where
  hashWithSalt _salt DatetimeRange' {..} =
    _salt `Prelude.hashWithSalt` end
      `Prelude.hashWithSalt` begin

instance Prelude.NFData DatetimeRange where
  rnf DatetimeRange' {..} =
    Prelude.rnf end `Prelude.seq` Prelude.rnf begin

instance Core.ToJSON DatetimeRange where
  toJSON DatetimeRange' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("End" Core..=) Prelude.<$> end,
            Prelude.Just ("Begin" Core..= begin)
          ]
      )
