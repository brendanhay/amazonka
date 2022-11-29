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
-- Module      : Amazonka.Location.Types.TimeZone
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.TimeZone where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a time zone. Includes the name of the time zone and
-- the offset from UTC in seconds.
--
-- /See:/ 'newTimeZone' smart constructor.
data TimeZone = TimeZone'
  { -- | The time zone\'s offset, in seconds, from UTC.
    offset :: Prelude.Maybe Prelude.Int,
    -- | The name of the time zone, following the
    -- <https://www.iana.org/time-zones IANA time zone standard>. For example,
    -- @America\/Los_Angeles@.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeZone' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'offset', 'timeZone_offset' - The time zone\'s offset, in seconds, from UTC.
--
-- 'name', 'timeZone_name' - The name of the time zone, following the
-- <https://www.iana.org/time-zones IANA time zone standard>. For example,
-- @America\/Los_Angeles@.
newTimeZone ::
  -- | 'name'
  Prelude.Text ->
  TimeZone
newTimeZone pName_ =
  TimeZone' {offset = Prelude.Nothing, name = pName_}

-- | The time zone\'s offset, in seconds, from UTC.
timeZone_offset :: Lens.Lens' TimeZone (Prelude.Maybe Prelude.Int)
timeZone_offset = Lens.lens (\TimeZone' {offset} -> offset) (\s@TimeZone' {} a -> s {offset = a} :: TimeZone)

-- | The name of the time zone, following the
-- <https://www.iana.org/time-zones IANA time zone standard>. For example,
-- @America\/Los_Angeles@.
timeZone_name :: Lens.Lens' TimeZone Prelude.Text
timeZone_name = Lens.lens (\TimeZone' {name} -> name) (\s@TimeZone' {} a -> s {name = a} :: TimeZone)

instance Core.FromJSON TimeZone where
  parseJSON =
    Core.withObject
      "TimeZone"
      ( \x ->
          TimeZone'
            Prelude.<$> (x Core..:? "Offset") Prelude.<*> (x Core..: "Name")
      )

instance Prelude.Hashable TimeZone where
  hashWithSalt _salt TimeZone' {..} =
    _salt `Prelude.hashWithSalt` offset
      `Prelude.hashWithSalt` name

instance Prelude.NFData TimeZone where
  rnf TimeZone' {..} =
    Prelude.rnf offset `Prelude.seq` Prelude.rnf name
