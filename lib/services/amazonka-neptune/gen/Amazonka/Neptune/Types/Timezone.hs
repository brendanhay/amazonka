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
-- Module      : Amazonka.Neptune.Types.Timezone
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Neptune.Types.Timezone where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A time zone associated with a DBInstance.
--
-- /See:/ 'newTimezone' smart constructor.
data Timezone = Timezone'
  { -- | The name of the time zone.
    timezoneName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Timezone' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timezoneName', 'timezone_timezoneName' - The name of the time zone.
newTimezone ::
  Timezone
newTimezone =
  Timezone' {timezoneName = Prelude.Nothing}

-- | The name of the time zone.
timezone_timezoneName :: Lens.Lens' Timezone (Prelude.Maybe Prelude.Text)
timezone_timezoneName = Lens.lens (\Timezone' {timezoneName} -> timezoneName) (\s@Timezone' {} a -> s {timezoneName = a} :: Timezone)

instance Data.FromXML Timezone where
  parseXML x =
    Timezone' Prelude.<$> (x Data..@? "TimezoneName")

instance Prelude.Hashable Timezone where
  hashWithSalt _salt Timezone' {..} =
    _salt `Prelude.hashWithSalt` timezoneName

instance Prelude.NFData Timezone where
  rnf Timezone' {..} = Prelude.rnf timezoneName
