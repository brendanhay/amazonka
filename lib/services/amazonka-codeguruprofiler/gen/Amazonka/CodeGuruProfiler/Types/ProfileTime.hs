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
-- Module      : Amazonka.CodeGuruProfiler.Types.ProfileTime
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruProfiler.Types.ProfileTime where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the start time of a profile.
--
-- /See:/ 'newProfileTime' smart constructor.
data ProfileTime = ProfileTime'
  { -- | The start time of a profile. It is specified using the ISO 8601 format.
    -- For example, 2020-06-01T13:15:02.001Z represents 1 millisecond past June
    -- 1, 2020 1:15:02 PM UTC.
    start :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProfileTime' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'start', 'profileTime_start' - The start time of a profile. It is specified using the ISO 8601 format.
-- For example, 2020-06-01T13:15:02.001Z represents 1 millisecond past June
-- 1, 2020 1:15:02 PM UTC.
newProfileTime ::
  ProfileTime
newProfileTime =
  ProfileTime' {start = Prelude.Nothing}

-- | The start time of a profile. It is specified using the ISO 8601 format.
-- For example, 2020-06-01T13:15:02.001Z represents 1 millisecond past June
-- 1, 2020 1:15:02 PM UTC.
profileTime_start :: Lens.Lens' ProfileTime (Prelude.Maybe Prelude.UTCTime)
profileTime_start = Lens.lens (\ProfileTime' {start} -> start) (\s@ProfileTime' {} a -> s {start = a} :: ProfileTime) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ProfileTime where
  parseJSON =
    Data.withObject
      "ProfileTime"
      ( \x ->
          ProfileTime' Prelude.<$> (x Data..:? "start")
      )

instance Prelude.Hashable ProfileTime where
  hashWithSalt _salt ProfileTime' {..} =
    _salt `Prelude.hashWithSalt` start

instance Prelude.NFData ProfileTime where
  rnf ProfileTime' {..} = Prelude.rnf start
