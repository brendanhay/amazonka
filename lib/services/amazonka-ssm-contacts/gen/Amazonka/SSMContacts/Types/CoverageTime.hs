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
-- Module      : Amazonka.SSMContacts.Types.CoverageTime
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMContacts.Types.CoverageTime where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMContacts.Types.HandOffTime

-- | Information about when an on-call shift begins and ends.
--
-- /See:/ 'newCoverageTime' smart constructor.
data CoverageTime = CoverageTime'
  { -- | Information about when the on-call rotation shift ends.
    end :: Prelude.Maybe HandOffTime,
    -- | Information about when the on-call rotation shift begins.
    start :: Prelude.Maybe HandOffTime
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoverageTime' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'end', 'coverageTime_end' - Information about when the on-call rotation shift ends.
--
-- 'start', 'coverageTime_start' - Information about when the on-call rotation shift begins.
newCoverageTime ::
  CoverageTime
newCoverageTime =
  CoverageTime'
    { end = Prelude.Nothing,
      start = Prelude.Nothing
    }

-- | Information about when the on-call rotation shift ends.
coverageTime_end :: Lens.Lens' CoverageTime (Prelude.Maybe HandOffTime)
coverageTime_end = Lens.lens (\CoverageTime' {end} -> end) (\s@CoverageTime' {} a -> s {end = a} :: CoverageTime)

-- | Information about when the on-call rotation shift begins.
coverageTime_start :: Lens.Lens' CoverageTime (Prelude.Maybe HandOffTime)
coverageTime_start = Lens.lens (\CoverageTime' {start} -> start) (\s@CoverageTime' {} a -> s {start = a} :: CoverageTime)

instance Data.FromJSON CoverageTime where
  parseJSON =
    Data.withObject
      "CoverageTime"
      ( \x ->
          CoverageTime'
            Prelude.<$> (x Data..:? "End")
            Prelude.<*> (x Data..:? "Start")
      )

instance Prelude.Hashable CoverageTime where
  hashWithSalt _salt CoverageTime' {..} =
    _salt
      `Prelude.hashWithSalt` end
      `Prelude.hashWithSalt` start

instance Prelude.NFData CoverageTime where
  rnf CoverageTime' {..} =
    Prelude.rnf end `Prelude.seq` Prelude.rnf start

instance Data.ToJSON CoverageTime where
  toJSON CoverageTime' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("End" Data..=) Prelude.<$> end,
            ("Start" Data..=) Prelude.<$> start
          ]
      )
