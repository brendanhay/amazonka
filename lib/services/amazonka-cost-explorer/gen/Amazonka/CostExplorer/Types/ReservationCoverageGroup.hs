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
-- Module      : Amazonka.CostExplorer.Types.ReservationCoverageGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.ReservationCoverageGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.Coverage
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A group of reservations that share a set of attributes.
--
-- /See:/ 'newReservationCoverageGroup' smart constructor.
data ReservationCoverageGroup = ReservationCoverageGroup'
  { -- | The attributes for this group of reservations.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | How much instance usage this group of reservations covered.
    coverage :: Prelude.Maybe Coverage
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReservationCoverageGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'reservationCoverageGroup_attributes' - The attributes for this group of reservations.
--
-- 'coverage', 'reservationCoverageGroup_coverage' - How much instance usage this group of reservations covered.
newReservationCoverageGroup ::
  ReservationCoverageGroup
newReservationCoverageGroup =
  ReservationCoverageGroup'
    { attributes =
        Prelude.Nothing,
      coverage = Prelude.Nothing
    }

-- | The attributes for this group of reservations.
reservationCoverageGroup_attributes :: Lens.Lens' ReservationCoverageGroup (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
reservationCoverageGroup_attributes = Lens.lens (\ReservationCoverageGroup' {attributes} -> attributes) (\s@ReservationCoverageGroup' {} a -> s {attributes = a} :: ReservationCoverageGroup) Prelude.. Lens.mapping Lens.coerced

-- | How much instance usage this group of reservations covered.
reservationCoverageGroup_coverage :: Lens.Lens' ReservationCoverageGroup (Prelude.Maybe Coverage)
reservationCoverageGroup_coverage = Lens.lens (\ReservationCoverageGroup' {coverage} -> coverage) (\s@ReservationCoverageGroup' {} a -> s {coverage = a} :: ReservationCoverageGroup)

instance Data.FromJSON ReservationCoverageGroup where
  parseJSON =
    Data.withObject
      "ReservationCoverageGroup"
      ( \x ->
          ReservationCoverageGroup'
            Prelude.<$> (x Data..:? "Attributes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Coverage")
      )

instance Prelude.Hashable ReservationCoverageGroup where
  hashWithSalt _salt ReservationCoverageGroup' {..} =
    _salt
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` coverage

instance Prelude.NFData ReservationCoverageGroup where
  rnf ReservationCoverageGroup' {..} =
    Prelude.rnf attributes `Prelude.seq`
      Prelude.rnf coverage
