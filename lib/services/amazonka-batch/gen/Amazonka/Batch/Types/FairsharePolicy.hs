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
-- Module      : Amazonka.Batch.Types.FairsharePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.FairsharePolicy where

import Amazonka.Batch.Types.ShareAttributes
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The fair share policy for a scheduling policy.
--
-- /See:/ 'newFairsharePolicy' smart constructor.
data FairsharePolicy = FairsharePolicy'
  { -- | A value used to reserve some of the available maximum vCPU for fair
    -- share identifiers that aren\'t already used.
    --
    -- The reserved ratio is
    -- @(@/@computeReservation@/@\/100)^@/@ActiveFairShares@/@ @ where
    -- @ @/@ActiveFairShares@/@ @ is the number of active fair share
    -- identifiers.
    --
    -- For example, a @computeReservation@ value of 50 indicates that
    -- Batchreserves 50% of the maximum available vCPU if there\'s only one
    -- fair share identifier. It reserves 25% if there are two fair share
    -- identifiers. It reserves 12.5% if there are three fair share
    -- identifiers. A @computeReservation@ value of 25 indicates that Batch
    -- should reserve 25% of the maximum available vCPU if there\'s only one
    -- fair share identifier, 6.25% if there are two fair share identifiers,
    -- and 1.56% if there are three fair share identifiers.
    --
    -- The minimum value is 0 and the maximum value is 99.
    computeReservation :: Prelude.Maybe Prelude.Int,
    -- | The amount of time (in seconds) to use to calculate a fair share
    -- percentage for each fair share identifier in use. A value of zero (0)
    -- indicates that only current usage is measured. The decay allows for more
    -- recently run jobs to have more weight than jobs that ran earlier. The
    -- maximum supported value is 604800 (1 week).
    shareDecaySeconds :: Prelude.Maybe Prelude.Int,
    -- | An array of @SharedIdentifier@ objects that contain the weights for the
    -- fair share identifiers for the fair share policy. Fair share identifiers
    -- that aren\'t included have a default weight of @1.0@.
    shareDistribution :: Prelude.Maybe [ShareAttributes]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FairsharePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computeReservation', 'fairsharePolicy_computeReservation' - A value used to reserve some of the available maximum vCPU for fair
-- share identifiers that aren\'t already used.
--
-- The reserved ratio is
-- @(@/@computeReservation@/@\/100)^@/@ActiveFairShares@/@ @ where
-- @ @/@ActiveFairShares@/@ @ is the number of active fair share
-- identifiers.
--
-- For example, a @computeReservation@ value of 50 indicates that
-- Batchreserves 50% of the maximum available vCPU if there\'s only one
-- fair share identifier. It reserves 25% if there are two fair share
-- identifiers. It reserves 12.5% if there are three fair share
-- identifiers. A @computeReservation@ value of 25 indicates that Batch
-- should reserve 25% of the maximum available vCPU if there\'s only one
-- fair share identifier, 6.25% if there are two fair share identifiers,
-- and 1.56% if there are three fair share identifiers.
--
-- The minimum value is 0 and the maximum value is 99.
--
-- 'shareDecaySeconds', 'fairsharePolicy_shareDecaySeconds' - The amount of time (in seconds) to use to calculate a fair share
-- percentage for each fair share identifier in use. A value of zero (0)
-- indicates that only current usage is measured. The decay allows for more
-- recently run jobs to have more weight than jobs that ran earlier. The
-- maximum supported value is 604800 (1 week).
--
-- 'shareDistribution', 'fairsharePolicy_shareDistribution' - An array of @SharedIdentifier@ objects that contain the weights for the
-- fair share identifiers for the fair share policy. Fair share identifiers
-- that aren\'t included have a default weight of @1.0@.
newFairsharePolicy ::
  FairsharePolicy
newFairsharePolicy =
  FairsharePolicy'
    { computeReservation =
        Prelude.Nothing,
      shareDecaySeconds = Prelude.Nothing,
      shareDistribution = Prelude.Nothing
    }

-- | A value used to reserve some of the available maximum vCPU for fair
-- share identifiers that aren\'t already used.
--
-- The reserved ratio is
-- @(@/@computeReservation@/@\/100)^@/@ActiveFairShares@/@ @ where
-- @ @/@ActiveFairShares@/@ @ is the number of active fair share
-- identifiers.
--
-- For example, a @computeReservation@ value of 50 indicates that
-- Batchreserves 50% of the maximum available vCPU if there\'s only one
-- fair share identifier. It reserves 25% if there are two fair share
-- identifiers. It reserves 12.5% if there are three fair share
-- identifiers. A @computeReservation@ value of 25 indicates that Batch
-- should reserve 25% of the maximum available vCPU if there\'s only one
-- fair share identifier, 6.25% if there are two fair share identifiers,
-- and 1.56% if there are three fair share identifiers.
--
-- The minimum value is 0 and the maximum value is 99.
fairsharePolicy_computeReservation :: Lens.Lens' FairsharePolicy (Prelude.Maybe Prelude.Int)
fairsharePolicy_computeReservation = Lens.lens (\FairsharePolicy' {computeReservation} -> computeReservation) (\s@FairsharePolicy' {} a -> s {computeReservation = a} :: FairsharePolicy)

-- | The amount of time (in seconds) to use to calculate a fair share
-- percentage for each fair share identifier in use. A value of zero (0)
-- indicates that only current usage is measured. The decay allows for more
-- recently run jobs to have more weight than jobs that ran earlier. The
-- maximum supported value is 604800 (1 week).
fairsharePolicy_shareDecaySeconds :: Lens.Lens' FairsharePolicy (Prelude.Maybe Prelude.Int)
fairsharePolicy_shareDecaySeconds = Lens.lens (\FairsharePolicy' {shareDecaySeconds} -> shareDecaySeconds) (\s@FairsharePolicy' {} a -> s {shareDecaySeconds = a} :: FairsharePolicy)

-- | An array of @SharedIdentifier@ objects that contain the weights for the
-- fair share identifiers for the fair share policy. Fair share identifiers
-- that aren\'t included have a default weight of @1.0@.
fairsharePolicy_shareDistribution :: Lens.Lens' FairsharePolicy (Prelude.Maybe [ShareAttributes])
fairsharePolicy_shareDistribution = Lens.lens (\FairsharePolicy' {shareDistribution} -> shareDistribution) (\s@FairsharePolicy' {} a -> s {shareDistribution = a} :: FairsharePolicy) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON FairsharePolicy where
  parseJSON =
    Data.withObject
      "FairsharePolicy"
      ( \x ->
          FairsharePolicy'
            Prelude.<$> (x Data..:? "computeReservation")
            Prelude.<*> (x Data..:? "shareDecaySeconds")
            Prelude.<*> ( x
                            Data..:? "shareDistribution"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable FairsharePolicy where
  hashWithSalt _salt FairsharePolicy' {..} =
    _salt
      `Prelude.hashWithSalt` computeReservation
      `Prelude.hashWithSalt` shareDecaySeconds
      `Prelude.hashWithSalt` shareDistribution

instance Prelude.NFData FairsharePolicy where
  rnf FairsharePolicy' {..} =
    Prelude.rnf computeReservation
      `Prelude.seq` Prelude.rnf shareDecaySeconds
      `Prelude.seq` Prelude.rnf shareDistribution

instance Data.ToJSON FairsharePolicy where
  toJSON FairsharePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("computeReservation" Data..=)
              Prelude.<$> computeReservation,
            ("shareDecaySeconds" Data..=)
              Prelude.<$> shareDecaySeconds,
            ("shareDistribution" Data..=)
              Prelude.<$> shareDistribution
          ]
      )
