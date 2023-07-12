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
-- Module      : Amazonka.BillingConductor.Types.ComputationPreference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.ComputationPreference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The preferences and settings that will be used to compute the Amazon Web
-- Services charges for a billing group.
--
-- /See:/ 'newComputationPreference' smart constructor.
data ComputationPreference = ComputationPreference'
  { -- | The Amazon Resource Name (ARN) of the pricing plan that\'s used to
    -- compute the Amazon Web Services charges for a billing group.
    pricingPlanArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComputationPreference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pricingPlanArn', 'computationPreference_pricingPlanArn' - The Amazon Resource Name (ARN) of the pricing plan that\'s used to
-- compute the Amazon Web Services charges for a billing group.
newComputationPreference ::
  -- | 'pricingPlanArn'
  Prelude.Text ->
  ComputationPreference
newComputationPreference pPricingPlanArn_ =
  ComputationPreference'
    { pricingPlanArn =
        pPricingPlanArn_
    }

-- | The Amazon Resource Name (ARN) of the pricing plan that\'s used to
-- compute the Amazon Web Services charges for a billing group.
computationPreference_pricingPlanArn :: Lens.Lens' ComputationPreference Prelude.Text
computationPreference_pricingPlanArn = Lens.lens (\ComputationPreference' {pricingPlanArn} -> pricingPlanArn) (\s@ComputationPreference' {} a -> s {pricingPlanArn = a} :: ComputationPreference)

instance Data.FromJSON ComputationPreference where
  parseJSON =
    Data.withObject
      "ComputationPreference"
      ( \x ->
          ComputationPreference'
            Prelude.<$> (x Data..: "PricingPlanArn")
      )

instance Prelude.Hashable ComputationPreference where
  hashWithSalt _salt ComputationPreference' {..} =
    _salt `Prelude.hashWithSalt` pricingPlanArn

instance Prelude.NFData ComputationPreference where
  rnf ComputationPreference' {..} =
    Prelude.rnf pricingPlanArn

instance Data.ToJSON ComputationPreference where
  toJSON ComputationPreference' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("PricingPlanArn" Data..= pricingPlanArn)
          ]
      )
