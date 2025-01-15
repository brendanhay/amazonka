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
-- Module      : Amazonka.MechanicalTurk.Types.ReviewPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MechanicalTurk.Types.ReviewPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MechanicalTurk.Types.PolicyParameter
import qualified Amazonka.Prelude as Prelude

-- | HIT Review Policy data structures represent HIT review policies, which
-- you specify when you create a HIT.
--
-- /See:/ 'newReviewPolicy' smart constructor.
data ReviewPolicy = ReviewPolicy'
  { -- | Name of the parameter from the Review policy.
    parameters :: Prelude.Maybe [PolicyParameter],
    -- | Name of a Review Policy: SimplePlurality\/2011-09-01 or
    -- ScoreMyKnownAnswers\/2011-09-01
    policyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReviewPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameters', 'reviewPolicy_parameters' - Name of the parameter from the Review policy.
--
-- 'policyName', 'reviewPolicy_policyName' - Name of a Review Policy: SimplePlurality\/2011-09-01 or
-- ScoreMyKnownAnswers\/2011-09-01
newReviewPolicy ::
  -- | 'policyName'
  Prelude.Text ->
  ReviewPolicy
newReviewPolicy pPolicyName_ =
  ReviewPolicy'
    { parameters = Prelude.Nothing,
      policyName = pPolicyName_
    }

-- | Name of the parameter from the Review policy.
reviewPolicy_parameters :: Lens.Lens' ReviewPolicy (Prelude.Maybe [PolicyParameter])
reviewPolicy_parameters = Lens.lens (\ReviewPolicy' {parameters} -> parameters) (\s@ReviewPolicy' {} a -> s {parameters = a} :: ReviewPolicy) Prelude.. Lens.mapping Lens.coerced

-- | Name of a Review Policy: SimplePlurality\/2011-09-01 or
-- ScoreMyKnownAnswers\/2011-09-01
reviewPolicy_policyName :: Lens.Lens' ReviewPolicy Prelude.Text
reviewPolicy_policyName = Lens.lens (\ReviewPolicy' {policyName} -> policyName) (\s@ReviewPolicy' {} a -> s {policyName = a} :: ReviewPolicy)

instance Data.FromJSON ReviewPolicy where
  parseJSON =
    Data.withObject
      "ReviewPolicy"
      ( \x ->
          ReviewPolicy'
            Prelude.<$> (x Data..:? "Parameters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "PolicyName")
      )

instance Prelude.Hashable ReviewPolicy where
  hashWithSalt _salt ReviewPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` policyName

instance Prelude.NFData ReviewPolicy where
  rnf ReviewPolicy' {..} =
    Prelude.rnf parameters `Prelude.seq`
      Prelude.rnf policyName

instance Data.ToJSON ReviewPolicy where
  toJSON ReviewPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Parameters" Data..=) Prelude.<$> parameters,
            Prelude.Just ("PolicyName" Data..= policyName)
          ]
      )
