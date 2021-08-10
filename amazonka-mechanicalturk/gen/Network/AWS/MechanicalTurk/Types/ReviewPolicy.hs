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
-- Module      : Network.AWS.MechanicalTurk.Types.ReviewPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.ReviewPolicy where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types.PolicyParameter
import qualified Network.AWS.Prelude as Prelude

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
reviewPolicy_parameters = Lens.lens (\ReviewPolicy' {parameters} -> parameters) (\s@ReviewPolicy' {} a -> s {parameters = a} :: ReviewPolicy) Prelude.. Lens.mapping Lens._Coerce

-- | Name of a Review Policy: SimplePlurality\/2011-09-01 or
-- ScoreMyKnownAnswers\/2011-09-01
reviewPolicy_policyName :: Lens.Lens' ReviewPolicy Prelude.Text
reviewPolicy_policyName = Lens.lens (\ReviewPolicy' {policyName} -> policyName) (\s@ReviewPolicy' {} a -> s {policyName = a} :: ReviewPolicy)

instance Core.FromJSON ReviewPolicy where
  parseJSON =
    Core.withObject
      "ReviewPolicy"
      ( \x ->
          ReviewPolicy'
            Prelude.<$> (x Core..:? "Parameters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "PolicyName")
      )

instance Prelude.Hashable ReviewPolicy

instance Prelude.NFData ReviewPolicy

instance Core.ToJSON ReviewPolicy where
  toJSON ReviewPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Parameters" Core..=) Prelude.<$> parameters,
            Prelude.Just ("PolicyName" Core..= policyName)
          ]
      )
