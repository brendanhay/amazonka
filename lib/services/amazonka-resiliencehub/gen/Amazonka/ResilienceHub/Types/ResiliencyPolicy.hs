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
-- Module      : Amazonka.ResilienceHub.Types.ResiliencyPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.ResiliencyPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResilienceHub.Types.DataLocationConstraint
import Amazonka.ResilienceHub.Types.DisruptionType
import Amazonka.ResilienceHub.Types.EstimatedCostTier
import Amazonka.ResilienceHub.Types.FailurePolicy
import Amazonka.ResilienceHub.Types.ResiliencyPolicyTier

-- | Defines a resiliency policy.
--
-- /See:/ 'newResiliencyPolicy' smart constructor.
data ResiliencyPolicy = ResiliencyPolicy'
  { -- | The tags assigned to the resource. A tag is a label that you assign to
    -- an Amazon Web Services resource. Each tag consists of a key\/value pair.
    tags :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The name of the policy
    policyName :: Prelude.Maybe Prelude.Text,
    -- | The resiliency policy.
    policy :: Prelude.Maybe (Prelude.HashMap DisruptionType FailurePolicy),
    -- | Specifies a high-level geographical location constraint for where your
    -- resilience policy data can be stored.
    dataLocationConstraint :: Prelude.Maybe DataLocationConstraint,
    -- | Specifies the estimated cost tier of the resiliency policy.
    estimatedCostTier :: Prelude.Maybe EstimatedCostTier,
    -- | The tier for this resiliency policy, ranging from the highest severity
    -- (@MissionCritical@) to lowest (@NonCritical@).
    tier :: Prelude.Maybe ResiliencyPolicyTier,
    -- | The Amazon Resource Name (ARN) of the resiliency policy. The format for
    -- this ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:resiliency-policy\/@policy-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    policyArn :: Prelude.Maybe Prelude.Text,
    -- | The timestamp for when the resiliency policy was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The description for the policy.
    policyDescription :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResiliencyPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'resiliencyPolicy_tags' - The tags assigned to the resource. A tag is a label that you assign to
-- an Amazon Web Services resource. Each tag consists of a key\/value pair.
--
-- 'policyName', 'resiliencyPolicy_policyName' - The name of the policy
--
-- 'policy', 'resiliencyPolicy_policy' - The resiliency policy.
--
-- 'dataLocationConstraint', 'resiliencyPolicy_dataLocationConstraint' - Specifies a high-level geographical location constraint for where your
-- resilience policy data can be stored.
--
-- 'estimatedCostTier', 'resiliencyPolicy_estimatedCostTier' - Specifies the estimated cost tier of the resiliency policy.
--
-- 'tier', 'resiliencyPolicy_tier' - The tier for this resiliency policy, ranging from the highest severity
-- (@MissionCritical@) to lowest (@NonCritical@).
--
-- 'policyArn', 'resiliencyPolicy_policyArn' - The Amazon Resource Name (ARN) of the resiliency policy. The format for
-- this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:resiliency-policy\/@policy-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'creationTime', 'resiliencyPolicy_creationTime' - The timestamp for when the resiliency policy was created.
--
-- 'policyDescription', 'resiliencyPolicy_policyDescription' - The description for the policy.
newResiliencyPolicy ::
  ResiliencyPolicy
newResiliencyPolicy =
  ResiliencyPolicy'
    { tags = Prelude.Nothing,
      policyName = Prelude.Nothing,
      policy = Prelude.Nothing,
      dataLocationConstraint = Prelude.Nothing,
      estimatedCostTier = Prelude.Nothing,
      tier = Prelude.Nothing,
      policyArn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      policyDescription = Prelude.Nothing
    }

-- | The tags assigned to the resource. A tag is a label that you assign to
-- an Amazon Web Services resource. Each tag consists of a key\/value pair.
resiliencyPolicy_tags :: Lens.Lens' ResiliencyPolicy (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
resiliencyPolicy_tags = Lens.lens (\ResiliencyPolicy' {tags} -> tags) (\s@ResiliencyPolicy' {} a -> s {tags = a} :: ResiliencyPolicy) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | The name of the policy
resiliencyPolicy_policyName :: Lens.Lens' ResiliencyPolicy (Prelude.Maybe Prelude.Text)
resiliencyPolicy_policyName = Lens.lens (\ResiliencyPolicy' {policyName} -> policyName) (\s@ResiliencyPolicy' {} a -> s {policyName = a} :: ResiliencyPolicy)

-- | The resiliency policy.
resiliencyPolicy_policy :: Lens.Lens' ResiliencyPolicy (Prelude.Maybe (Prelude.HashMap DisruptionType FailurePolicy))
resiliencyPolicy_policy = Lens.lens (\ResiliencyPolicy' {policy} -> policy) (\s@ResiliencyPolicy' {} a -> s {policy = a} :: ResiliencyPolicy) Prelude.. Lens.mapping Lens.coerced

-- | Specifies a high-level geographical location constraint for where your
-- resilience policy data can be stored.
resiliencyPolicy_dataLocationConstraint :: Lens.Lens' ResiliencyPolicy (Prelude.Maybe DataLocationConstraint)
resiliencyPolicy_dataLocationConstraint = Lens.lens (\ResiliencyPolicy' {dataLocationConstraint} -> dataLocationConstraint) (\s@ResiliencyPolicy' {} a -> s {dataLocationConstraint = a} :: ResiliencyPolicy)

-- | Specifies the estimated cost tier of the resiliency policy.
resiliencyPolicy_estimatedCostTier :: Lens.Lens' ResiliencyPolicy (Prelude.Maybe EstimatedCostTier)
resiliencyPolicy_estimatedCostTier = Lens.lens (\ResiliencyPolicy' {estimatedCostTier} -> estimatedCostTier) (\s@ResiliencyPolicy' {} a -> s {estimatedCostTier = a} :: ResiliencyPolicy)

-- | The tier for this resiliency policy, ranging from the highest severity
-- (@MissionCritical@) to lowest (@NonCritical@).
resiliencyPolicy_tier :: Lens.Lens' ResiliencyPolicy (Prelude.Maybe ResiliencyPolicyTier)
resiliencyPolicy_tier = Lens.lens (\ResiliencyPolicy' {tier} -> tier) (\s@ResiliencyPolicy' {} a -> s {tier = a} :: ResiliencyPolicy)

-- | The Amazon Resource Name (ARN) of the resiliency policy. The format for
-- this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:resiliency-policy\/@policy-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
resiliencyPolicy_policyArn :: Lens.Lens' ResiliencyPolicy (Prelude.Maybe Prelude.Text)
resiliencyPolicy_policyArn = Lens.lens (\ResiliencyPolicy' {policyArn} -> policyArn) (\s@ResiliencyPolicy' {} a -> s {policyArn = a} :: ResiliencyPolicy)

-- | The timestamp for when the resiliency policy was created.
resiliencyPolicy_creationTime :: Lens.Lens' ResiliencyPolicy (Prelude.Maybe Prelude.UTCTime)
resiliencyPolicy_creationTime = Lens.lens (\ResiliencyPolicy' {creationTime} -> creationTime) (\s@ResiliencyPolicy' {} a -> s {creationTime = a} :: ResiliencyPolicy) Prelude.. Lens.mapping Core._Time

-- | The description for the policy.
resiliencyPolicy_policyDescription :: Lens.Lens' ResiliencyPolicy (Prelude.Maybe Prelude.Text)
resiliencyPolicy_policyDescription = Lens.lens (\ResiliencyPolicy' {policyDescription} -> policyDescription) (\s@ResiliencyPolicy' {} a -> s {policyDescription = a} :: ResiliencyPolicy)

instance Core.FromJSON ResiliencyPolicy where
  parseJSON =
    Core.withObject
      "ResiliencyPolicy"
      ( \x ->
          ResiliencyPolicy'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "policyName")
            Prelude.<*> (x Core..:? "policy" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "dataLocationConstraint")
            Prelude.<*> (x Core..:? "estimatedCostTier")
            Prelude.<*> (x Core..:? "tier")
            Prelude.<*> (x Core..:? "policyArn")
            Prelude.<*> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "policyDescription")
      )

instance Prelude.Hashable ResiliencyPolicy where
  hashWithSalt _salt ResiliencyPolicy' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` dataLocationConstraint
      `Prelude.hashWithSalt` estimatedCostTier
      `Prelude.hashWithSalt` tier
      `Prelude.hashWithSalt` policyArn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` policyDescription

instance Prelude.NFData ResiliencyPolicy where
  rnf ResiliencyPolicy' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf policy
      `Prelude.seq` Prelude.rnf dataLocationConstraint
      `Prelude.seq` Prelude.rnf estimatedCostTier
      `Prelude.seq` Prelude.rnf tier
      `Prelude.seq` Prelude.rnf policyArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf policyDescription
