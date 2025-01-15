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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.ResiliencyPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { -- | The timestamp for when the resiliency policy was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | Specifies a high-level geographical location constraint for where your
    -- resilience policy data can be stored.
    dataLocationConstraint :: Prelude.Maybe DataLocationConstraint,
    -- | Specifies the estimated cost tier of the resiliency policy.
    estimatedCostTier :: Prelude.Maybe EstimatedCostTier,
    -- | The resiliency policy.
    policy :: Prelude.Maybe (Prelude.HashMap DisruptionType FailurePolicy),
    -- | The Amazon Resource Name (ARN) of the resiliency policy. The format for
    -- this ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:resiliency-policy\/@policy-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    policyArn :: Prelude.Maybe Prelude.Text,
    -- | The description for the policy.
    policyDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the policy
    policyName :: Prelude.Maybe Prelude.Text,
    -- | The tags assigned to the resource. A tag is a label that you assign to
    -- an Amazon Web Services resource. Each tag consists of a key\/value pair.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The tier for this resiliency policy, ranging from the highest severity
    -- (@MissionCritical@) to lowest (@NonCritical@).
    tier :: Prelude.Maybe ResiliencyPolicyTier
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
-- 'creationTime', 'resiliencyPolicy_creationTime' - The timestamp for when the resiliency policy was created.
--
-- 'dataLocationConstraint', 'resiliencyPolicy_dataLocationConstraint' - Specifies a high-level geographical location constraint for where your
-- resilience policy data can be stored.
--
-- 'estimatedCostTier', 'resiliencyPolicy_estimatedCostTier' - Specifies the estimated cost tier of the resiliency policy.
--
-- 'policy', 'resiliencyPolicy_policy' - The resiliency policy.
--
-- 'policyArn', 'resiliencyPolicy_policyArn' - The Amazon Resource Name (ARN) of the resiliency policy. The format for
-- this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:resiliency-policy\/@policy-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'policyDescription', 'resiliencyPolicy_policyDescription' - The description for the policy.
--
-- 'policyName', 'resiliencyPolicy_policyName' - The name of the policy
--
-- 'tags', 'resiliencyPolicy_tags' - The tags assigned to the resource. A tag is a label that you assign to
-- an Amazon Web Services resource. Each tag consists of a key\/value pair.
--
-- 'tier', 'resiliencyPolicy_tier' - The tier for this resiliency policy, ranging from the highest severity
-- (@MissionCritical@) to lowest (@NonCritical@).
newResiliencyPolicy ::
  ResiliencyPolicy
newResiliencyPolicy =
  ResiliencyPolicy'
    { creationTime = Prelude.Nothing,
      dataLocationConstraint = Prelude.Nothing,
      estimatedCostTier = Prelude.Nothing,
      policy = Prelude.Nothing,
      policyArn = Prelude.Nothing,
      policyDescription = Prelude.Nothing,
      policyName = Prelude.Nothing,
      tags = Prelude.Nothing,
      tier = Prelude.Nothing
    }

-- | The timestamp for when the resiliency policy was created.
resiliencyPolicy_creationTime :: Lens.Lens' ResiliencyPolicy (Prelude.Maybe Prelude.UTCTime)
resiliencyPolicy_creationTime = Lens.lens (\ResiliencyPolicy' {creationTime} -> creationTime) (\s@ResiliencyPolicy' {} a -> s {creationTime = a} :: ResiliencyPolicy) Prelude.. Lens.mapping Data._Time

-- | Specifies a high-level geographical location constraint for where your
-- resilience policy data can be stored.
resiliencyPolicy_dataLocationConstraint :: Lens.Lens' ResiliencyPolicy (Prelude.Maybe DataLocationConstraint)
resiliencyPolicy_dataLocationConstraint = Lens.lens (\ResiliencyPolicy' {dataLocationConstraint} -> dataLocationConstraint) (\s@ResiliencyPolicy' {} a -> s {dataLocationConstraint = a} :: ResiliencyPolicy)

-- | Specifies the estimated cost tier of the resiliency policy.
resiliencyPolicy_estimatedCostTier :: Lens.Lens' ResiliencyPolicy (Prelude.Maybe EstimatedCostTier)
resiliencyPolicy_estimatedCostTier = Lens.lens (\ResiliencyPolicy' {estimatedCostTier} -> estimatedCostTier) (\s@ResiliencyPolicy' {} a -> s {estimatedCostTier = a} :: ResiliencyPolicy)

-- | The resiliency policy.
resiliencyPolicy_policy :: Lens.Lens' ResiliencyPolicy (Prelude.Maybe (Prelude.HashMap DisruptionType FailurePolicy))
resiliencyPolicy_policy = Lens.lens (\ResiliencyPolicy' {policy} -> policy) (\s@ResiliencyPolicy' {} a -> s {policy = a} :: ResiliencyPolicy) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the resiliency policy. The format for
-- this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:resiliency-policy\/@policy-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
resiliencyPolicy_policyArn :: Lens.Lens' ResiliencyPolicy (Prelude.Maybe Prelude.Text)
resiliencyPolicy_policyArn = Lens.lens (\ResiliencyPolicy' {policyArn} -> policyArn) (\s@ResiliencyPolicy' {} a -> s {policyArn = a} :: ResiliencyPolicy)

-- | The description for the policy.
resiliencyPolicy_policyDescription :: Lens.Lens' ResiliencyPolicy (Prelude.Maybe Prelude.Text)
resiliencyPolicy_policyDescription = Lens.lens (\ResiliencyPolicy' {policyDescription} -> policyDescription) (\s@ResiliencyPolicy' {} a -> s {policyDescription = a} :: ResiliencyPolicy)

-- | The name of the policy
resiliencyPolicy_policyName :: Lens.Lens' ResiliencyPolicy (Prelude.Maybe Prelude.Text)
resiliencyPolicy_policyName = Lens.lens (\ResiliencyPolicy' {policyName} -> policyName) (\s@ResiliencyPolicy' {} a -> s {policyName = a} :: ResiliencyPolicy)

-- | The tags assigned to the resource. A tag is a label that you assign to
-- an Amazon Web Services resource. Each tag consists of a key\/value pair.
resiliencyPolicy_tags :: Lens.Lens' ResiliencyPolicy (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
resiliencyPolicy_tags = Lens.lens (\ResiliencyPolicy' {tags} -> tags) (\s@ResiliencyPolicy' {} a -> s {tags = a} :: ResiliencyPolicy) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The tier for this resiliency policy, ranging from the highest severity
-- (@MissionCritical@) to lowest (@NonCritical@).
resiliencyPolicy_tier :: Lens.Lens' ResiliencyPolicy (Prelude.Maybe ResiliencyPolicyTier)
resiliencyPolicy_tier = Lens.lens (\ResiliencyPolicy' {tier} -> tier) (\s@ResiliencyPolicy' {} a -> s {tier = a} :: ResiliencyPolicy)

instance Data.FromJSON ResiliencyPolicy where
  parseJSON =
    Data.withObject
      "ResiliencyPolicy"
      ( \x ->
          ResiliencyPolicy'
            Prelude.<$> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "dataLocationConstraint")
            Prelude.<*> (x Data..:? "estimatedCostTier")
            Prelude.<*> (x Data..:? "policy" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "policyArn")
            Prelude.<*> (x Data..:? "policyDescription")
            Prelude.<*> (x Data..:? "policyName")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "tier")
      )

instance Prelude.Hashable ResiliencyPolicy where
  hashWithSalt _salt ResiliencyPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` dataLocationConstraint
      `Prelude.hashWithSalt` estimatedCostTier
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` policyArn
      `Prelude.hashWithSalt` policyDescription
      `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` tier

instance Prelude.NFData ResiliencyPolicy where
  rnf ResiliencyPolicy' {..} =
    Prelude.rnf creationTime `Prelude.seq`
      Prelude.rnf dataLocationConstraint `Prelude.seq`
        Prelude.rnf estimatedCostTier `Prelude.seq`
          Prelude.rnf policy `Prelude.seq`
            Prelude.rnf policyArn `Prelude.seq`
              Prelude.rnf policyDescription `Prelude.seq`
                Prelude.rnf policyName `Prelude.seq`
                  Prelude.rnf tags `Prelude.seq`
                    Prelude.rnf tier
