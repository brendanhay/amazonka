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
-- Module      : Amazonka.DLM.Types.LifecyclePolicySummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DLM.Types.LifecyclePolicySummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DLM.Types.GettablePolicyStateValues
import Amazonka.DLM.Types.PolicyTypeValues
import qualified Amazonka.Prelude as Prelude

-- | Summary information about a lifecycle policy.
--
-- /See:/ 'newLifecyclePolicySummary' smart constructor.
data LifecyclePolicySummary = LifecyclePolicySummary'
  { -- | The tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The identifier of the lifecycle policy.
    policyId :: Prelude.Maybe Prelude.Text,
    -- | The type of policy. @EBS_SNAPSHOT_MANAGEMENT@ indicates that the policy
    -- manages the lifecycle of Amazon EBS snapshots. @IMAGE_MANAGEMENT@
    -- indicates that the policy manages the lifecycle of EBS-backed AMIs.
    -- @EVENT_BASED_POLICY@ indicates that the policy automates cross-account
    -- snapshot copies for snapshots that are shared with your account.
    policyType :: Prelude.Maybe PolicyTypeValues,
    -- | The activation state of the lifecycle policy.
    state :: Prelude.Maybe GettablePolicyStateValues,
    -- | The description of the lifecycle policy.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LifecyclePolicySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'lifecyclePolicySummary_tags' - The tags.
--
-- 'policyId', 'lifecyclePolicySummary_policyId' - The identifier of the lifecycle policy.
--
-- 'policyType', 'lifecyclePolicySummary_policyType' - The type of policy. @EBS_SNAPSHOT_MANAGEMENT@ indicates that the policy
-- manages the lifecycle of Amazon EBS snapshots. @IMAGE_MANAGEMENT@
-- indicates that the policy manages the lifecycle of EBS-backed AMIs.
-- @EVENT_BASED_POLICY@ indicates that the policy automates cross-account
-- snapshot copies for snapshots that are shared with your account.
--
-- 'state', 'lifecyclePolicySummary_state' - The activation state of the lifecycle policy.
--
-- 'description', 'lifecyclePolicySummary_description' - The description of the lifecycle policy.
newLifecyclePolicySummary ::
  LifecyclePolicySummary
newLifecyclePolicySummary =
  LifecyclePolicySummary'
    { tags = Prelude.Nothing,
      policyId = Prelude.Nothing,
      policyType = Prelude.Nothing,
      state = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The tags.
lifecyclePolicySummary_tags :: Lens.Lens' LifecyclePolicySummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
lifecyclePolicySummary_tags = Lens.lens (\LifecyclePolicySummary' {tags} -> tags) (\s@LifecyclePolicySummary' {} a -> s {tags = a} :: LifecyclePolicySummary) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the lifecycle policy.
lifecyclePolicySummary_policyId :: Lens.Lens' LifecyclePolicySummary (Prelude.Maybe Prelude.Text)
lifecyclePolicySummary_policyId = Lens.lens (\LifecyclePolicySummary' {policyId} -> policyId) (\s@LifecyclePolicySummary' {} a -> s {policyId = a} :: LifecyclePolicySummary)

-- | The type of policy. @EBS_SNAPSHOT_MANAGEMENT@ indicates that the policy
-- manages the lifecycle of Amazon EBS snapshots. @IMAGE_MANAGEMENT@
-- indicates that the policy manages the lifecycle of EBS-backed AMIs.
-- @EVENT_BASED_POLICY@ indicates that the policy automates cross-account
-- snapshot copies for snapshots that are shared with your account.
lifecyclePolicySummary_policyType :: Lens.Lens' LifecyclePolicySummary (Prelude.Maybe PolicyTypeValues)
lifecyclePolicySummary_policyType = Lens.lens (\LifecyclePolicySummary' {policyType} -> policyType) (\s@LifecyclePolicySummary' {} a -> s {policyType = a} :: LifecyclePolicySummary)

-- | The activation state of the lifecycle policy.
lifecyclePolicySummary_state :: Lens.Lens' LifecyclePolicySummary (Prelude.Maybe GettablePolicyStateValues)
lifecyclePolicySummary_state = Lens.lens (\LifecyclePolicySummary' {state} -> state) (\s@LifecyclePolicySummary' {} a -> s {state = a} :: LifecyclePolicySummary)

-- | The description of the lifecycle policy.
lifecyclePolicySummary_description :: Lens.Lens' LifecyclePolicySummary (Prelude.Maybe Prelude.Text)
lifecyclePolicySummary_description = Lens.lens (\LifecyclePolicySummary' {description} -> description) (\s@LifecyclePolicySummary' {} a -> s {description = a} :: LifecyclePolicySummary)

instance Core.FromJSON LifecyclePolicySummary where
  parseJSON =
    Core.withObject
      "LifecyclePolicySummary"
      ( \x ->
          LifecyclePolicySummary'
            Prelude.<$> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "PolicyId")
            Prelude.<*> (x Core..:? "PolicyType")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable LifecyclePolicySummary where
  hashWithSalt _salt LifecyclePolicySummary' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` policyId
      `Prelude.hashWithSalt` policyType
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` description

instance Prelude.NFData LifecyclePolicySummary where
  rnf LifecyclePolicySummary' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf policyId
      `Prelude.seq` Prelude.rnf policyType
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf description
