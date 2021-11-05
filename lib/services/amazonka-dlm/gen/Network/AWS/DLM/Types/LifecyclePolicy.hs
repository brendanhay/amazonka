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
-- Module      : Network.AWS.DLM.Types.LifecyclePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DLM.Types.LifecyclePolicy where

import qualified Network.AWS.Core as Core
import Network.AWS.DLM.Types.GettablePolicyStateValues
import Network.AWS.DLM.Types.PolicyDetails
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Detailed information about a lifecycle policy.
--
-- /See:/ 'newLifecyclePolicy' smart constructor.
data LifecyclePolicy = LifecyclePolicy'
  { -- | The activation state of the lifecycle policy.
    state :: Prelude.Maybe GettablePolicyStateValues,
    -- | The configuration of the lifecycle policy
    policyDetails :: Prelude.Maybe PolicyDetails,
    -- | The identifier of the lifecycle policy.
    policyId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role used to run the
    -- operations specified by the lifecycle policy.
    executionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The local date and time when the lifecycle policy was created.
    dateCreated :: Prelude.Maybe Core.POSIX,
    -- | The description of the status.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The local date and time when the lifecycle policy was last modified.
    dateModified :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the policy.
    policyArn :: Prelude.Maybe Prelude.Text,
    -- | The description of the lifecycle policy.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LifecyclePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'lifecyclePolicy_state' - The activation state of the lifecycle policy.
--
-- 'policyDetails', 'lifecyclePolicy_policyDetails' - The configuration of the lifecycle policy
--
-- 'policyId', 'lifecyclePolicy_policyId' - The identifier of the lifecycle policy.
--
-- 'executionRoleArn', 'lifecyclePolicy_executionRoleArn' - The Amazon Resource Name (ARN) of the IAM role used to run the
-- operations specified by the lifecycle policy.
--
-- 'dateCreated', 'lifecyclePolicy_dateCreated' - The local date and time when the lifecycle policy was created.
--
-- 'statusMessage', 'lifecyclePolicy_statusMessage' - The description of the status.
--
-- 'dateModified', 'lifecyclePolicy_dateModified' - The local date and time when the lifecycle policy was last modified.
--
-- 'policyArn', 'lifecyclePolicy_policyArn' - The Amazon Resource Name (ARN) of the policy.
--
-- 'description', 'lifecyclePolicy_description' - The description of the lifecycle policy.
--
-- 'tags', 'lifecyclePolicy_tags' - The tags.
newLifecyclePolicy ::
  LifecyclePolicy
newLifecyclePolicy =
  LifecyclePolicy'
    { state = Prelude.Nothing,
      policyDetails = Prelude.Nothing,
      policyId = Prelude.Nothing,
      executionRoleArn = Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      dateModified = Prelude.Nothing,
      policyArn = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The activation state of the lifecycle policy.
lifecyclePolicy_state :: Lens.Lens' LifecyclePolicy (Prelude.Maybe GettablePolicyStateValues)
lifecyclePolicy_state = Lens.lens (\LifecyclePolicy' {state} -> state) (\s@LifecyclePolicy' {} a -> s {state = a} :: LifecyclePolicy)

-- | The configuration of the lifecycle policy
lifecyclePolicy_policyDetails :: Lens.Lens' LifecyclePolicy (Prelude.Maybe PolicyDetails)
lifecyclePolicy_policyDetails = Lens.lens (\LifecyclePolicy' {policyDetails} -> policyDetails) (\s@LifecyclePolicy' {} a -> s {policyDetails = a} :: LifecyclePolicy)

-- | The identifier of the lifecycle policy.
lifecyclePolicy_policyId :: Lens.Lens' LifecyclePolicy (Prelude.Maybe Prelude.Text)
lifecyclePolicy_policyId = Lens.lens (\LifecyclePolicy' {policyId} -> policyId) (\s@LifecyclePolicy' {} a -> s {policyId = a} :: LifecyclePolicy)

-- | The Amazon Resource Name (ARN) of the IAM role used to run the
-- operations specified by the lifecycle policy.
lifecyclePolicy_executionRoleArn :: Lens.Lens' LifecyclePolicy (Prelude.Maybe Prelude.Text)
lifecyclePolicy_executionRoleArn = Lens.lens (\LifecyclePolicy' {executionRoleArn} -> executionRoleArn) (\s@LifecyclePolicy' {} a -> s {executionRoleArn = a} :: LifecyclePolicy)

-- | The local date and time when the lifecycle policy was created.
lifecyclePolicy_dateCreated :: Lens.Lens' LifecyclePolicy (Prelude.Maybe Prelude.UTCTime)
lifecyclePolicy_dateCreated = Lens.lens (\LifecyclePolicy' {dateCreated} -> dateCreated) (\s@LifecyclePolicy' {} a -> s {dateCreated = a} :: LifecyclePolicy) Prelude.. Lens.mapping Core._Time

-- | The description of the status.
lifecyclePolicy_statusMessage :: Lens.Lens' LifecyclePolicy (Prelude.Maybe Prelude.Text)
lifecyclePolicy_statusMessage = Lens.lens (\LifecyclePolicy' {statusMessage} -> statusMessage) (\s@LifecyclePolicy' {} a -> s {statusMessage = a} :: LifecyclePolicy)

-- | The local date and time when the lifecycle policy was last modified.
lifecyclePolicy_dateModified :: Lens.Lens' LifecyclePolicy (Prelude.Maybe Prelude.UTCTime)
lifecyclePolicy_dateModified = Lens.lens (\LifecyclePolicy' {dateModified} -> dateModified) (\s@LifecyclePolicy' {} a -> s {dateModified = a} :: LifecyclePolicy) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the policy.
lifecyclePolicy_policyArn :: Lens.Lens' LifecyclePolicy (Prelude.Maybe Prelude.Text)
lifecyclePolicy_policyArn = Lens.lens (\LifecyclePolicy' {policyArn} -> policyArn) (\s@LifecyclePolicy' {} a -> s {policyArn = a} :: LifecyclePolicy)

-- | The description of the lifecycle policy.
lifecyclePolicy_description :: Lens.Lens' LifecyclePolicy (Prelude.Maybe Prelude.Text)
lifecyclePolicy_description = Lens.lens (\LifecyclePolicy' {description} -> description) (\s@LifecyclePolicy' {} a -> s {description = a} :: LifecyclePolicy)

-- | The tags.
lifecyclePolicy_tags :: Lens.Lens' LifecyclePolicy (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
lifecyclePolicy_tags = Lens.lens (\LifecyclePolicy' {tags} -> tags) (\s@LifecyclePolicy' {} a -> s {tags = a} :: LifecyclePolicy) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON LifecyclePolicy where
  parseJSON =
    Core.withObject
      "LifecyclePolicy"
      ( \x ->
          LifecyclePolicy'
            Prelude.<$> (x Core..:? "State")
            Prelude.<*> (x Core..:? "PolicyDetails")
            Prelude.<*> (x Core..:? "PolicyId")
            Prelude.<*> (x Core..:? "ExecutionRoleArn")
            Prelude.<*> (x Core..:? "DateCreated")
            Prelude.<*> (x Core..:? "StatusMessage")
            Prelude.<*> (x Core..:? "DateModified")
            Prelude.<*> (x Core..:? "PolicyArn")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable LifecyclePolicy

instance Prelude.NFData LifecyclePolicy
