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
-- Module      : Amazonka.DLM.Types.LifecyclePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DLM.Types.LifecyclePolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DLM.Types.GettablePolicyStateValues
import Amazonka.DLM.Types.PolicyDetails
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | __[All policy types]__ Detailed information about a snapshot, AMI, or
-- event-based lifecycle policy.
--
-- /See:/ 'newLifecyclePolicy' smart constructor.
data LifecyclePolicy = LifecyclePolicy'
  { -- | The local date and time when the lifecycle policy was created.
    dateCreated :: Prelude.Maybe Data.ISO8601,
    -- | The local date and time when the lifecycle policy was last modified.
    dateModified :: Prelude.Maybe Data.ISO8601,
    -- | The description of the lifecycle policy.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role used to run the
    -- operations specified by the lifecycle policy.
    executionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the policy.
    policyArn :: Prelude.Maybe Prelude.Text,
    -- | The configuration of the lifecycle policy
    policyDetails :: Prelude.Maybe PolicyDetails,
    -- | The identifier of the lifecycle policy.
    policyId :: Prelude.Maybe Prelude.Text,
    -- | The activation state of the lifecycle policy.
    state :: Prelude.Maybe GettablePolicyStateValues,
    -- | The description of the status.
    statusMessage :: Prelude.Maybe Prelude.Text,
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
-- 'dateCreated', 'lifecyclePolicy_dateCreated' - The local date and time when the lifecycle policy was created.
--
-- 'dateModified', 'lifecyclePolicy_dateModified' - The local date and time when the lifecycle policy was last modified.
--
-- 'description', 'lifecyclePolicy_description' - The description of the lifecycle policy.
--
-- 'executionRoleArn', 'lifecyclePolicy_executionRoleArn' - The Amazon Resource Name (ARN) of the IAM role used to run the
-- operations specified by the lifecycle policy.
--
-- 'policyArn', 'lifecyclePolicy_policyArn' - The Amazon Resource Name (ARN) of the policy.
--
-- 'policyDetails', 'lifecyclePolicy_policyDetails' - The configuration of the lifecycle policy
--
-- 'policyId', 'lifecyclePolicy_policyId' - The identifier of the lifecycle policy.
--
-- 'state', 'lifecyclePolicy_state' - The activation state of the lifecycle policy.
--
-- 'statusMessage', 'lifecyclePolicy_statusMessage' - The description of the status.
--
-- 'tags', 'lifecyclePolicy_tags' - The tags.
newLifecyclePolicy ::
  LifecyclePolicy
newLifecyclePolicy =
  LifecyclePolicy'
    { dateCreated = Prelude.Nothing,
      dateModified = Prelude.Nothing,
      description = Prelude.Nothing,
      executionRoleArn = Prelude.Nothing,
      policyArn = Prelude.Nothing,
      policyDetails = Prelude.Nothing,
      policyId = Prelude.Nothing,
      state = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The local date and time when the lifecycle policy was created.
lifecyclePolicy_dateCreated :: Lens.Lens' LifecyclePolicy (Prelude.Maybe Prelude.UTCTime)
lifecyclePolicy_dateCreated = Lens.lens (\LifecyclePolicy' {dateCreated} -> dateCreated) (\s@LifecyclePolicy' {} a -> s {dateCreated = a} :: LifecyclePolicy) Prelude.. Lens.mapping Data._Time

-- | The local date and time when the lifecycle policy was last modified.
lifecyclePolicy_dateModified :: Lens.Lens' LifecyclePolicy (Prelude.Maybe Prelude.UTCTime)
lifecyclePolicy_dateModified = Lens.lens (\LifecyclePolicy' {dateModified} -> dateModified) (\s@LifecyclePolicy' {} a -> s {dateModified = a} :: LifecyclePolicy) Prelude.. Lens.mapping Data._Time

-- | The description of the lifecycle policy.
lifecyclePolicy_description :: Lens.Lens' LifecyclePolicy (Prelude.Maybe Prelude.Text)
lifecyclePolicy_description = Lens.lens (\LifecyclePolicy' {description} -> description) (\s@LifecyclePolicy' {} a -> s {description = a} :: LifecyclePolicy)

-- | The Amazon Resource Name (ARN) of the IAM role used to run the
-- operations specified by the lifecycle policy.
lifecyclePolicy_executionRoleArn :: Lens.Lens' LifecyclePolicy (Prelude.Maybe Prelude.Text)
lifecyclePolicy_executionRoleArn = Lens.lens (\LifecyclePolicy' {executionRoleArn} -> executionRoleArn) (\s@LifecyclePolicy' {} a -> s {executionRoleArn = a} :: LifecyclePolicy)

-- | The Amazon Resource Name (ARN) of the policy.
lifecyclePolicy_policyArn :: Lens.Lens' LifecyclePolicy (Prelude.Maybe Prelude.Text)
lifecyclePolicy_policyArn = Lens.lens (\LifecyclePolicy' {policyArn} -> policyArn) (\s@LifecyclePolicy' {} a -> s {policyArn = a} :: LifecyclePolicy)

-- | The configuration of the lifecycle policy
lifecyclePolicy_policyDetails :: Lens.Lens' LifecyclePolicy (Prelude.Maybe PolicyDetails)
lifecyclePolicy_policyDetails = Lens.lens (\LifecyclePolicy' {policyDetails} -> policyDetails) (\s@LifecyclePolicy' {} a -> s {policyDetails = a} :: LifecyclePolicy)

-- | The identifier of the lifecycle policy.
lifecyclePolicy_policyId :: Lens.Lens' LifecyclePolicy (Prelude.Maybe Prelude.Text)
lifecyclePolicy_policyId = Lens.lens (\LifecyclePolicy' {policyId} -> policyId) (\s@LifecyclePolicy' {} a -> s {policyId = a} :: LifecyclePolicy)

-- | The activation state of the lifecycle policy.
lifecyclePolicy_state :: Lens.Lens' LifecyclePolicy (Prelude.Maybe GettablePolicyStateValues)
lifecyclePolicy_state = Lens.lens (\LifecyclePolicy' {state} -> state) (\s@LifecyclePolicy' {} a -> s {state = a} :: LifecyclePolicy)

-- | The description of the status.
lifecyclePolicy_statusMessage :: Lens.Lens' LifecyclePolicy (Prelude.Maybe Prelude.Text)
lifecyclePolicy_statusMessage = Lens.lens (\LifecyclePolicy' {statusMessage} -> statusMessage) (\s@LifecyclePolicy' {} a -> s {statusMessage = a} :: LifecyclePolicy)

-- | The tags.
lifecyclePolicy_tags :: Lens.Lens' LifecyclePolicy (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
lifecyclePolicy_tags = Lens.lens (\LifecyclePolicy' {tags} -> tags) (\s@LifecyclePolicy' {} a -> s {tags = a} :: LifecyclePolicy) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON LifecyclePolicy where
  parseJSON =
    Data.withObject
      "LifecyclePolicy"
      ( \x ->
          LifecyclePolicy'
            Prelude.<$> (x Data..:? "DateCreated")
            Prelude.<*> (x Data..:? "DateModified")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "ExecutionRoleArn")
            Prelude.<*> (x Data..:? "PolicyArn")
            Prelude.<*> (x Data..:? "PolicyDetails")
            Prelude.<*> (x Data..:? "PolicyId")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "StatusMessage")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable LifecyclePolicy where
  hashWithSalt _salt LifecyclePolicy' {..} =
    _salt `Prelude.hashWithSalt` dateCreated
      `Prelude.hashWithSalt` dateModified
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` executionRoleArn
      `Prelude.hashWithSalt` policyArn
      `Prelude.hashWithSalt` policyDetails
      `Prelude.hashWithSalt` policyId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` tags

instance Prelude.NFData LifecyclePolicy where
  rnf LifecyclePolicy' {..} =
    Prelude.rnf dateCreated
      `Prelude.seq` Prelude.rnf dateModified
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf executionRoleArn
      `Prelude.seq` Prelude.rnf policyArn
      `Prelude.seq` Prelude.rnf policyDetails
      `Prelude.seq` Prelude.rnf policyId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf tags
