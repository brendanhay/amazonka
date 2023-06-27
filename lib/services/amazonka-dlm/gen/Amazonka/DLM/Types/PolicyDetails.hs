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
-- Module      : Amazonka.DLM.Types.PolicyDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DLM.Types.PolicyDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DLM.Types.Action
import Amazonka.DLM.Types.EventSource
import Amazonka.DLM.Types.Parameters
import Amazonka.DLM.Types.PolicyTypeValues
import Amazonka.DLM.Types.ResourceLocationValues
import Amazonka.DLM.Types.ResourceTypeValues
import Amazonka.DLM.Types.Schedule
import Amazonka.DLM.Types.Tag
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | __[All policy types]__ Specifies the configuration of a lifecycle
-- policy.
--
-- /See:/ 'newPolicyDetails' smart constructor.
data PolicyDetails = PolicyDetails'
  { -- | __[Event-based policies only]__ The actions to be performed when the
    -- event-based policy is activated. You can specify only one action per
    -- policy.
    actions :: Prelude.Maybe (Prelude.NonEmpty Action),
    -- | __[Event-based policies only]__ The event that activates the event-based
    -- policy.
    eventSource :: Prelude.Maybe EventSource,
    -- | __[Snapshot and AMI policies only]__ A set of optional parameters for
    -- snapshot and AMI lifecycle policies.
    --
    -- If you are modifying a policy that was created or previously modified
    -- using the Amazon Data Lifecycle Manager console, then you must include
    -- this parameter and specify either the default values or the new values
    -- that you require. You can\'t omit this parameter or set its values to
    -- null.
    parameters :: Prelude.Maybe Parameters,
    -- | __[All policy types]__ The valid target resource types and actions a
    -- policy can manage. Specify @EBS_SNAPSHOT_MANAGEMENT@ to create a
    -- lifecycle policy that manages the lifecycle of Amazon EBS snapshots.
    -- Specify @IMAGE_MANAGEMENT@ to create a lifecycle policy that manages the
    -- lifecycle of EBS-backed AMIs. Specify @EVENT_BASED_POLICY @ to create an
    -- event-based policy that performs specific actions when a defined event
    -- occurs in your Amazon Web Services account.
    --
    -- The default is @EBS_SNAPSHOT_MANAGEMENT@.
    policyType :: Prelude.Maybe PolicyTypeValues,
    -- | __[Snapshot and AMI policies only]__ The location of the resources to
    -- backup. If the source resources are located in an Amazon Web Services
    -- Region, specify @CLOUD@. If the source resources are located on an
    -- Outpost in your account, specify @OUTPOST@.
    --
    -- If you specify @OUTPOST@, Amazon Data Lifecycle Manager backs up all
    -- resources of the specified type with matching target tags across all of
    -- the Outposts in your account.
    resourceLocations :: Prelude.Maybe (Prelude.NonEmpty ResourceLocationValues),
    -- | __[Snapshot policies only]__ The target resource type for snapshot and
    -- AMI lifecycle policies. Use @VOLUME @to create snapshots of individual
    -- volumes or use @INSTANCE@ to create multi-volume snapshots from the
    -- volumes for an instance.
    resourceTypes :: Prelude.Maybe (Prelude.NonEmpty ResourceTypeValues),
    -- | __[Snapshot and AMI policies only]__ The schedules of policy-defined
    -- actions for snapshot and AMI lifecycle policies. A policy can have up to
    -- four schedules—one mandatory schedule and up to three optional
    -- schedules.
    schedules :: Prelude.Maybe (Prelude.NonEmpty Schedule),
    -- | __[Snapshot and AMI policies only]__ The single tag that identifies
    -- targeted resources for this policy.
    targetTags :: Prelude.Maybe (Prelude.NonEmpty Tag)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'policyDetails_actions' - __[Event-based policies only]__ The actions to be performed when the
-- event-based policy is activated. You can specify only one action per
-- policy.
--
-- 'eventSource', 'policyDetails_eventSource' - __[Event-based policies only]__ The event that activates the event-based
-- policy.
--
-- 'parameters', 'policyDetails_parameters' - __[Snapshot and AMI policies only]__ A set of optional parameters for
-- snapshot and AMI lifecycle policies.
--
-- If you are modifying a policy that was created or previously modified
-- using the Amazon Data Lifecycle Manager console, then you must include
-- this parameter and specify either the default values or the new values
-- that you require. You can\'t omit this parameter or set its values to
-- null.
--
-- 'policyType', 'policyDetails_policyType' - __[All policy types]__ The valid target resource types and actions a
-- policy can manage. Specify @EBS_SNAPSHOT_MANAGEMENT@ to create a
-- lifecycle policy that manages the lifecycle of Amazon EBS snapshots.
-- Specify @IMAGE_MANAGEMENT@ to create a lifecycle policy that manages the
-- lifecycle of EBS-backed AMIs. Specify @EVENT_BASED_POLICY @ to create an
-- event-based policy that performs specific actions when a defined event
-- occurs in your Amazon Web Services account.
--
-- The default is @EBS_SNAPSHOT_MANAGEMENT@.
--
-- 'resourceLocations', 'policyDetails_resourceLocations' - __[Snapshot and AMI policies only]__ The location of the resources to
-- backup. If the source resources are located in an Amazon Web Services
-- Region, specify @CLOUD@. If the source resources are located on an
-- Outpost in your account, specify @OUTPOST@.
--
-- If you specify @OUTPOST@, Amazon Data Lifecycle Manager backs up all
-- resources of the specified type with matching target tags across all of
-- the Outposts in your account.
--
-- 'resourceTypes', 'policyDetails_resourceTypes' - __[Snapshot policies only]__ The target resource type for snapshot and
-- AMI lifecycle policies. Use @VOLUME @to create snapshots of individual
-- volumes or use @INSTANCE@ to create multi-volume snapshots from the
-- volumes for an instance.
--
-- 'schedules', 'policyDetails_schedules' - __[Snapshot and AMI policies only]__ The schedules of policy-defined
-- actions for snapshot and AMI lifecycle policies. A policy can have up to
-- four schedules—one mandatory schedule and up to three optional
-- schedules.
--
-- 'targetTags', 'policyDetails_targetTags' - __[Snapshot and AMI policies only]__ The single tag that identifies
-- targeted resources for this policy.
newPolicyDetails ::
  PolicyDetails
newPolicyDetails =
  PolicyDetails'
    { actions = Prelude.Nothing,
      eventSource = Prelude.Nothing,
      parameters = Prelude.Nothing,
      policyType = Prelude.Nothing,
      resourceLocations = Prelude.Nothing,
      resourceTypes = Prelude.Nothing,
      schedules = Prelude.Nothing,
      targetTags = Prelude.Nothing
    }

-- | __[Event-based policies only]__ The actions to be performed when the
-- event-based policy is activated. You can specify only one action per
-- policy.
policyDetails_actions :: Lens.Lens' PolicyDetails (Prelude.Maybe (Prelude.NonEmpty Action))
policyDetails_actions = Lens.lens (\PolicyDetails' {actions} -> actions) (\s@PolicyDetails' {} a -> s {actions = a} :: PolicyDetails) Prelude.. Lens.mapping Lens.coerced

-- | __[Event-based policies only]__ The event that activates the event-based
-- policy.
policyDetails_eventSource :: Lens.Lens' PolicyDetails (Prelude.Maybe EventSource)
policyDetails_eventSource = Lens.lens (\PolicyDetails' {eventSource} -> eventSource) (\s@PolicyDetails' {} a -> s {eventSource = a} :: PolicyDetails)

-- | __[Snapshot and AMI policies only]__ A set of optional parameters for
-- snapshot and AMI lifecycle policies.
--
-- If you are modifying a policy that was created or previously modified
-- using the Amazon Data Lifecycle Manager console, then you must include
-- this parameter and specify either the default values or the new values
-- that you require. You can\'t omit this parameter or set its values to
-- null.
policyDetails_parameters :: Lens.Lens' PolicyDetails (Prelude.Maybe Parameters)
policyDetails_parameters = Lens.lens (\PolicyDetails' {parameters} -> parameters) (\s@PolicyDetails' {} a -> s {parameters = a} :: PolicyDetails)

-- | __[All policy types]__ The valid target resource types and actions a
-- policy can manage. Specify @EBS_SNAPSHOT_MANAGEMENT@ to create a
-- lifecycle policy that manages the lifecycle of Amazon EBS snapshots.
-- Specify @IMAGE_MANAGEMENT@ to create a lifecycle policy that manages the
-- lifecycle of EBS-backed AMIs. Specify @EVENT_BASED_POLICY @ to create an
-- event-based policy that performs specific actions when a defined event
-- occurs in your Amazon Web Services account.
--
-- The default is @EBS_SNAPSHOT_MANAGEMENT@.
policyDetails_policyType :: Lens.Lens' PolicyDetails (Prelude.Maybe PolicyTypeValues)
policyDetails_policyType = Lens.lens (\PolicyDetails' {policyType} -> policyType) (\s@PolicyDetails' {} a -> s {policyType = a} :: PolicyDetails)

-- | __[Snapshot and AMI policies only]__ The location of the resources to
-- backup. If the source resources are located in an Amazon Web Services
-- Region, specify @CLOUD@. If the source resources are located on an
-- Outpost in your account, specify @OUTPOST@.
--
-- If you specify @OUTPOST@, Amazon Data Lifecycle Manager backs up all
-- resources of the specified type with matching target tags across all of
-- the Outposts in your account.
policyDetails_resourceLocations :: Lens.Lens' PolicyDetails (Prelude.Maybe (Prelude.NonEmpty ResourceLocationValues))
policyDetails_resourceLocations = Lens.lens (\PolicyDetails' {resourceLocations} -> resourceLocations) (\s@PolicyDetails' {} a -> s {resourceLocations = a} :: PolicyDetails) Prelude.. Lens.mapping Lens.coerced

-- | __[Snapshot policies only]__ The target resource type for snapshot and
-- AMI lifecycle policies. Use @VOLUME @to create snapshots of individual
-- volumes or use @INSTANCE@ to create multi-volume snapshots from the
-- volumes for an instance.
policyDetails_resourceTypes :: Lens.Lens' PolicyDetails (Prelude.Maybe (Prelude.NonEmpty ResourceTypeValues))
policyDetails_resourceTypes = Lens.lens (\PolicyDetails' {resourceTypes} -> resourceTypes) (\s@PolicyDetails' {} a -> s {resourceTypes = a} :: PolicyDetails) Prelude.. Lens.mapping Lens.coerced

-- | __[Snapshot and AMI policies only]__ The schedules of policy-defined
-- actions for snapshot and AMI lifecycle policies. A policy can have up to
-- four schedules—one mandatory schedule and up to three optional
-- schedules.
policyDetails_schedules :: Lens.Lens' PolicyDetails (Prelude.Maybe (Prelude.NonEmpty Schedule))
policyDetails_schedules = Lens.lens (\PolicyDetails' {schedules} -> schedules) (\s@PolicyDetails' {} a -> s {schedules = a} :: PolicyDetails) Prelude.. Lens.mapping Lens.coerced

-- | __[Snapshot and AMI policies only]__ The single tag that identifies
-- targeted resources for this policy.
policyDetails_targetTags :: Lens.Lens' PolicyDetails (Prelude.Maybe (Prelude.NonEmpty Tag))
policyDetails_targetTags = Lens.lens (\PolicyDetails' {targetTags} -> targetTags) (\s@PolicyDetails' {} a -> s {targetTags = a} :: PolicyDetails) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PolicyDetails where
  parseJSON =
    Data.withObject
      "PolicyDetails"
      ( \x ->
          PolicyDetails'
            Prelude.<$> (x Data..:? "Actions")
            Prelude.<*> (x Data..:? "EventSource")
            Prelude.<*> (x Data..:? "Parameters")
            Prelude.<*> (x Data..:? "PolicyType")
            Prelude.<*> (x Data..:? "ResourceLocations")
            Prelude.<*> (x Data..:? "ResourceTypes")
            Prelude.<*> (x Data..:? "Schedules")
            Prelude.<*> (x Data..:? "TargetTags")
      )

instance Prelude.Hashable PolicyDetails where
  hashWithSalt _salt PolicyDetails' {..} =
    _salt
      `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` eventSource
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` policyType
      `Prelude.hashWithSalt` resourceLocations
      `Prelude.hashWithSalt` resourceTypes
      `Prelude.hashWithSalt` schedules
      `Prelude.hashWithSalt` targetTags

instance Prelude.NFData PolicyDetails where
  rnf PolicyDetails' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf eventSource
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf policyType
      `Prelude.seq` Prelude.rnf resourceLocations
      `Prelude.seq` Prelude.rnf resourceTypes
      `Prelude.seq` Prelude.rnf schedules
      `Prelude.seq` Prelude.rnf targetTags

instance Data.ToJSON PolicyDetails where
  toJSON PolicyDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Actions" Data..=) Prelude.<$> actions,
            ("EventSource" Data..=) Prelude.<$> eventSource,
            ("Parameters" Data..=) Prelude.<$> parameters,
            ("PolicyType" Data..=) Prelude.<$> policyType,
            ("ResourceLocations" Data..=)
              Prelude.<$> resourceLocations,
            ("ResourceTypes" Data..=) Prelude.<$> resourceTypes,
            ("Schedules" Data..=) Prelude.<$> schedules,
            ("TargetTags" Data..=) Prelude.<$> targetTags
          ]
      )
