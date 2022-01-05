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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DLM.Types.PolicyDetails where

import qualified Amazonka.Core as Core
import Amazonka.DLM.Types.Action
import Amazonka.DLM.Types.EventSource
import Amazonka.DLM.Types.Parameters
import Amazonka.DLM.Types.PolicyTypeValues
import Amazonka.DLM.Types.ResourceLocationValues
import Amazonka.DLM.Types.ResourceTypeValues
import Amazonka.DLM.Types.Schedule
import Amazonka.DLM.Types.Tag
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies the configuration of a lifecycle policy.
--
-- /See:/ 'newPolicyDetails' smart constructor.
data PolicyDetails = PolicyDetails'
  { -- | The actions to be performed when the event-based policy is triggered.
    -- You can specify only one action per policy.
    --
    -- This parameter is required for event-based policies only. If you are
    -- creating a snapshot or AMI policy, omit this parameter.
    actions :: Prelude.Maybe (Prelude.NonEmpty Action),
    -- | The single tag that identifies targeted resources for this policy.
    --
    -- This parameter is required for snapshot and AMI policies only. If you
    -- are creating an event-based policy, omit this parameter.
    targetTags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The valid target resource types and actions a policy can manage. Specify
    -- @EBS_SNAPSHOT_MANAGEMENT@ to create a lifecycle policy that manages the
    -- lifecycle of Amazon EBS snapshots. Specify @IMAGE_MANAGEMENT@ to create
    -- a lifecycle policy that manages the lifecycle of EBS-backed AMIs.
    -- Specify @EVENT_BASED_POLICY @ to create an event-based policy that
    -- performs specific actions when a defined event occurs in your Amazon Web
    -- Services account.
    --
    -- The default is @EBS_SNAPSHOT_MANAGEMENT@.
    policyType :: Prelude.Maybe PolicyTypeValues,
    -- | The location of the resources to backup. If the source resources are
    -- located in an Amazon Web Services Region, specify @CLOUD@. If the source
    -- resources are located on an Outpost in your account, specify @OUTPOST@.
    --
    -- If you specify @OUTPOST@, Amazon Data Lifecycle Manager backs up all
    -- resources of the specified type with matching target tags across all of
    -- the Outposts in your account.
    resourceLocations :: Prelude.Maybe (Prelude.NonEmpty ResourceLocationValues),
    -- | A set of optional parameters for snapshot and AMI lifecycle policies.
    --
    -- This parameter is required for snapshot and AMI policies only. If you
    -- are creating an event-based policy, omit this parameter.
    parameters :: Prelude.Maybe Parameters,
    -- | The schedules of policy-defined actions for snapshot and AMI lifecycle
    -- policies. A policy can have up to four schedules—one mandatory schedule
    -- and up to three optional schedules.
    --
    -- This parameter is required for snapshot and AMI policies only. If you
    -- are creating an event-based policy, omit this parameter.
    schedules :: Prelude.Maybe (Prelude.NonEmpty Schedule),
    -- | The event that triggers the event-based policy.
    --
    -- This parameter is required for event-based policies only. If you are
    -- creating a snapshot or AMI policy, omit this parameter.
    eventSource :: Prelude.Maybe EventSource,
    -- | The target resource type for snapshot and AMI lifecycle policies. Use
    -- @VOLUME @to create snapshots of individual volumes or use @INSTANCE@ to
    -- create multi-volume snapshots from the volumes for an instance.
    --
    -- This parameter is required for snapshot and AMI policies only. If you
    -- are creating an event-based policy, omit this parameter.
    resourceTypes :: Prelude.Maybe (Prelude.NonEmpty ResourceTypeValues)
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
-- 'actions', 'policyDetails_actions' - The actions to be performed when the event-based policy is triggered.
-- You can specify only one action per policy.
--
-- This parameter is required for event-based policies only. If you are
-- creating a snapshot or AMI policy, omit this parameter.
--
-- 'targetTags', 'policyDetails_targetTags' - The single tag that identifies targeted resources for this policy.
--
-- This parameter is required for snapshot and AMI policies only. If you
-- are creating an event-based policy, omit this parameter.
--
-- 'policyType', 'policyDetails_policyType' - The valid target resource types and actions a policy can manage. Specify
-- @EBS_SNAPSHOT_MANAGEMENT@ to create a lifecycle policy that manages the
-- lifecycle of Amazon EBS snapshots. Specify @IMAGE_MANAGEMENT@ to create
-- a lifecycle policy that manages the lifecycle of EBS-backed AMIs.
-- Specify @EVENT_BASED_POLICY @ to create an event-based policy that
-- performs specific actions when a defined event occurs in your Amazon Web
-- Services account.
--
-- The default is @EBS_SNAPSHOT_MANAGEMENT@.
--
-- 'resourceLocations', 'policyDetails_resourceLocations' - The location of the resources to backup. If the source resources are
-- located in an Amazon Web Services Region, specify @CLOUD@. If the source
-- resources are located on an Outpost in your account, specify @OUTPOST@.
--
-- If you specify @OUTPOST@, Amazon Data Lifecycle Manager backs up all
-- resources of the specified type with matching target tags across all of
-- the Outposts in your account.
--
-- 'parameters', 'policyDetails_parameters' - A set of optional parameters for snapshot and AMI lifecycle policies.
--
-- This parameter is required for snapshot and AMI policies only. If you
-- are creating an event-based policy, omit this parameter.
--
-- 'schedules', 'policyDetails_schedules' - The schedules of policy-defined actions for snapshot and AMI lifecycle
-- policies. A policy can have up to four schedules—one mandatory schedule
-- and up to three optional schedules.
--
-- This parameter is required for snapshot and AMI policies only. If you
-- are creating an event-based policy, omit this parameter.
--
-- 'eventSource', 'policyDetails_eventSource' - The event that triggers the event-based policy.
--
-- This parameter is required for event-based policies only. If you are
-- creating a snapshot or AMI policy, omit this parameter.
--
-- 'resourceTypes', 'policyDetails_resourceTypes' - The target resource type for snapshot and AMI lifecycle policies. Use
-- @VOLUME @to create snapshots of individual volumes or use @INSTANCE@ to
-- create multi-volume snapshots from the volumes for an instance.
--
-- This parameter is required for snapshot and AMI policies only. If you
-- are creating an event-based policy, omit this parameter.
newPolicyDetails ::
  PolicyDetails
newPolicyDetails =
  PolicyDetails'
    { actions = Prelude.Nothing,
      targetTags = Prelude.Nothing,
      policyType = Prelude.Nothing,
      resourceLocations = Prelude.Nothing,
      parameters = Prelude.Nothing,
      schedules = Prelude.Nothing,
      eventSource = Prelude.Nothing,
      resourceTypes = Prelude.Nothing
    }

-- | The actions to be performed when the event-based policy is triggered.
-- You can specify only one action per policy.
--
-- This parameter is required for event-based policies only. If you are
-- creating a snapshot or AMI policy, omit this parameter.
policyDetails_actions :: Lens.Lens' PolicyDetails (Prelude.Maybe (Prelude.NonEmpty Action))
policyDetails_actions = Lens.lens (\PolicyDetails' {actions} -> actions) (\s@PolicyDetails' {} a -> s {actions = a} :: PolicyDetails) Prelude.. Lens.mapping Lens.coerced

-- | The single tag that identifies targeted resources for this policy.
--
-- This parameter is required for snapshot and AMI policies only. If you
-- are creating an event-based policy, omit this parameter.
policyDetails_targetTags :: Lens.Lens' PolicyDetails (Prelude.Maybe (Prelude.NonEmpty Tag))
policyDetails_targetTags = Lens.lens (\PolicyDetails' {targetTags} -> targetTags) (\s@PolicyDetails' {} a -> s {targetTags = a} :: PolicyDetails) Prelude.. Lens.mapping Lens.coerced

-- | The valid target resource types and actions a policy can manage. Specify
-- @EBS_SNAPSHOT_MANAGEMENT@ to create a lifecycle policy that manages the
-- lifecycle of Amazon EBS snapshots. Specify @IMAGE_MANAGEMENT@ to create
-- a lifecycle policy that manages the lifecycle of EBS-backed AMIs.
-- Specify @EVENT_BASED_POLICY @ to create an event-based policy that
-- performs specific actions when a defined event occurs in your Amazon Web
-- Services account.
--
-- The default is @EBS_SNAPSHOT_MANAGEMENT@.
policyDetails_policyType :: Lens.Lens' PolicyDetails (Prelude.Maybe PolicyTypeValues)
policyDetails_policyType = Lens.lens (\PolicyDetails' {policyType} -> policyType) (\s@PolicyDetails' {} a -> s {policyType = a} :: PolicyDetails)

-- | The location of the resources to backup. If the source resources are
-- located in an Amazon Web Services Region, specify @CLOUD@. If the source
-- resources are located on an Outpost in your account, specify @OUTPOST@.
--
-- If you specify @OUTPOST@, Amazon Data Lifecycle Manager backs up all
-- resources of the specified type with matching target tags across all of
-- the Outposts in your account.
policyDetails_resourceLocations :: Lens.Lens' PolicyDetails (Prelude.Maybe (Prelude.NonEmpty ResourceLocationValues))
policyDetails_resourceLocations = Lens.lens (\PolicyDetails' {resourceLocations} -> resourceLocations) (\s@PolicyDetails' {} a -> s {resourceLocations = a} :: PolicyDetails) Prelude.. Lens.mapping Lens.coerced

-- | A set of optional parameters for snapshot and AMI lifecycle policies.
--
-- This parameter is required for snapshot and AMI policies only. If you
-- are creating an event-based policy, omit this parameter.
policyDetails_parameters :: Lens.Lens' PolicyDetails (Prelude.Maybe Parameters)
policyDetails_parameters = Lens.lens (\PolicyDetails' {parameters} -> parameters) (\s@PolicyDetails' {} a -> s {parameters = a} :: PolicyDetails)

-- | The schedules of policy-defined actions for snapshot and AMI lifecycle
-- policies. A policy can have up to four schedules—one mandatory schedule
-- and up to three optional schedules.
--
-- This parameter is required for snapshot and AMI policies only. If you
-- are creating an event-based policy, omit this parameter.
policyDetails_schedules :: Lens.Lens' PolicyDetails (Prelude.Maybe (Prelude.NonEmpty Schedule))
policyDetails_schedules = Lens.lens (\PolicyDetails' {schedules} -> schedules) (\s@PolicyDetails' {} a -> s {schedules = a} :: PolicyDetails) Prelude.. Lens.mapping Lens.coerced

-- | The event that triggers the event-based policy.
--
-- This parameter is required for event-based policies only. If you are
-- creating a snapshot or AMI policy, omit this parameter.
policyDetails_eventSource :: Lens.Lens' PolicyDetails (Prelude.Maybe EventSource)
policyDetails_eventSource = Lens.lens (\PolicyDetails' {eventSource} -> eventSource) (\s@PolicyDetails' {} a -> s {eventSource = a} :: PolicyDetails)

-- | The target resource type for snapshot and AMI lifecycle policies. Use
-- @VOLUME @to create snapshots of individual volumes or use @INSTANCE@ to
-- create multi-volume snapshots from the volumes for an instance.
--
-- This parameter is required for snapshot and AMI policies only. If you
-- are creating an event-based policy, omit this parameter.
policyDetails_resourceTypes :: Lens.Lens' PolicyDetails (Prelude.Maybe (Prelude.NonEmpty ResourceTypeValues))
policyDetails_resourceTypes = Lens.lens (\PolicyDetails' {resourceTypes} -> resourceTypes) (\s@PolicyDetails' {} a -> s {resourceTypes = a} :: PolicyDetails) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON PolicyDetails where
  parseJSON =
    Core.withObject
      "PolicyDetails"
      ( \x ->
          PolicyDetails'
            Prelude.<$> (x Core..:? "Actions")
            Prelude.<*> (x Core..:? "TargetTags")
            Prelude.<*> (x Core..:? "PolicyType")
            Prelude.<*> (x Core..:? "ResourceLocations")
            Prelude.<*> (x Core..:? "Parameters")
            Prelude.<*> (x Core..:? "Schedules")
            Prelude.<*> (x Core..:? "EventSource")
            Prelude.<*> (x Core..:? "ResourceTypes")
      )

instance Prelude.Hashable PolicyDetails where
  hashWithSalt _salt PolicyDetails' {..} =
    _salt `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` targetTags
      `Prelude.hashWithSalt` policyType
      `Prelude.hashWithSalt` resourceLocations
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` schedules
      `Prelude.hashWithSalt` eventSource
      `Prelude.hashWithSalt` resourceTypes

instance Prelude.NFData PolicyDetails where
  rnf PolicyDetails' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf targetTags
      `Prelude.seq` Prelude.rnf policyType
      `Prelude.seq` Prelude.rnf resourceLocations
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf schedules
      `Prelude.seq` Prelude.rnf eventSource
      `Prelude.seq` Prelude.rnf resourceTypes

instance Core.ToJSON PolicyDetails where
  toJSON PolicyDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Actions" Core..=) Prelude.<$> actions,
            ("TargetTags" Core..=) Prelude.<$> targetTags,
            ("PolicyType" Core..=) Prelude.<$> policyType,
            ("ResourceLocations" Core..=)
              Prelude.<$> resourceLocations,
            ("Parameters" Core..=) Prelude.<$> parameters,
            ("Schedules" Core..=) Prelude.<$> schedules,
            ("EventSource" Core..=) Prelude.<$> eventSource,
            ("ResourceTypes" Core..=) Prelude.<$> resourceTypes
          ]
      )
