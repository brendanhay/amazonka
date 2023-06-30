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
-- Module      : Amazonka.CloudFormation.Types.RollbackConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.RollbackConfiguration where

import Amazonka.CloudFormation.Types.RollbackTrigger
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Structure containing the rollback triggers for CloudFormation to monitor
-- during stack creation and updating operations, and for the specified
-- monitoring period afterwards.
--
-- Rollback triggers enable you to have CloudFormation monitor the state of
-- your application during stack creation and updating, and to roll back
-- that operation if the application breaches the threshold of any of the
-- alarms you\'ve specified. For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-rollback-triggers.html Monitor and Roll Back Stack Operations>.
--
-- /See:/ 'newRollbackConfiguration' smart constructor.
data RollbackConfiguration = RollbackConfiguration'
  { -- | The amount of time, in minutes, during which CloudFormation should
    -- monitor all the rollback triggers after the stack creation or update
    -- operation deploys all necessary resources.
    --
    -- The default is 0 minutes.
    --
    -- If you specify a monitoring period but don\'t specify any rollback
    -- triggers, CloudFormation still waits the specified period of time before
    -- cleaning up old resources after update operations. You can use this
    -- monitoring period to perform any manual stack validation desired, and
    -- manually cancel the stack creation or update (using
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CancelUpdateStack.html CancelUpdateStack>,
    -- for example) as necessary.
    --
    -- If you specify 0 for this parameter, CloudFormation still monitors the
    -- specified rollback triggers during stack creation and update operations.
    -- Then, for update operations, it begins disposing of old resources
    -- immediately once the operation completes.
    monitoringTimeInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The triggers to monitor during stack creation or update actions.
    --
    -- By default, CloudFormation saves the rollback triggers specified for a
    -- stack and applies them to any subsequent update operations for the
    -- stack, unless you specify otherwise. If you do specify rollback triggers
    -- for this parameter, those triggers replace any list of triggers
    -- previously specified for the stack. This means:
    --
    -- -   To use the rollback triggers previously specified for this stack, if
    --     any, don\'t specify this parameter.
    --
    -- -   To specify new or updated rollback triggers, you must specify /all/
    --     the triggers that you want used for this stack, even triggers
    --     you\'ve specified before (for example, when creating the stack or
    --     during a previous stack update). Any triggers that you don\'t
    --     include in the updated list of triggers are no longer applied to the
    --     stack.
    --
    -- -   To remove all currently specified triggers, specify an empty list
    --     for this parameter.
    --
    -- If a specified trigger is missing, the entire stack operation fails and
    -- is rolled back.
    rollbackTriggers :: Prelude.Maybe [RollbackTrigger]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RollbackConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monitoringTimeInMinutes', 'rollbackConfiguration_monitoringTimeInMinutes' - The amount of time, in minutes, during which CloudFormation should
-- monitor all the rollback triggers after the stack creation or update
-- operation deploys all necessary resources.
--
-- The default is 0 minutes.
--
-- If you specify a monitoring period but don\'t specify any rollback
-- triggers, CloudFormation still waits the specified period of time before
-- cleaning up old resources after update operations. You can use this
-- monitoring period to perform any manual stack validation desired, and
-- manually cancel the stack creation or update (using
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CancelUpdateStack.html CancelUpdateStack>,
-- for example) as necessary.
--
-- If you specify 0 for this parameter, CloudFormation still monitors the
-- specified rollback triggers during stack creation and update operations.
-- Then, for update operations, it begins disposing of old resources
-- immediately once the operation completes.
--
-- 'rollbackTriggers', 'rollbackConfiguration_rollbackTriggers' - The triggers to monitor during stack creation or update actions.
--
-- By default, CloudFormation saves the rollback triggers specified for a
-- stack and applies them to any subsequent update operations for the
-- stack, unless you specify otherwise. If you do specify rollback triggers
-- for this parameter, those triggers replace any list of triggers
-- previously specified for the stack. This means:
--
-- -   To use the rollback triggers previously specified for this stack, if
--     any, don\'t specify this parameter.
--
-- -   To specify new or updated rollback triggers, you must specify /all/
--     the triggers that you want used for this stack, even triggers
--     you\'ve specified before (for example, when creating the stack or
--     during a previous stack update). Any triggers that you don\'t
--     include in the updated list of triggers are no longer applied to the
--     stack.
--
-- -   To remove all currently specified triggers, specify an empty list
--     for this parameter.
--
-- If a specified trigger is missing, the entire stack operation fails and
-- is rolled back.
newRollbackConfiguration ::
  RollbackConfiguration
newRollbackConfiguration =
  RollbackConfiguration'
    { monitoringTimeInMinutes =
        Prelude.Nothing,
      rollbackTriggers = Prelude.Nothing
    }

-- | The amount of time, in minutes, during which CloudFormation should
-- monitor all the rollback triggers after the stack creation or update
-- operation deploys all necessary resources.
--
-- The default is 0 minutes.
--
-- If you specify a monitoring period but don\'t specify any rollback
-- triggers, CloudFormation still waits the specified period of time before
-- cleaning up old resources after update operations. You can use this
-- monitoring period to perform any manual stack validation desired, and
-- manually cancel the stack creation or update (using
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CancelUpdateStack.html CancelUpdateStack>,
-- for example) as necessary.
--
-- If you specify 0 for this parameter, CloudFormation still monitors the
-- specified rollback triggers during stack creation and update operations.
-- Then, for update operations, it begins disposing of old resources
-- immediately once the operation completes.
rollbackConfiguration_monitoringTimeInMinutes :: Lens.Lens' RollbackConfiguration (Prelude.Maybe Prelude.Natural)
rollbackConfiguration_monitoringTimeInMinutes = Lens.lens (\RollbackConfiguration' {monitoringTimeInMinutes} -> monitoringTimeInMinutes) (\s@RollbackConfiguration' {} a -> s {monitoringTimeInMinutes = a} :: RollbackConfiguration)

-- | The triggers to monitor during stack creation or update actions.
--
-- By default, CloudFormation saves the rollback triggers specified for a
-- stack and applies them to any subsequent update operations for the
-- stack, unless you specify otherwise. If you do specify rollback triggers
-- for this parameter, those triggers replace any list of triggers
-- previously specified for the stack. This means:
--
-- -   To use the rollback triggers previously specified for this stack, if
--     any, don\'t specify this parameter.
--
-- -   To specify new or updated rollback triggers, you must specify /all/
--     the triggers that you want used for this stack, even triggers
--     you\'ve specified before (for example, when creating the stack or
--     during a previous stack update). Any triggers that you don\'t
--     include in the updated list of triggers are no longer applied to the
--     stack.
--
-- -   To remove all currently specified triggers, specify an empty list
--     for this parameter.
--
-- If a specified trigger is missing, the entire stack operation fails and
-- is rolled back.
rollbackConfiguration_rollbackTriggers :: Lens.Lens' RollbackConfiguration (Prelude.Maybe [RollbackTrigger])
rollbackConfiguration_rollbackTriggers = Lens.lens (\RollbackConfiguration' {rollbackTriggers} -> rollbackTriggers) (\s@RollbackConfiguration' {} a -> s {rollbackTriggers = a} :: RollbackConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML RollbackConfiguration where
  parseXML x =
    RollbackConfiguration'
      Prelude.<$> (x Data..@? "MonitoringTimeInMinutes")
      Prelude.<*> ( x
                      Data..@? "RollbackTriggers"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )

instance Prelude.Hashable RollbackConfiguration where
  hashWithSalt _salt RollbackConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` monitoringTimeInMinutes
      `Prelude.hashWithSalt` rollbackTriggers

instance Prelude.NFData RollbackConfiguration where
  rnf RollbackConfiguration' {..} =
    Prelude.rnf monitoringTimeInMinutes
      `Prelude.seq` Prelude.rnf rollbackTriggers

instance Data.ToQuery RollbackConfiguration where
  toQuery RollbackConfiguration' {..} =
    Prelude.mconcat
      [ "MonitoringTimeInMinutes"
          Data.=: monitoringTimeInMinutes,
        "RollbackTriggers"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> rollbackTriggers
            )
      ]
