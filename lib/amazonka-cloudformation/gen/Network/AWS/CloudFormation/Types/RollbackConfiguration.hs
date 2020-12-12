{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.RollbackConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.RollbackConfiguration
  ( RollbackConfiguration (..),

    -- * Smart constructor
    mkRollbackConfiguration,

    -- * Lenses
    rcRollbackTriggers,
    rcMonitoringTimeInMinutes,
  )
where

import Network.AWS.CloudFormation.Types.RollbackTrigger
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Structure containing the rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
--
-- Rollback triggers enable you to have AWS CloudFormation monitor the state of your application during stack creation and updating, and to roll back that operation if the application breaches the threshold of any of the alarms you've specified. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-rollback-triggers.html Monitor and Roll Back Stack Operations> .
--
-- /See:/ 'mkRollbackConfiguration' smart constructor.
data RollbackConfiguration = RollbackConfiguration'
  { rollbackTriggers ::
      Lude.Maybe [RollbackTrigger],
    monitoringTimeInMinutes ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RollbackConfiguration' with the minimum fields required to make a request.
--
-- * 'monitoringTimeInMinutes' - The amount of time, in minutes, during which CloudFormation should monitor all the rollback triggers after the stack creation or update operation deploys all necessary resources.
--
-- The default is 0 minutes.
-- If you specify a monitoring period but do not specify any rollback triggers, CloudFormation still waits the specified period of time before cleaning up old resources after update operations. You can use this monitoring period to perform any manual stack validation desired, and manually cancel the stack creation or update (using <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CancelUpdateStack.html CancelUpdateStack> , for example) as necessary.
-- If you specify 0 for this parameter, CloudFormation still monitors the specified rollback triggers during stack creation and update operations. Then, for update operations, it begins disposing of old resources immediately once the operation completes.
-- * 'rollbackTriggers' - The triggers to monitor during stack creation or update actions.
--
-- By default, AWS CloudFormation saves the rollback triggers specified for a stack and applies them to any subsequent update operations for the stack, unless you specify otherwise. If you do specify rollback triggers for this parameter, those triggers replace any list of triggers previously specified for the stack. This means:
--
--     * To use the rollback triggers previously specified for this stack, if any, don't specify this parameter.
--
--
--     * To specify new or updated rollback triggers, you must specify /all/ the triggers that you want used for this stack, even triggers you've specifed before (for example, when creating the stack or during a previous stack update). Any triggers that you don't include in the updated list of triggers are no longer applied to the stack.
--
--
--     * To remove all currently specified triggers, specify an empty list for this parameter.
--
--
-- If a specified trigger is missing, the entire stack operation fails and is rolled back.
mkRollbackConfiguration ::
  RollbackConfiguration
mkRollbackConfiguration =
  RollbackConfiguration'
    { rollbackTriggers = Lude.Nothing,
      monitoringTimeInMinutes = Lude.Nothing
    }

-- | The triggers to monitor during stack creation or update actions.
--
-- By default, AWS CloudFormation saves the rollback triggers specified for a stack and applies them to any subsequent update operations for the stack, unless you specify otherwise. If you do specify rollback triggers for this parameter, those triggers replace any list of triggers previously specified for the stack. This means:
--
--     * To use the rollback triggers previously specified for this stack, if any, don't specify this parameter.
--
--
--     * To specify new or updated rollback triggers, you must specify /all/ the triggers that you want used for this stack, even triggers you've specifed before (for example, when creating the stack or during a previous stack update). Any triggers that you don't include in the updated list of triggers are no longer applied to the stack.
--
--
--     * To remove all currently specified triggers, specify an empty list for this parameter.
--
--
-- If a specified trigger is missing, the entire stack operation fails and is rolled back.
--
-- /Note:/ Consider using 'rollbackTriggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRollbackTriggers :: Lens.Lens' RollbackConfiguration (Lude.Maybe [RollbackTrigger])
rcRollbackTriggers = Lens.lens (rollbackTriggers :: RollbackConfiguration -> Lude.Maybe [RollbackTrigger]) (\s a -> s {rollbackTriggers = a} :: RollbackConfiguration)
{-# DEPRECATED rcRollbackTriggers "Use generic-lens or generic-optics with 'rollbackTriggers' instead." #-}

-- | The amount of time, in minutes, during which CloudFormation should monitor all the rollback triggers after the stack creation or update operation deploys all necessary resources.
--
-- The default is 0 minutes.
-- If you specify a monitoring period but do not specify any rollback triggers, CloudFormation still waits the specified period of time before cleaning up old resources after update operations. You can use this monitoring period to perform any manual stack validation desired, and manually cancel the stack creation or update (using <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CancelUpdateStack.html CancelUpdateStack> , for example) as necessary.
-- If you specify 0 for this parameter, CloudFormation still monitors the specified rollback triggers during stack creation and update operations. Then, for update operations, it begins disposing of old resources immediately once the operation completes.
--
-- /Note:/ Consider using 'monitoringTimeInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcMonitoringTimeInMinutes :: Lens.Lens' RollbackConfiguration (Lude.Maybe Lude.Natural)
rcMonitoringTimeInMinutes = Lens.lens (monitoringTimeInMinutes :: RollbackConfiguration -> Lude.Maybe Lude.Natural) (\s a -> s {monitoringTimeInMinutes = a} :: RollbackConfiguration)
{-# DEPRECATED rcMonitoringTimeInMinutes "Use generic-lens or generic-optics with 'monitoringTimeInMinutes' instead." #-}

instance Lude.FromXML RollbackConfiguration where
  parseXML x =
    RollbackConfiguration'
      Lude.<$> ( x Lude..@? "RollbackTriggers" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "MonitoringTimeInMinutes")

instance Lude.ToQuery RollbackConfiguration where
  toQuery RollbackConfiguration' {..} =
    Lude.mconcat
      [ "RollbackTriggers"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> rollbackTriggers),
        "MonitoringTimeInMinutes" Lude.=: monitoringTimeInMinutes
      ]
