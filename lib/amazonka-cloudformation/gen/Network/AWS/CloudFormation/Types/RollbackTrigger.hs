{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.RollbackTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.RollbackTrigger
  ( RollbackTrigger (..)
  -- * Smart constructor
  , mkRollbackTrigger
  -- * Lenses
  , rtArn
  , rtType
  ) where

import qualified Network.AWS.CloudFormation.Types.Arn as Types
import qualified Network.AWS.CloudFormation.Types.Type as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A rollback trigger AWS CloudFormation monitors during creation and updating of stacks. If any of the alarms you specify goes to ALARM state during the stack operation or within the specified monitoring period afterwards, CloudFormation rolls back the entire stack operation. 
--
-- /See:/ 'mkRollbackTrigger' smart constructor.
data RollbackTrigger = RollbackTrigger'
  { arn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the rollback trigger.
--
-- If a specified trigger is missing, the entire stack operation fails and is rolled back. 
  , type' :: Types.Type
    -- ^ The resource type of the rollback trigger. Currently, <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-cw-alarm.html AWS::CloudWatch::Alarm> is the only supported resource type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RollbackTrigger' value with any optional fields omitted.
mkRollbackTrigger
    :: Types.Arn -- ^ 'arn'
    -> Types.Type -- ^ 'type\''
    -> RollbackTrigger
mkRollbackTrigger arn type' = RollbackTrigger'{arn, type'}

-- | The Amazon Resource Name (ARN) of the rollback trigger.
--
-- If a specified trigger is missing, the entire stack operation fails and is rolled back. 
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtArn :: Lens.Lens' RollbackTrigger Types.Arn
rtArn = Lens.field @"arn"
{-# INLINEABLE rtArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The resource type of the rollback trigger. Currently, <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-cw-alarm.html AWS::CloudWatch::Alarm> is the only supported resource type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtType :: Lens.Lens' RollbackTrigger Types.Type
rtType = Lens.field @"type'"
{-# INLINEABLE rtType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.ToQuery RollbackTrigger where
        toQuery RollbackTrigger{..}
          = Core.toQueryPair "Arn" arn Core.<> Core.toQueryPair "Type" type'

instance Core.FromXML RollbackTrigger where
        parseXML x
          = RollbackTrigger' Core.<$>
              (x Core..@ "Arn") Core.<*> x Core..@ "Type"
