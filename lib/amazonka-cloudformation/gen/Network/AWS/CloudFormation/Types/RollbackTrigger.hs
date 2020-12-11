-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.RollbackTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.RollbackTrigger
  ( RollbackTrigger (..),

    -- * Smart constructor
    mkRollbackTrigger,

    -- * Lenses
    rtARN,
    rtType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A rollback trigger AWS CloudFormation monitors during creation and updating of stacks. If any of the alarms you specify goes to ALARM state during the stack operation or within the specified monitoring period afterwards, CloudFormation rolls back the entire stack operation.
--
-- /See:/ 'mkRollbackTrigger' smart constructor.
data RollbackTrigger = RollbackTrigger'
  { arn :: Lude.Text,
    type' :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RollbackTrigger' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the rollback trigger.
--
-- If a specified trigger is missing, the entire stack operation fails and is rolled back.
-- * 'type'' - The resource type of the rollback trigger. Currently, <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-cw-alarm.html AWS::CloudWatch::Alarm> is the only supported resource type.
mkRollbackTrigger ::
  -- | 'arn'
  Lude.Text ->
  -- | 'type''
  Lude.Text ->
  RollbackTrigger
mkRollbackTrigger pARN_ pType_ =
  RollbackTrigger' {arn = pARN_, type' = pType_}

-- | The Amazon Resource Name (ARN) of the rollback trigger.
--
-- If a specified trigger is missing, the entire stack operation fails and is rolled back.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtARN :: Lens.Lens' RollbackTrigger Lude.Text
rtARN = Lens.lens (arn :: RollbackTrigger -> Lude.Text) (\s a -> s {arn = a} :: RollbackTrigger)
{-# DEPRECATED rtARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The resource type of the rollback trigger. Currently, <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-cw-alarm.html AWS::CloudWatch::Alarm> is the only supported resource type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtType :: Lens.Lens' RollbackTrigger Lude.Text
rtType = Lens.lens (type' :: RollbackTrigger -> Lude.Text) (\s a -> s {type' = a} :: RollbackTrigger)
{-# DEPRECATED rtType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromXML RollbackTrigger where
  parseXML x =
    RollbackTrigger'
      Lude.<$> (x Lude..@ "Arn") Lude.<*> (x Lude..@ "Type")

instance Lude.ToQuery RollbackTrigger where
  toQuery RollbackTrigger' {..} =
    Lude.mconcat ["Arn" Lude.=: arn, "Type" Lude.=: type']
