-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.AttackSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.AttackSummary
  ( AttackSummary (..),

    -- * Smart constructor
    mkAttackSummary,

    -- * Lenses
    asAttackVectors,
    asAttackId,
    asStartTime,
    asResourceARN,
    asEndTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Shield.Types.AttackVectorDescription

-- | Summarizes all DDoS attacks for a specified time period.
--
-- /See:/ 'mkAttackSummary' smart constructor.
data AttackSummary = AttackSummary'
  { attackVectors ::
      Lude.Maybe [AttackVectorDescription],
    attackId :: Lude.Maybe Lude.Text,
    startTime :: Lude.Maybe Lude.Timestamp,
    resourceARN :: Lude.Maybe Lude.Text,
    endTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttackSummary' with the minimum fields required to make a request.
--
-- * 'attackId' - The unique identifier (ID) of the attack.
-- * 'attackVectors' - The list of attacks for a specified time period.
-- * 'endTime' - The end time of the attack, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
-- * 'resourceARN' - The ARN (Amazon Resource Name) of the resource that was attacked.
-- * 'startTime' - The start time of the attack, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
mkAttackSummary ::
  AttackSummary
mkAttackSummary =
  AttackSummary'
    { attackVectors = Lude.Nothing,
      attackId = Lude.Nothing,
      startTime = Lude.Nothing,
      resourceARN = Lude.Nothing,
      endTime = Lude.Nothing
    }

-- | The list of attacks for a specified time period.
--
-- /Note:/ Consider using 'attackVectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asAttackVectors :: Lens.Lens' AttackSummary (Lude.Maybe [AttackVectorDescription])
asAttackVectors = Lens.lens (attackVectors :: AttackSummary -> Lude.Maybe [AttackVectorDescription]) (\s a -> s {attackVectors = a} :: AttackSummary)
{-# DEPRECATED asAttackVectors "Use generic-lens or generic-optics with 'attackVectors' instead." #-}

-- | The unique identifier (ID) of the attack.
--
-- /Note:/ Consider using 'attackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asAttackId :: Lens.Lens' AttackSummary (Lude.Maybe Lude.Text)
asAttackId = Lens.lens (attackId :: AttackSummary -> Lude.Maybe Lude.Text) (\s a -> s {attackId = a} :: AttackSummary)
{-# DEPRECATED asAttackId "Use generic-lens or generic-optics with 'attackId' instead." #-}

-- | The start time of the attack, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asStartTime :: Lens.Lens' AttackSummary (Lude.Maybe Lude.Timestamp)
asStartTime = Lens.lens (startTime :: AttackSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: AttackSummary)
{-# DEPRECATED asStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The ARN (Amazon Resource Name) of the resource that was attacked.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asResourceARN :: Lens.Lens' AttackSummary (Lude.Maybe Lude.Text)
asResourceARN = Lens.lens (resourceARN :: AttackSummary -> Lude.Maybe Lude.Text) (\s a -> s {resourceARN = a} :: AttackSummary)
{-# DEPRECATED asResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | The end time of the attack, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asEndTime :: Lens.Lens' AttackSummary (Lude.Maybe Lude.Timestamp)
asEndTime = Lens.lens (endTime :: AttackSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: AttackSummary)
{-# DEPRECATED asEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

instance Lude.FromJSON AttackSummary where
  parseJSON =
    Lude.withObject
      "AttackSummary"
      ( \x ->
          AttackSummary'
            Lude.<$> (x Lude..:? "AttackVectors" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AttackId")
            Lude.<*> (x Lude..:? "StartTime")
            Lude.<*> (x Lude..:? "ResourceArn")
            Lude.<*> (x Lude..:? "EndTime")
      )
