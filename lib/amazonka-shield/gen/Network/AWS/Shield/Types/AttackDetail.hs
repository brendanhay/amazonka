{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.AttackDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.AttackDetail
  ( AttackDetail (..),

    -- * Smart constructor
    mkAttackDetail,

    -- * Lenses
    adAttackId,
    adStartTime,
    adSubResources,
    adMitigations,
    adAttackProperties,
    adAttackCounters,
    adResourceARN,
    adEndTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Shield.Types.AttackProperty
import Network.AWS.Shield.Types.Mitigation
import Network.AWS.Shield.Types.SubResourceSummary
import Network.AWS.Shield.Types.SummarizedCounter

-- | The details of a DDoS attack.
--
-- /See:/ 'mkAttackDetail' smart constructor.
data AttackDetail = AttackDetail'
  { -- | The unique identifier (ID) of the attack.
    attackId :: Lude.Maybe Lude.Text,
    -- | The time the attack started, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
    startTime :: Lude.Maybe Lude.Timestamp,
    -- | If applicable, additional detail about the resource being attacked, for example, IP address or URL.
    subResources :: Lude.Maybe [SubResourceSummary],
    -- | List of mitigation actions taken for the attack.
    mitigations :: Lude.Maybe [Mitigation],
    -- | The array of 'AttackProperty' objects.
    attackProperties :: Lude.Maybe [AttackProperty],
    -- | List of counters that describe the attack for the specified time period.
    attackCounters :: Lude.Maybe [SummarizedCounter],
    -- | The ARN (Amazon Resource Name) of the resource that was attacked.
    resourceARN :: Lude.Maybe Lude.Text,
    -- | The time the attack ended, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
    endTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttackDetail' with the minimum fields required to make a request.
--
-- * 'attackId' - The unique identifier (ID) of the attack.
-- * 'startTime' - The time the attack started, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
-- * 'subResources' - If applicable, additional detail about the resource being attacked, for example, IP address or URL.
-- * 'mitigations' - List of mitigation actions taken for the attack.
-- * 'attackProperties' - The array of 'AttackProperty' objects.
-- * 'attackCounters' - List of counters that describe the attack for the specified time period.
-- * 'resourceARN' - The ARN (Amazon Resource Name) of the resource that was attacked.
-- * 'endTime' - The time the attack ended, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
mkAttackDetail ::
  AttackDetail
mkAttackDetail =
  AttackDetail'
    { attackId = Lude.Nothing,
      startTime = Lude.Nothing,
      subResources = Lude.Nothing,
      mitigations = Lude.Nothing,
      attackProperties = Lude.Nothing,
      attackCounters = Lude.Nothing,
      resourceARN = Lude.Nothing,
      endTime = Lude.Nothing
    }

-- | The unique identifier (ID) of the attack.
--
-- /Note:/ Consider using 'attackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAttackId :: Lens.Lens' AttackDetail (Lude.Maybe Lude.Text)
adAttackId = Lens.lens (attackId :: AttackDetail -> Lude.Maybe Lude.Text) (\s a -> s {attackId = a} :: AttackDetail)
{-# DEPRECATED adAttackId "Use generic-lens or generic-optics with 'attackId' instead." #-}

-- | The time the attack started, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adStartTime :: Lens.Lens' AttackDetail (Lude.Maybe Lude.Timestamp)
adStartTime = Lens.lens (startTime :: AttackDetail -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: AttackDetail)
{-# DEPRECATED adStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | If applicable, additional detail about the resource being attacked, for example, IP address or URL.
--
-- /Note:/ Consider using 'subResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adSubResources :: Lens.Lens' AttackDetail (Lude.Maybe [SubResourceSummary])
adSubResources = Lens.lens (subResources :: AttackDetail -> Lude.Maybe [SubResourceSummary]) (\s a -> s {subResources = a} :: AttackDetail)
{-# DEPRECATED adSubResources "Use generic-lens or generic-optics with 'subResources' instead." #-}

-- | List of mitigation actions taken for the attack.
--
-- /Note:/ Consider using 'mitigations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adMitigations :: Lens.Lens' AttackDetail (Lude.Maybe [Mitigation])
adMitigations = Lens.lens (mitigations :: AttackDetail -> Lude.Maybe [Mitigation]) (\s a -> s {mitigations = a} :: AttackDetail)
{-# DEPRECATED adMitigations "Use generic-lens or generic-optics with 'mitigations' instead." #-}

-- | The array of 'AttackProperty' objects.
--
-- /Note:/ Consider using 'attackProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAttackProperties :: Lens.Lens' AttackDetail (Lude.Maybe [AttackProperty])
adAttackProperties = Lens.lens (attackProperties :: AttackDetail -> Lude.Maybe [AttackProperty]) (\s a -> s {attackProperties = a} :: AttackDetail)
{-# DEPRECATED adAttackProperties "Use generic-lens or generic-optics with 'attackProperties' instead." #-}

-- | List of counters that describe the attack for the specified time period.
--
-- /Note:/ Consider using 'attackCounters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAttackCounters :: Lens.Lens' AttackDetail (Lude.Maybe [SummarizedCounter])
adAttackCounters = Lens.lens (attackCounters :: AttackDetail -> Lude.Maybe [SummarizedCounter]) (\s a -> s {attackCounters = a} :: AttackDetail)
{-# DEPRECATED adAttackCounters "Use generic-lens or generic-optics with 'attackCounters' instead." #-}

-- | The ARN (Amazon Resource Name) of the resource that was attacked.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adResourceARN :: Lens.Lens' AttackDetail (Lude.Maybe Lude.Text)
adResourceARN = Lens.lens (resourceARN :: AttackDetail -> Lude.Maybe Lude.Text) (\s a -> s {resourceARN = a} :: AttackDetail)
{-# DEPRECATED adResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | The time the attack ended, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adEndTime :: Lens.Lens' AttackDetail (Lude.Maybe Lude.Timestamp)
adEndTime = Lens.lens (endTime :: AttackDetail -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: AttackDetail)
{-# DEPRECATED adEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

instance Lude.FromJSON AttackDetail where
  parseJSON =
    Lude.withObject
      "AttackDetail"
      ( \x ->
          AttackDetail'
            Lude.<$> (x Lude..:? "AttackId")
            Lude.<*> (x Lude..:? "StartTime")
            Lude.<*> (x Lude..:? "SubResources" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Mitigations" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AttackProperties" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AttackCounters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ResourceArn")
            Lude.<*> (x Lude..:? "EndTime")
      )
