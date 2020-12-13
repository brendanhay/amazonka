{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.SubResourceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.SubResourceSummary
  ( SubResourceSummary (..),

    -- * Smart constructor
    mkSubResourceSummary,

    -- * Lenses
    srsCounters,
    srsAttackVectors,
    srsId,
    srsType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Shield.Types.SubResourceType
import Network.AWS.Shield.Types.SummarizedAttackVector
import Network.AWS.Shield.Types.SummarizedCounter

-- | The attack information for the specified SubResource.
--
-- /See:/ 'mkSubResourceSummary' smart constructor.
data SubResourceSummary = SubResourceSummary'
  { -- | The counters that describe the details of the attack.
    counters :: Lude.Maybe [SummarizedCounter],
    -- | The list of attack types and associated counters.
    attackVectors :: Lude.Maybe [SummarizedAttackVector],
    -- | The unique identifier (ID) of the @SubResource@ .
    id :: Lude.Maybe Lude.Text,
    -- | The @SubResource@ type.
    type' :: Lude.Maybe SubResourceType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SubResourceSummary' with the minimum fields required to make a request.
--
-- * 'counters' - The counters that describe the details of the attack.
-- * 'attackVectors' - The list of attack types and associated counters.
-- * 'id' - The unique identifier (ID) of the @SubResource@ .
-- * 'type'' - The @SubResource@ type.
mkSubResourceSummary ::
  SubResourceSummary
mkSubResourceSummary =
  SubResourceSummary'
    { counters = Lude.Nothing,
      attackVectors = Lude.Nothing,
      id = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The counters that describe the details of the attack.
--
-- /Note:/ Consider using 'counters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsCounters :: Lens.Lens' SubResourceSummary (Lude.Maybe [SummarizedCounter])
srsCounters = Lens.lens (counters :: SubResourceSummary -> Lude.Maybe [SummarizedCounter]) (\s a -> s {counters = a} :: SubResourceSummary)
{-# DEPRECATED srsCounters "Use generic-lens or generic-optics with 'counters' instead." #-}

-- | The list of attack types and associated counters.
--
-- /Note:/ Consider using 'attackVectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsAttackVectors :: Lens.Lens' SubResourceSummary (Lude.Maybe [SummarizedAttackVector])
srsAttackVectors = Lens.lens (attackVectors :: SubResourceSummary -> Lude.Maybe [SummarizedAttackVector]) (\s a -> s {attackVectors = a} :: SubResourceSummary)
{-# DEPRECATED srsAttackVectors "Use generic-lens or generic-optics with 'attackVectors' instead." #-}

-- | The unique identifier (ID) of the @SubResource@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsId :: Lens.Lens' SubResourceSummary (Lude.Maybe Lude.Text)
srsId = Lens.lens (id :: SubResourceSummary -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: SubResourceSummary)
{-# DEPRECATED srsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The @SubResource@ type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsType :: Lens.Lens' SubResourceSummary (Lude.Maybe SubResourceType)
srsType = Lens.lens (type' :: SubResourceSummary -> Lude.Maybe SubResourceType) (\s a -> s {type' = a} :: SubResourceSummary)
{-# DEPRECATED srsType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON SubResourceSummary where
  parseJSON =
    Lude.withObject
      "SubResourceSummary"
      ( \x ->
          SubResourceSummary'
            Lude.<$> (x Lude..:? "Counters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AttackVectors" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Type")
      )
