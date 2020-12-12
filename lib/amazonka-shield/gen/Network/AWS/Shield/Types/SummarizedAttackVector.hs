{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.SummarizedAttackVector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.SummarizedAttackVector
  ( SummarizedAttackVector (..),

    -- * Smart constructor
    mkSummarizedAttackVector,

    -- * Lenses
    savVectorCounters,
    savVectorType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Shield.Types.SummarizedCounter

-- | A summary of information about the attack.
--
-- /See:/ 'mkSummarizedAttackVector' smart constructor.
data SummarizedAttackVector = SummarizedAttackVector'
  { vectorCounters ::
      Lude.Maybe [SummarizedCounter],
    vectorType :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SummarizedAttackVector' with the minimum fields required to make a request.
--
-- * 'vectorCounters' - The list of counters that describe the details of the attack.
-- * 'vectorType' - The attack type, for example, SNMP reflection or SYN flood.
mkSummarizedAttackVector ::
  -- | 'vectorType'
  Lude.Text ->
  SummarizedAttackVector
mkSummarizedAttackVector pVectorType_ =
  SummarizedAttackVector'
    { vectorCounters = Lude.Nothing,
      vectorType = pVectorType_
    }

-- | The list of counters that describe the details of the attack.
--
-- /Note:/ Consider using 'vectorCounters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
savVectorCounters :: Lens.Lens' SummarizedAttackVector (Lude.Maybe [SummarizedCounter])
savVectorCounters = Lens.lens (vectorCounters :: SummarizedAttackVector -> Lude.Maybe [SummarizedCounter]) (\s a -> s {vectorCounters = a} :: SummarizedAttackVector)
{-# DEPRECATED savVectorCounters "Use generic-lens or generic-optics with 'vectorCounters' instead." #-}

-- | The attack type, for example, SNMP reflection or SYN flood.
--
-- /Note:/ Consider using 'vectorType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
savVectorType :: Lens.Lens' SummarizedAttackVector Lude.Text
savVectorType = Lens.lens (vectorType :: SummarizedAttackVector -> Lude.Text) (\s a -> s {vectorType = a} :: SummarizedAttackVector)
{-# DEPRECATED savVectorType "Use generic-lens or generic-optics with 'vectorType' instead." #-}

instance Lude.FromJSON SummarizedAttackVector where
  parseJSON =
    Lude.withObject
      "SummarizedAttackVector"
      ( \x ->
          SummarizedAttackVector'
            Lude.<$> (x Lude..:? "VectorCounters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "VectorType")
      )
