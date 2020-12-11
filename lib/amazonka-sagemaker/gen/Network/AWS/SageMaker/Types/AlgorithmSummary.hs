-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AlgorithmSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AlgorithmSummary
  ( AlgorithmSummary (..),

    -- * Smart constructor
    mkAlgorithmSummary,

    -- * Lenses
    aAlgorithmDescription,
    aAlgorithmName,
    aAlgorithmARN,
    aCreationTime,
    aAlgorithmStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.AlgorithmStatus

-- | Provides summary information about an algorithm.
--
-- /See:/ 'mkAlgorithmSummary' smart constructor.
data AlgorithmSummary = AlgorithmSummary'
  { algorithmDescription ::
      Lude.Maybe Lude.Text,
    algorithmName :: Lude.Text,
    algorithmARN :: Lude.Text,
    creationTime :: Lude.Timestamp,
    algorithmStatus :: AlgorithmStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AlgorithmSummary' with the minimum fields required to make a request.
--
-- * 'algorithmARN' - The Amazon Resource Name (ARN) of the algorithm.
-- * 'algorithmDescription' - A brief description of the algorithm.
-- * 'algorithmName' - The name of the algorithm that is described by the summary.
-- * 'algorithmStatus' - The overall status of the algorithm.
-- * 'creationTime' - A timestamp that shows when the algorithm was created.
mkAlgorithmSummary ::
  -- | 'algorithmName'
  Lude.Text ->
  -- | 'algorithmARN'
  Lude.Text ->
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'algorithmStatus'
  AlgorithmStatus ->
  AlgorithmSummary
mkAlgorithmSummary
  pAlgorithmName_
  pAlgorithmARN_
  pCreationTime_
  pAlgorithmStatus_ =
    AlgorithmSummary'
      { algorithmDescription = Lude.Nothing,
        algorithmName = pAlgorithmName_,
        algorithmARN = pAlgorithmARN_,
        creationTime = pCreationTime_,
        algorithmStatus = pAlgorithmStatus_
      }

-- | A brief description of the algorithm.
--
-- /Note:/ Consider using 'algorithmDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAlgorithmDescription :: Lens.Lens' AlgorithmSummary (Lude.Maybe Lude.Text)
aAlgorithmDescription = Lens.lens (algorithmDescription :: AlgorithmSummary -> Lude.Maybe Lude.Text) (\s a -> s {algorithmDescription = a} :: AlgorithmSummary)
{-# DEPRECATED aAlgorithmDescription "Use generic-lens or generic-optics with 'algorithmDescription' instead." #-}

-- | The name of the algorithm that is described by the summary.
--
-- /Note:/ Consider using 'algorithmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAlgorithmName :: Lens.Lens' AlgorithmSummary Lude.Text
aAlgorithmName = Lens.lens (algorithmName :: AlgorithmSummary -> Lude.Text) (\s a -> s {algorithmName = a} :: AlgorithmSummary)
{-# DEPRECATED aAlgorithmName "Use generic-lens or generic-optics with 'algorithmName' instead." #-}

-- | The Amazon Resource Name (ARN) of the algorithm.
--
-- /Note:/ Consider using 'algorithmARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAlgorithmARN :: Lens.Lens' AlgorithmSummary Lude.Text
aAlgorithmARN = Lens.lens (algorithmARN :: AlgorithmSummary -> Lude.Text) (\s a -> s {algorithmARN = a} :: AlgorithmSummary)
{-# DEPRECATED aAlgorithmARN "Use generic-lens or generic-optics with 'algorithmARN' instead." #-}

-- | A timestamp that shows when the algorithm was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCreationTime :: Lens.Lens' AlgorithmSummary Lude.Timestamp
aCreationTime = Lens.lens (creationTime :: AlgorithmSummary -> Lude.Timestamp) (\s a -> s {creationTime = a} :: AlgorithmSummary)
{-# DEPRECATED aCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The overall status of the algorithm.
--
-- /Note:/ Consider using 'algorithmStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAlgorithmStatus :: Lens.Lens' AlgorithmSummary AlgorithmStatus
aAlgorithmStatus = Lens.lens (algorithmStatus :: AlgorithmSummary -> AlgorithmStatus) (\s a -> s {algorithmStatus = a} :: AlgorithmSummary)
{-# DEPRECATED aAlgorithmStatus "Use generic-lens or generic-optics with 'algorithmStatus' instead." #-}

instance Lude.FromJSON AlgorithmSummary where
  parseJSON =
    Lude.withObject
      "AlgorithmSummary"
      ( \x ->
          AlgorithmSummary'
            Lude.<$> (x Lude..:? "AlgorithmDescription")
            Lude.<*> (x Lude..: "AlgorithmName")
            Lude.<*> (x Lude..: "AlgorithmArn")
            Lude.<*> (x Lude..: "CreationTime")
            Lude.<*> (x Lude..: "AlgorithmStatus")
      )
