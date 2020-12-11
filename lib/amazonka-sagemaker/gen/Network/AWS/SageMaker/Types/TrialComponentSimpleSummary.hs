-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialComponentSimpleSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialComponentSimpleSummary
  ( TrialComponentSimpleSummary (..),

    -- * Smart constructor
    mkTrialComponentSimpleSummary,

    -- * Lenses
    tcssCreationTime,
    tcssCreatedBy,
    tcssTrialComponentName,
    tcssTrialComponentARN,
    tcssTrialComponentSource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.TrialComponentSource
import Network.AWS.SageMaker.Types.UserContext

-- | A short summary of a trial component.
--
-- /See:/ 'mkTrialComponentSimpleSummary' smart constructor.
data TrialComponentSimpleSummary = TrialComponentSimpleSummary'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    createdBy :: Lude.Maybe UserContext,
    trialComponentName ::
      Lude.Maybe Lude.Text,
    trialComponentARN ::
      Lude.Maybe Lude.Text,
    trialComponentSource ::
      Lude.Maybe TrialComponentSource
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrialComponentSimpleSummary' with the minimum fields required to make a request.
--
-- * 'createdBy' - Undocumented field.
-- * 'creationTime' - When the component was created.
-- * 'trialComponentARN' - The Amazon Resource Name (ARN) of the trial component.
-- * 'trialComponentName' - The name of the trial component.
-- * 'trialComponentSource' - Undocumented field.
mkTrialComponentSimpleSummary ::
  TrialComponentSimpleSummary
mkTrialComponentSimpleSummary =
  TrialComponentSimpleSummary'
    { creationTime = Lude.Nothing,
      createdBy = Lude.Nothing,
      trialComponentName = Lude.Nothing,
      trialComponentARN = Lude.Nothing,
      trialComponentSource = Lude.Nothing
    }

-- | When the component was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcssCreationTime :: Lens.Lens' TrialComponentSimpleSummary (Lude.Maybe Lude.Timestamp)
tcssCreationTime = Lens.lens (creationTime :: TrialComponentSimpleSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: TrialComponentSimpleSummary)
{-# DEPRECATED tcssCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcssCreatedBy :: Lens.Lens' TrialComponentSimpleSummary (Lude.Maybe UserContext)
tcssCreatedBy = Lens.lens (createdBy :: TrialComponentSimpleSummary -> Lude.Maybe UserContext) (\s a -> s {createdBy = a} :: TrialComponentSimpleSummary)
{-# DEPRECATED tcssCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | The name of the trial component.
--
-- /Note:/ Consider using 'trialComponentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcssTrialComponentName :: Lens.Lens' TrialComponentSimpleSummary (Lude.Maybe Lude.Text)
tcssTrialComponentName = Lens.lens (trialComponentName :: TrialComponentSimpleSummary -> Lude.Maybe Lude.Text) (\s a -> s {trialComponentName = a} :: TrialComponentSimpleSummary)
{-# DEPRECATED tcssTrialComponentName "Use generic-lens or generic-optics with 'trialComponentName' instead." #-}

-- | The Amazon Resource Name (ARN) of the trial component.
--
-- /Note:/ Consider using 'trialComponentARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcssTrialComponentARN :: Lens.Lens' TrialComponentSimpleSummary (Lude.Maybe Lude.Text)
tcssTrialComponentARN = Lens.lens (trialComponentARN :: TrialComponentSimpleSummary -> Lude.Maybe Lude.Text) (\s a -> s {trialComponentARN = a} :: TrialComponentSimpleSummary)
{-# DEPRECATED tcssTrialComponentARN "Use generic-lens or generic-optics with 'trialComponentARN' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'trialComponentSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcssTrialComponentSource :: Lens.Lens' TrialComponentSimpleSummary (Lude.Maybe TrialComponentSource)
tcssTrialComponentSource = Lens.lens (trialComponentSource :: TrialComponentSimpleSummary -> Lude.Maybe TrialComponentSource) (\s a -> s {trialComponentSource = a} :: TrialComponentSimpleSummary)
{-# DEPRECATED tcssTrialComponentSource "Use generic-lens or generic-optics with 'trialComponentSource' instead." #-}

instance Lude.FromJSON TrialComponentSimpleSummary where
  parseJSON =
    Lude.withObject
      "TrialComponentSimpleSummary"
      ( \x ->
          TrialComponentSimpleSummary'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "CreatedBy")
            Lude.<*> (x Lude..:? "TrialComponentName")
            Lude.<*> (x Lude..:? "TrialComponentArn")
            Lude.<*> (x Lude..:? "TrialComponentSource")
      )
