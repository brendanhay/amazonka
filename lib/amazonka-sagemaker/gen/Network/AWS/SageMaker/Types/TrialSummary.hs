-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialSummary
  ( TrialSummary (..),

    -- * Smart constructor
    mkTrialSummary,

    -- * Lenses
    tsCreationTime,
    tsTrialARN,
    tsLastModifiedTime,
    tsTrialSource,
    tsDisplayName,
    tsTrialName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.TrialSource

-- | A summary of the properties of a trial. To get the complete set of properties, call the 'DescribeTrial' API and provide the @TrialName@ .
--
-- /See:/ 'mkTrialSummary' smart constructor.
data TrialSummary = TrialSummary'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    trialARN :: Lude.Maybe Lude.Text,
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    trialSource :: Lude.Maybe TrialSource,
    displayName :: Lude.Maybe Lude.Text,
    trialName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrialSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - When the trial was created.
-- * 'displayName' - The name of the trial as displayed. If @DisplayName@ isn't specified, @TrialName@ is displayed.
-- * 'lastModifiedTime' - When the trial was last modified.
-- * 'trialARN' - The Amazon Resource Name (ARN) of the trial.
-- * 'trialName' - The name of the trial.
-- * 'trialSource' - Undocumented field.
mkTrialSummary ::
  TrialSummary
mkTrialSummary =
  TrialSummary'
    { creationTime = Lude.Nothing,
      trialARN = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      trialSource = Lude.Nothing,
      displayName = Lude.Nothing,
      trialName = Lude.Nothing
    }

-- | When the trial was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsCreationTime :: Lens.Lens' TrialSummary (Lude.Maybe Lude.Timestamp)
tsCreationTime = Lens.lens (creationTime :: TrialSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: TrialSummary)
{-# DEPRECATED tsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the trial.
--
-- /Note:/ Consider using 'trialARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsTrialARN :: Lens.Lens' TrialSummary (Lude.Maybe Lude.Text)
tsTrialARN = Lens.lens (trialARN :: TrialSummary -> Lude.Maybe Lude.Text) (\s a -> s {trialARN = a} :: TrialSummary)
{-# DEPRECATED tsTrialARN "Use generic-lens or generic-optics with 'trialARN' instead." #-}

-- | When the trial was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsLastModifiedTime :: Lens.Lens' TrialSummary (Lude.Maybe Lude.Timestamp)
tsLastModifiedTime = Lens.lens (lastModifiedTime :: TrialSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: TrialSummary)
{-# DEPRECATED tsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'trialSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsTrialSource :: Lens.Lens' TrialSummary (Lude.Maybe TrialSource)
tsTrialSource = Lens.lens (trialSource :: TrialSummary -> Lude.Maybe TrialSource) (\s a -> s {trialSource = a} :: TrialSummary)
{-# DEPRECATED tsTrialSource "Use generic-lens or generic-optics with 'trialSource' instead." #-}

-- | The name of the trial as displayed. If @DisplayName@ isn't specified, @TrialName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsDisplayName :: Lens.Lens' TrialSummary (Lude.Maybe Lude.Text)
tsDisplayName = Lens.lens (displayName :: TrialSummary -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: TrialSummary)
{-# DEPRECATED tsDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The name of the trial.
--
-- /Note:/ Consider using 'trialName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsTrialName :: Lens.Lens' TrialSummary (Lude.Maybe Lude.Text)
tsTrialName = Lens.lens (trialName :: TrialSummary -> Lude.Maybe Lude.Text) (\s a -> s {trialName = a} :: TrialSummary)
{-# DEPRECATED tsTrialName "Use generic-lens or generic-optics with 'trialName' instead." #-}

instance Lude.FromJSON TrialSummary where
  parseJSON =
    Lude.withObject
      "TrialSummary"
      ( \x ->
          TrialSummary'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "TrialArn")
            Lude.<*> (x Lude..:? "LastModifiedTime")
            Lude.<*> (x Lude..:? "TrialSource")
            Lude.<*> (x Lude..:? "DisplayName")
            Lude.<*> (x Lude..:? "TrialName")
      )
