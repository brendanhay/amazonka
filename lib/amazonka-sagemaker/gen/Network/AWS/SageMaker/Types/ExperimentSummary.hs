{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ExperimentSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ExperimentSummary
  ( ExperimentSummary (..),

    -- * Smart constructor
    mkExperimentSummary,

    -- * Lenses
    esfCreationTime,
    esfLastModifiedTime,
    esfExperimentName,
    esfExperimentSource,
    esfExperimentARN,
    esfDisplayName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.ExperimentSource

-- | A summary of the properties of an experiment. To get the complete set of properties, call the 'DescribeExperiment' API and provide the @ExperimentName@ .
--
-- /See:/ 'mkExperimentSummary' smart constructor.
data ExperimentSummary = ExperimentSummary'
  { -- | When the experiment was created.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | When the experiment was last modified.
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    -- | The name of the experiment.
    experimentName :: Lude.Maybe Lude.Text,
    experimentSource :: Lude.Maybe ExperimentSource,
    -- | The Amazon Resource Name (ARN) of the experiment.
    experimentARN :: Lude.Maybe Lude.Text,
    -- | The name of the experiment as displayed. If @DisplayName@ isn't specified, @ExperimentName@ is displayed.
    displayName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExperimentSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - When the experiment was created.
-- * 'lastModifiedTime' - When the experiment was last modified.
-- * 'experimentName' - The name of the experiment.
-- * 'experimentSource' -
-- * 'experimentARN' - The Amazon Resource Name (ARN) of the experiment.
-- * 'displayName' - The name of the experiment as displayed. If @DisplayName@ isn't specified, @ExperimentName@ is displayed.
mkExperimentSummary ::
  ExperimentSummary
mkExperimentSummary =
  ExperimentSummary'
    { creationTime = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      experimentName = Lude.Nothing,
      experimentSource = Lude.Nothing,
      experimentARN = Lude.Nothing,
      displayName = Lude.Nothing
    }

-- | When the experiment was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esfCreationTime :: Lens.Lens' ExperimentSummary (Lude.Maybe Lude.Timestamp)
esfCreationTime = Lens.lens (creationTime :: ExperimentSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: ExperimentSummary)
{-# DEPRECATED esfCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | When the experiment was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esfLastModifiedTime :: Lens.Lens' ExperimentSummary (Lude.Maybe Lude.Timestamp)
esfLastModifiedTime = Lens.lens (lastModifiedTime :: ExperimentSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: ExperimentSummary)
{-# DEPRECATED esfLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The name of the experiment.
--
-- /Note:/ Consider using 'experimentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esfExperimentName :: Lens.Lens' ExperimentSummary (Lude.Maybe Lude.Text)
esfExperimentName = Lens.lens (experimentName :: ExperimentSummary -> Lude.Maybe Lude.Text) (\s a -> s {experimentName = a} :: ExperimentSummary)
{-# DEPRECATED esfExperimentName "Use generic-lens or generic-optics with 'experimentName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'experimentSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esfExperimentSource :: Lens.Lens' ExperimentSummary (Lude.Maybe ExperimentSource)
esfExperimentSource = Lens.lens (experimentSource :: ExperimentSummary -> Lude.Maybe ExperimentSource) (\s a -> s {experimentSource = a} :: ExperimentSummary)
{-# DEPRECATED esfExperimentSource "Use generic-lens or generic-optics with 'experimentSource' instead." #-}

-- | The Amazon Resource Name (ARN) of the experiment.
--
-- /Note:/ Consider using 'experimentARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esfExperimentARN :: Lens.Lens' ExperimentSummary (Lude.Maybe Lude.Text)
esfExperimentARN = Lens.lens (experimentARN :: ExperimentSummary -> Lude.Maybe Lude.Text) (\s a -> s {experimentARN = a} :: ExperimentSummary)
{-# DEPRECATED esfExperimentARN "Use generic-lens or generic-optics with 'experimentARN' instead." #-}

-- | The name of the experiment as displayed. If @DisplayName@ isn't specified, @ExperimentName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esfDisplayName :: Lens.Lens' ExperimentSummary (Lude.Maybe Lude.Text)
esfDisplayName = Lens.lens (displayName :: ExperimentSummary -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: ExperimentSummary)
{-# DEPRECATED esfDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

instance Lude.FromJSON ExperimentSummary where
  parseJSON =
    Lude.withObject
      "ExperimentSummary"
      ( \x ->
          ExperimentSummary'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "LastModifiedTime")
            Lude.<*> (x Lude..:? "ExperimentName")
            Lude.<*> (x Lude..:? "ExperimentSource")
            Lude.<*> (x Lude..:? "ExperimentArn")
            Lude.<*> (x Lude..:? "DisplayName")
      )
