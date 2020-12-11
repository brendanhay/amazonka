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
    expCreationTime,
    expLastModifiedTime,
    expExperimentName,
    expExperimentSource,
    expExperimentARN,
    expDisplayName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.ExperimentSource

-- | A summary of the properties of an experiment. To get the complete set of properties, call the 'DescribeExperiment' API and provide the @ExperimentName@ .
--
-- /See:/ 'mkExperimentSummary' smart constructor.
data ExperimentSummary = ExperimentSummary'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    experimentName :: Lude.Maybe Lude.Text,
    experimentSource :: Lude.Maybe ExperimentSource,
    experimentARN :: Lude.Maybe Lude.Text,
    displayName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExperimentSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - When the experiment was created.
-- * 'displayName' - The name of the experiment as displayed. If @DisplayName@ isn't specified, @ExperimentName@ is displayed.
-- * 'experimentARN' - The Amazon Resource Name (ARN) of the experiment.
-- * 'experimentName' - The name of the experiment.
-- * 'experimentSource' - Undocumented field.
-- * 'lastModifiedTime' - When the experiment was last modified.
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
expCreationTime :: Lens.Lens' ExperimentSummary (Lude.Maybe Lude.Timestamp)
expCreationTime = Lens.lens (creationTime :: ExperimentSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: ExperimentSummary)
{-# DEPRECATED expCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | When the experiment was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
expLastModifiedTime :: Lens.Lens' ExperimentSummary (Lude.Maybe Lude.Timestamp)
expLastModifiedTime = Lens.lens (lastModifiedTime :: ExperimentSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: ExperimentSummary)
{-# DEPRECATED expLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The name of the experiment.
--
-- /Note:/ Consider using 'experimentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
expExperimentName :: Lens.Lens' ExperimentSummary (Lude.Maybe Lude.Text)
expExperimentName = Lens.lens (experimentName :: ExperimentSummary -> Lude.Maybe Lude.Text) (\s a -> s {experimentName = a} :: ExperimentSummary)
{-# DEPRECATED expExperimentName "Use generic-lens or generic-optics with 'experimentName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'experimentSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
expExperimentSource :: Lens.Lens' ExperimentSummary (Lude.Maybe ExperimentSource)
expExperimentSource = Lens.lens (experimentSource :: ExperimentSummary -> Lude.Maybe ExperimentSource) (\s a -> s {experimentSource = a} :: ExperimentSummary)
{-# DEPRECATED expExperimentSource "Use generic-lens or generic-optics with 'experimentSource' instead." #-}

-- | The Amazon Resource Name (ARN) of the experiment.
--
-- /Note:/ Consider using 'experimentARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
expExperimentARN :: Lens.Lens' ExperimentSummary (Lude.Maybe Lude.Text)
expExperimentARN = Lens.lens (experimentARN :: ExperimentSummary -> Lude.Maybe Lude.Text) (\s a -> s {experimentARN = a} :: ExperimentSummary)
{-# DEPRECATED expExperimentARN "Use generic-lens or generic-optics with 'experimentARN' instead." #-}

-- | The name of the experiment as displayed. If @DisplayName@ isn't specified, @ExperimentName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
expDisplayName :: Lens.Lens' ExperimentSummary (Lude.Maybe Lude.Text)
expDisplayName = Lens.lens (displayName :: ExperimentSummary -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: ExperimentSummary)
{-# DEPRECATED expDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

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
