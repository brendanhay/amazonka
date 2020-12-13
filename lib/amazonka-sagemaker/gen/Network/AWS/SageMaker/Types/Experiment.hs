{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Experiment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Experiment
  ( Experiment (..),

    -- * Smart constructor
    mkExperiment,

    -- * Lenses
    eCreationTime,
    eCreatedBy,
    eLastModifiedTime,
    eExperimentName,
    eExperimentARN,
    eSource,
    eDisplayName,
    eLastModifiedBy,
    eDescription,
    eTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.ExperimentSource
import Network.AWS.SageMaker.Types.Tag
import Network.AWS.SageMaker.Types.UserContext

-- | The properties of an experiment as returned by the 'Search' API.
--
-- /See:/ 'mkExperiment' smart constructor.
data Experiment = Experiment'
  { -- | When the experiment was created.
    creationTime :: Lude.Maybe Lude.Timestamp,
    createdBy :: Lude.Maybe UserContext,
    -- | When the experiment was last modified.
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    -- | The name of the experiment.
    experimentName :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the experiment.
    experimentARN :: Lude.Maybe Lude.Text,
    source :: Lude.Maybe ExperimentSource,
    -- | The name of the experiment as displayed. If @DisplayName@ isn't specified, @ExperimentName@ is displayed.
    displayName :: Lude.Maybe Lude.Text,
    lastModifiedBy :: Lude.Maybe UserContext,
    -- | The description of the experiment.
    description :: Lude.Maybe Lude.Text,
    -- | The list of tags that are associated with the experiment. You can use 'Search' API to search on the tags.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Experiment' with the minimum fields required to make a request.
--
-- * 'creationTime' - When the experiment was created.
-- * 'createdBy' -
-- * 'lastModifiedTime' - When the experiment was last modified.
-- * 'experimentName' - The name of the experiment.
-- * 'experimentARN' - The Amazon Resource Name (ARN) of the experiment.
-- * 'source' -
-- * 'displayName' - The name of the experiment as displayed. If @DisplayName@ isn't specified, @ExperimentName@ is displayed.
-- * 'lastModifiedBy' -
-- * 'description' - The description of the experiment.
-- * 'tags' - The list of tags that are associated with the experiment. You can use 'Search' API to search on the tags.
mkExperiment ::
  Experiment
mkExperiment =
  Experiment'
    { creationTime = Lude.Nothing,
      createdBy = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      experimentName = Lude.Nothing,
      experimentARN = Lude.Nothing,
      source = Lude.Nothing,
      displayName = Lude.Nothing,
      lastModifiedBy = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | When the experiment was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eCreationTime :: Lens.Lens' Experiment (Lude.Maybe Lude.Timestamp)
eCreationTime = Lens.lens (creationTime :: Experiment -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: Experiment)
{-# DEPRECATED eCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eCreatedBy :: Lens.Lens' Experiment (Lude.Maybe UserContext)
eCreatedBy = Lens.lens (createdBy :: Experiment -> Lude.Maybe UserContext) (\s a -> s {createdBy = a} :: Experiment)
{-# DEPRECATED eCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | When the experiment was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eLastModifiedTime :: Lens.Lens' Experiment (Lude.Maybe Lude.Timestamp)
eLastModifiedTime = Lens.lens (lastModifiedTime :: Experiment -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: Experiment)
{-# DEPRECATED eLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The name of the experiment.
--
-- /Note:/ Consider using 'experimentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eExperimentName :: Lens.Lens' Experiment (Lude.Maybe Lude.Text)
eExperimentName = Lens.lens (experimentName :: Experiment -> Lude.Maybe Lude.Text) (\s a -> s {experimentName = a} :: Experiment)
{-# DEPRECATED eExperimentName "Use generic-lens or generic-optics with 'experimentName' instead." #-}

-- | The Amazon Resource Name (ARN) of the experiment.
--
-- /Note:/ Consider using 'experimentARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eExperimentARN :: Lens.Lens' Experiment (Lude.Maybe Lude.Text)
eExperimentARN = Lens.lens (experimentARN :: Experiment -> Lude.Maybe Lude.Text) (\s a -> s {experimentARN = a} :: Experiment)
{-# DEPRECATED eExperimentARN "Use generic-lens or generic-optics with 'experimentARN' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSource :: Lens.Lens' Experiment (Lude.Maybe ExperimentSource)
eSource = Lens.lens (source :: Experiment -> Lude.Maybe ExperimentSource) (\s a -> s {source = a} :: Experiment)
{-# DEPRECATED eSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The name of the experiment as displayed. If @DisplayName@ isn't specified, @ExperimentName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDisplayName :: Lens.Lens' Experiment (Lude.Maybe Lude.Text)
eDisplayName = Lens.lens (displayName :: Experiment -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: Experiment)
{-# DEPRECATED eDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eLastModifiedBy :: Lens.Lens' Experiment (Lude.Maybe UserContext)
eLastModifiedBy = Lens.lens (lastModifiedBy :: Experiment -> Lude.Maybe UserContext) (\s a -> s {lastModifiedBy = a} :: Experiment)
{-# DEPRECATED eLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | The description of the experiment.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDescription :: Lens.Lens' Experiment (Lude.Maybe Lude.Text)
eDescription = Lens.lens (description :: Experiment -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Experiment)
{-# DEPRECATED eDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The list of tags that are associated with the experiment. You can use 'Search' API to search on the tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eTags :: Lens.Lens' Experiment (Lude.Maybe [Tag])
eTags = Lens.lens (tags :: Experiment -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: Experiment)
{-# DEPRECATED eTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON Experiment where
  parseJSON =
    Lude.withObject
      "Experiment"
      ( \x ->
          Experiment'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "CreatedBy")
            Lude.<*> (x Lude..:? "LastModifiedTime")
            Lude.<*> (x Lude..:? "ExperimentName")
            Lude.<*> (x Lude..:? "ExperimentArn")
            Lude.<*> (x Lude..:? "Source")
            Lude.<*> (x Lude..:? "DisplayName")
            Lude.<*> (x Lude..:? "LastModifiedBy")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "Tags" Lude..!= Lude.mempty)
      )
