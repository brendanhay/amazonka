{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Trial
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Trial
  ( Trial (..),

    -- * Smart constructor
    mkTrial,

    -- * Lenses
    tCreationTime,
    tTrialComponentSummaries,
    tTrialARN,
    tCreatedBy,
    tLastModifiedTime,
    tExperimentName,
    tSource,
    tDisplayName,
    tTrialName,
    tLastModifiedBy,
    tTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.Tag
import Network.AWS.SageMaker.Types.TrialComponentSimpleSummary
import Network.AWS.SageMaker.Types.TrialSource
import Network.AWS.SageMaker.Types.UserContext

-- | The properties of a trial as returned by the 'Search' API.
--
-- /See:/ 'mkTrial' smart constructor.
data Trial = Trial'
  { -- | When the trial was created.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | A list of the components associated with the trial. For each component, a summary of the component's properties is included.
    trialComponentSummaries :: Lude.Maybe [TrialComponentSimpleSummary],
    -- | The Amazon Resource Name (ARN) of the trial.
    trialARN :: Lude.Maybe Lude.Text,
    createdBy :: Lude.Maybe UserContext,
    -- | Who last modified the trial.
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    -- | The name of the experiment the trial is part of.
    experimentName :: Lude.Maybe Lude.Text,
    source :: Lude.Maybe TrialSource,
    -- | The name of the trial as displayed. If @DisplayName@ isn't specified, @TrialName@ is displayed.
    displayName :: Lude.Maybe Lude.Text,
    -- | The name of the trial.
    trialName :: Lude.Maybe Lude.Text,
    lastModifiedBy :: Lude.Maybe UserContext,
    -- | The list of tags that are associated with the trial. You can use 'Search' API to search on the tags.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Trial' with the minimum fields required to make a request.
--
-- * 'creationTime' - When the trial was created.
-- * 'trialComponentSummaries' - A list of the components associated with the trial. For each component, a summary of the component's properties is included.
-- * 'trialARN' - The Amazon Resource Name (ARN) of the trial.
-- * 'createdBy' -
-- * 'lastModifiedTime' - Who last modified the trial.
-- * 'experimentName' - The name of the experiment the trial is part of.
-- * 'source' -
-- * 'displayName' - The name of the trial as displayed. If @DisplayName@ isn't specified, @TrialName@ is displayed.
-- * 'trialName' - The name of the trial.
-- * 'lastModifiedBy' -
-- * 'tags' - The list of tags that are associated with the trial. You can use 'Search' API to search on the tags.
mkTrial ::
  Trial
mkTrial =
  Trial'
    { creationTime = Lude.Nothing,
      trialComponentSummaries = Lude.Nothing,
      trialARN = Lude.Nothing,
      createdBy = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      experimentName = Lude.Nothing,
      source = Lude.Nothing,
      displayName = Lude.Nothing,
      trialName = Lude.Nothing,
      lastModifiedBy = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | When the trial was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tCreationTime :: Lens.Lens' Trial (Lude.Maybe Lude.Timestamp)
tCreationTime = Lens.lens (creationTime :: Trial -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: Trial)
{-# DEPRECATED tCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | A list of the components associated with the trial. For each component, a summary of the component's properties is included.
--
-- /Note:/ Consider using 'trialComponentSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTrialComponentSummaries :: Lens.Lens' Trial (Lude.Maybe [TrialComponentSimpleSummary])
tTrialComponentSummaries = Lens.lens (trialComponentSummaries :: Trial -> Lude.Maybe [TrialComponentSimpleSummary]) (\s a -> s {trialComponentSummaries = a} :: Trial)
{-# DEPRECATED tTrialComponentSummaries "Use generic-lens or generic-optics with 'trialComponentSummaries' instead." #-}

-- | The Amazon Resource Name (ARN) of the trial.
--
-- /Note:/ Consider using 'trialARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTrialARN :: Lens.Lens' Trial (Lude.Maybe Lude.Text)
tTrialARN = Lens.lens (trialARN :: Trial -> Lude.Maybe Lude.Text) (\s a -> s {trialARN = a} :: Trial)
{-# DEPRECATED tTrialARN "Use generic-lens or generic-optics with 'trialARN' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tCreatedBy :: Lens.Lens' Trial (Lude.Maybe UserContext)
tCreatedBy = Lens.lens (createdBy :: Trial -> Lude.Maybe UserContext) (\s a -> s {createdBy = a} :: Trial)
{-# DEPRECATED tCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | Who last modified the trial.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tLastModifiedTime :: Lens.Lens' Trial (Lude.Maybe Lude.Timestamp)
tLastModifiedTime = Lens.lens (lastModifiedTime :: Trial -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: Trial)
{-# DEPRECATED tLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The name of the experiment the trial is part of.
--
-- /Note:/ Consider using 'experimentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tExperimentName :: Lens.Lens' Trial (Lude.Maybe Lude.Text)
tExperimentName = Lens.lens (experimentName :: Trial -> Lude.Maybe Lude.Text) (\s a -> s {experimentName = a} :: Trial)
{-# DEPRECATED tExperimentName "Use generic-lens or generic-optics with 'experimentName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tSource :: Lens.Lens' Trial (Lude.Maybe TrialSource)
tSource = Lens.lens (source :: Trial -> Lude.Maybe TrialSource) (\s a -> s {source = a} :: Trial)
{-# DEPRECATED tSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The name of the trial as displayed. If @DisplayName@ isn't specified, @TrialName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tDisplayName :: Lens.Lens' Trial (Lude.Maybe Lude.Text)
tDisplayName = Lens.lens (displayName :: Trial -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: Trial)
{-# DEPRECATED tDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The name of the trial.
--
-- /Note:/ Consider using 'trialName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTrialName :: Lens.Lens' Trial (Lude.Maybe Lude.Text)
tTrialName = Lens.lens (trialName :: Trial -> Lude.Maybe Lude.Text) (\s a -> s {trialName = a} :: Trial)
{-# DEPRECATED tTrialName "Use generic-lens or generic-optics with 'trialName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tLastModifiedBy :: Lens.Lens' Trial (Lude.Maybe UserContext)
tLastModifiedBy = Lens.lens (lastModifiedBy :: Trial -> Lude.Maybe UserContext) (\s a -> s {lastModifiedBy = a} :: Trial)
{-# DEPRECATED tLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | The list of tags that are associated with the trial. You can use 'Search' API to search on the tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTags :: Lens.Lens' Trial (Lude.Maybe [Tag])
tTags = Lens.lens (tags :: Trial -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: Trial)
{-# DEPRECATED tTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON Trial where
  parseJSON =
    Lude.withObject
      "Trial"
      ( \x ->
          Trial'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "TrialComponentSummaries" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "TrialArn")
            Lude.<*> (x Lude..:? "CreatedBy")
            Lude.<*> (x Lude..:? "LastModifiedTime")
            Lude.<*> (x Lude..:? "ExperimentName")
            Lude.<*> (x Lude..:? "Source")
            Lude.<*> (x Lude..:? "DisplayName")
            Lude.<*> (x Lude..:? "TrialName")
            Lude.<*> (x Lude..:? "LastModifiedBy")
            Lude.<*> (x Lude..:? "Tags" Lude..!= Lude.mempty)
      )
