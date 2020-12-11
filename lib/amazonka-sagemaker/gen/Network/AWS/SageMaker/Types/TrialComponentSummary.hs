-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialComponentSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialComponentSummary
  ( TrialComponentSummary (..),

    -- * Smart constructor
    mkTrialComponentSummary,

    -- * Lenses
    tcsCreationTime,
    tcsStatus,
    tcsStartTime,
    tcsCreatedBy,
    tcsLastModifiedTime,
    tcsEndTime,
    tcsTrialComponentName,
    tcsDisplayName,
    tcsLastModifiedBy,
    tcsTrialComponentARN,
    tcsTrialComponentSource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.TrialComponentSource
import Network.AWS.SageMaker.Types.TrialComponentStatus
import Network.AWS.SageMaker.Types.UserContext

-- | A summary of the properties of a trial component. To get all the properties, call the 'DescribeTrialComponent' API and provide the @TrialComponentName@ .
--
-- /See:/ 'mkTrialComponentSummary' smart constructor.
data TrialComponentSummary = TrialComponentSummary'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    status :: Lude.Maybe TrialComponentStatus,
    startTime :: Lude.Maybe Lude.Timestamp,
    createdBy :: Lude.Maybe UserContext,
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    endTime :: Lude.Maybe Lude.Timestamp,
    trialComponentName :: Lude.Maybe Lude.Text,
    displayName :: Lude.Maybe Lude.Text,
    lastModifiedBy :: Lude.Maybe UserContext,
    trialComponentARN :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'TrialComponentSummary' with the minimum fields required to make a request.
--
-- * 'createdBy' - Who created the component.
-- * 'creationTime' - When the component was created.
-- * 'displayName' - The name of the component as displayed. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
-- * 'endTime' - When the component ended.
-- * 'lastModifiedBy' - Who last modified the component.
-- * 'lastModifiedTime' - When the component was last modified.
-- * 'startTime' - When the component started.
-- * 'status' - The status of the component. States include:
--
--
--     * InProgress
--
--
--     * Completed
--
--
--     * Failed
--
--
-- * 'trialComponentARN' - The ARN of the trial component.
-- * 'trialComponentName' - The name of the trial component.
-- * 'trialComponentSource' - Undocumented field.
mkTrialComponentSummary ::
  TrialComponentSummary
mkTrialComponentSummary =
  TrialComponentSummary'
    { creationTime = Lude.Nothing,
      status = Lude.Nothing,
      startTime = Lude.Nothing,
      createdBy = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      endTime = Lude.Nothing,
      trialComponentName = Lude.Nothing,
      displayName = Lude.Nothing,
      lastModifiedBy = Lude.Nothing,
      trialComponentARN = Lude.Nothing,
      trialComponentSource = Lude.Nothing
    }

-- | When the component was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsCreationTime :: Lens.Lens' TrialComponentSummary (Lude.Maybe Lude.Timestamp)
tcsCreationTime = Lens.lens (creationTime :: TrialComponentSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: TrialComponentSummary)
{-# DEPRECATED tcsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The status of the component. States include:
--
--
--     * InProgress
--
--
--     * Completed
--
--
--     * Failed
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsStatus :: Lens.Lens' TrialComponentSummary (Lude.Maybe TrialComponentStatus)
tcsStatus = Lens.lens (status :: TrialComponentSummary -> Lude.Maybe TrialComponentStatus) (\s a -> s {status = a} :: TrialComponentSummary)
{-# DEPRECATED tcsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | When the component started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsStartTime :: Lens.Lens' TrialComponentSummary (Lude.Maybe Lude.Timestamp)
tcsStartTime = Lens.lens (startTime :: TrialComponentSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: TrialComponentSummary)
{-# DEPRECATED tcsStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | Who created the component.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsCreatedBy :: Lens.Lens' TrialComponentSummary (Lude.Maybe UserContext)
tcsCreatedBy = Lens.lens (createdBy :: TrialComponentSummary -> Lude.Maybe UserContext) (\s a -> s {createdBy = a} :: TrialComponentSummary)
{-# DEPRECATED tcsCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | When the component was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsLastModifiedTime :: Lens.Lens' TrialComponentSummary (Lude.Maybe Lude.Timestamp)
tcsLastModifiedTime = Lens.lens (lastModifiedTime :: TrialComponentSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: TrialComponentSummary)
{-# DEPRECATED tcsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | When the component ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsEndTime :: Lens.Lens' TrialComponentSummary (Lude.Maybe Lude.Timestamp)
tcsEndTime = Lens.lens (endTime :: TrialComponentSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: TrialComponentSummary)
{-# DEPRECATED tcsEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The name of the trial component.
--
-- /Note:/ Consider using 'trialComponentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsTrialComponentName :: Lens.Lens' TrialComponentSummary (Lude.Maybe Lude.Text)
tcsTrialComponentName = Lens.lens (trialComponentName :: TrialComponentSummary -> Lude.Maybe Lude.Text) (\s a -> s {trialComponentName = a} :: TrialComponentSummary)
{-# DEPRECATED tcsTrialComponentName "Use generic-lens or generic-optics with 'trialComponentName' instead." #-}

-- | The name of the component as displayed. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsDisplayName :: Lens.Lens' TrialComponentSummary (Lude.Maybe Lude.Text)
tcsDisplayName = Lens.lens (displayName :: TrialComponentSummary -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: TrialComponentSummary)
{-# DEPRECATED tcsDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | Who last modified the component.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsLastModifiedBy :: Lens.Lens' TrialComponentSummary (Lude.Maybe UserContext)
tcsLastModifiedBy = Lens.lens (lastModifiedBy :: TrialComponentSummary -> Lude.Maybe UserContext) (\s a -> s {lastModifiedBy = a} :: TrialComponentSummary)
{-# DEPRECATED tcsLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | The ARN of the trial component.
--
-- /Note:/ Consider using 'trialComponentARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsTrialComponentARN :: Lens.Lens' TrialComponentSummary (Lude.Maybe Lude.Text)
tcsTrialComponentARN = Lens.lens (trialComponentARN :: TrialComponentSummary -> Lude.Maybe Lude.Text) (\s a -> s {trialComponentARN = a} :: TrialComponentSummary)
{-# DEPRECATED tcsTrialComponentARN "Use generic-lens or generic-optics with 'trialComponentARN' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'trialComponentSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsTrialComponentSource :: Lens.Lens' TrialComponentSummary (Lude.Maybe TrialComponentSource)
tcsTrialComponentSource = Lens.lens (trialComponentSource :: TrialComponentSummary -> Lude.Maybe TrialComponentSource) (\s a -> s {trialComponentSource = a} :: TrialComponentSummary)
{-# DEPRECATED tcsTrialComponentSource "Use generic-lens or generic-optics with 'trialComponentSource' instead." #-}

instance Lude.FromJSON TrialComponentSummary where
  parseJSON =
    Lude.withObject
      "TrialComponentSummary"
      ( \x ->
          TrialComponentSummary'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "StartTime")
            Lude.<*> (x Lude..:? "CreatedBy")
            Lude.<*> (x Lude..:? "LastModifiedTime")
            Lude.<*> (x Lude..:? "EndTime")
            Lude.<*> (x Lude..:? "TrialComponentName")
            Lude.<*> (x Lude..:? "DisplayName")
            Lude.<*> (x Lude..:? "LastModifiedBy")
            Lude.<*> (x Lude..:? "TrialComponentArn")
            Lude.<*> (x Lude..:? "TrialComponentSource")
      )
