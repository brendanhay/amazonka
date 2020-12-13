{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeTrial
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of a trial's properties.
module Network.AWS.SageMaker.DescribeTrial
  ( -- * Creating a request
    DescribeTrial (..),
    mkDescribeTrial,

    -- ** Request lenses
    dTrialName,

    -- * Destructuring the response
    DescribeTrialResponse (..),
    mkDescribeTrialResponse,

    -- ** Response lenses
    dtrsCreationTime,
    dtrsTrialARN,
    dtrsCreatedBy,
    dtrsLastModifiedTime,
    dtrsExperimentName,
    dtrsSource,
    dtrsDisplayName,
    dtrsTrialName,
    dtrsLastModifiedBy,
    dtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeTrial' smart constructor.
newtype DescribeTrial = DescribeTrial'
  { -- | The name of the trial to describe.
    trialName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTrial' with the minimum fields required to make a request.
--
-- * 'trialName' - The name of the trial to describe.
mkDescribeTrial ::
  -- | 'trialName'
  Lude.Text ->
  DescribeTrial
mkDescribeTrial pTrialName_ =
  DescribeTrial' {trialName = pTrialName_}

-- | The name of the trial to describe.
--
-- /Note:/ Consider using 'trialName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTrialName :: Lens.Lens' DescribeTrial Lude.Text
dTrialName = Lens.lens (trialName :: DescribeTrial -> Lude.Text) (\s a -> s {trialName = a} :: DescribeTrial)
{-# DEPRECATED dTrialName "Use generic-lens or generic-optics with 'trialName' instead." #-}

instance Lude.AWSRequest DescribeTrial where
  type Rs DescribeTrial = DescribeTrialResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeTrialResponse'
            Lude.<$> (x Lude..?> "CreationTime")
            Lude.<*> (x Lude..?> "TrialArn")
            Lude.<*> (x Lude..?> "CreatedBy")
            Lude.<*> (x Lude..?> "LastModifiedTime")
            Lude.<*> (x Lude..?> "ExperimentName")
            Lude.<*> (x Lude..?> "Source")
            Lude.<*> (x Lude..?> "DisplayName")
            Lude.<*> (x Lude..?> "TrialName")
            Lude.<*> (x Lude..?> "LastModifiedBy")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTrial where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DescribeTrial" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeTrial where
  toJSON DescribeTrial' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("TrialName" Lude..= trialName)])

instance Lude.ToPath DescribeTrial where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTrial where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeTrialResponse' smart constructor.
data DescribeTrialResponse = DescribeTrialResponse'
  { -- | When the trial was created.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | The Amazon Resource Name (ARN) of the trial.
    trialARN :: Lude.Maybe Lude.Text,
    -- | Who created the trial.
    createdBy :: Lude.Maybe UserContext,
    -- | When the trial was last modified.
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    -- | The name of the experiment the trial is part of.
    experimentName :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the source and, optionally, the job type.
    source :: Lude.Maybe TrialSource,
    -- | The name of the trial as displayed. If @DisplayName@ isn't specified, @TrialName@ is displayed.
    displayName :: Lude.Maybe Lude.Text,
    -- | The name of the trial.
    trialName :: Lude.Maybe Lude.Text,
    -- | Who last modified the trial.
    lastModifiedBy :: Lude.Maybe UserContext,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTrialResponse' with the minimum fields required to make a request.
--
-- * 'creationTime' - When the trial was created.
-- * 'trialARN' - The Amazon Resource Name (ARN) of the trial.
-- * 'createdBy' - Who created the trial.
-- * 'lastModifiedTime' - When the trial was last modified.
-- * 'experimentName' - The name of the experiment the trial is part of.
-- * 'source' - The Amazon Resource Name (ARN) of the source and, optionally, the job type.
-- * 'displayName' - The name of the trial as displayed. If @DisplayName@ isn't specified, @TrialName@ is displayed.
-- * 'trialName' - The name of the trial.
-- * 'lastModifiedBy' - Who last modified the trial.
-- * 'responseStatus' - The response status code.
mkDescribeTrialResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTrialResponse
mkDescribeTrialResponse pResponseStatus_ =
  DescribeTrialResponse'
    { creationTime = Lude.Nothing,
      trialARN = Lude.Nothing,
      createdBy = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      experimentName = Lude.Nothing,
      source = Lude.Nothing,
      displayName = Lude.Nothing,
      trialName = Lude.Nothing,
      lastModifiedBy = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | When the trial was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsCreationTime :: Lens.Lens' DescribeTrialResponse (Lude.Maybe Lude.Timestamp)
dtrsCreationTime = Lens.lens (creationTime :: DescribeTrialResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeTrialResponse)
{-# DEPRECATED dtrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the trial.
--
-- /Note:/ Consider using 'trialARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsTrialARN :: Lens.Lens' DescribeTrialResponse (Lude.Maybe Lude.Text)
dtrsTrialARN = Lens.lens (trialARN :: DescribeTrialResponse -> Lude.Maybe Lude.Text) (\s a -> s {trialARN = a} :: DescribeTrialResponse)
{-# DEPRECATED dtrsTrialARN "Use generic-lens or generic-optics with 'trialARN' instead." #-}

-- | Who created the trial.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsCreatedBy :: Lens.Lens' DescribeTrialResponse (Lude.Maybe UserContext)
dtrsCreatedBy = Lens.lens (createdBy :: DescribeTrialResponse -> Lude.Maybe UserContext) (\s a -> s {createdBy = a} :: DescribeTrialResponse)
{-# DEPRECATED dtrsCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | When the trial was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsLastModifiedTime :: Lens.Lens' DescribeTrialResponse (Lude.Maybe Lude.Timestamp)
dtrsLastModifiedTime = Lens.lens (lastModifiedTime :: DescribeTrialResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: DescribeTrialResponse)
{-# DEPRECATED dtrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The name of the experiment the trial is part of.
--
-- /Note:/ Consider using 'experimentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsExperimentName :: Lens.Lens' DescribeTrialResponse (Lude.Maybe Lude.Text)
dtrsExperimentName = Lens.lens (experimentName :: DescribeTrialResponse -> Lude.Maybe Lude.Text) (\s a -> s {experimentName = a} :: DescribeTrialResponse)
{-# DEPRECATED dtrsExperimentName "Use generic-lens or generic-optics with 'experimentName' instead." #-}

-- | The Amazon Resource Name (ARN) of the source and, optionally, the job type.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsSource :: Lens.Lens' DescribeTrialResponse (Lude.Maybe TrialSource)
dtrsSource = Lens.lens (source :: DescribeTrialResponse -> Lude.Maybe TrialSource) (\s a -> s {source = a} :: DescribeTrialResponse)
{-# DEPRECATED dtrsSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The name of the trial as displayed. If @DisplayName@ isn't specified, @TrialName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsDisplayName :: Lens.Lens' DescribeTrialResponse (Lude.Maybe Lude.Text)
dtrsDisplayName = Lens.lens (displayName :: DescribeTrialResponse -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: DescribeTrialResponse)
{-# DEPRECATED dtrsDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The name of the trial.
--
-- /Note:/ Consider using 'trialName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsTrialName :: Lens.Lens' DescribeTrialResponse (Lude.Maybe Lude.Text)
dtrsTrialName = Lens.lens (trialName :: DescribeTrialResponse -> Lude.Maybe Lude.Text) (\s a -> s {trialName = a} :: DescribeTrialResponse)
{-# DEPRECATED dtrsTrialName "Use generic-lens or generic-optics with 'trialName' instead." #-}

-- | Who last modified the trial.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsLastModifiedBy :: Lens.Lens' DescribeTrialResponse (Lude.Maybe UserContext)
dtrsLastModifiedBy = Lens.lens (lastModifiedBy :: DescribeTrialResponse -> Lude.Maybe UserContext) (\s a -> s {lastModifiedBy = a} :: DescribeTrialResponse)
{-# DEPRECATED dtrsLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsResponseStatus :: Lens.Lens' DescribeTrialResponse Lude.Int
dtrsResponseStatus = Lens.lens (responseStatus :: DescribeTrialResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTrialResponse)
{-# DEPRECATED dtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
