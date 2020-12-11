{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeExperiment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of an experiment's properties.
module Network.AWS.SageMaker.DescribeExperiment
  ( -- * Creating a request
    DescribeExperiment (..),
    mkDescribeExperiment,

    -- ** Request lenses
    deExperimentName,

    -- * Destructuring the response
    DescribeExperimentResponse (..),
    mkDescribeExperimentResponse,

    -- ** Response lenses
    deersCreationTime,
    deersCreatedBy,
    deersLastModifiedTime,
    deersExperimentName,
    deersExperimentARN,
    deersSource,
    deersDisplayName,
    deersLastModifiedBy,
    deersDescription,
    deersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeExperiment' smart constructor.
newtype DescribeExperiment = DescribeExperiment'
  { experimentName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeExperiment' with the minimum fields required to make a request.
--
-- * 'experimentName' - The name of the experiment to describe.
mkDescribeExperiment ::
  -- | 'experimentName'
  Lude.Text ->
  DescribeExperiment
mkDescribeExperiment pExperimentName_ =
  DescribeExperiment' {experimentName = pExperimentName_}

-- | The name of the experiment to describe.
--
-- /Note:/ Consider using 'experimentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deExperimentName :: Lens.Lens' DescribeExperiment Lude.Text
deExperimentName = Lens.lens (experimentName :: DescribeExperiment -> Lude.Text) (\s a -> s {experimentName = a} :: DescribeExperiment)
{-# DEPRECATED deExperimentName "Use generic-lens or generic-optics with 'experimentName' instead." #-}

instance Lude.AWSRequest DescribeExperiment where
  type Rs DescribeExperiment = DescribeExperimentResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeExperimentResponse'
            Lude.<$> (x Lude..?> "CreationTime")
            Lude.<*> (x Lude..?> "CreatedBy")
            Lude.<*> (x Lude..?> "LastModifiedTime")
            Lude.<*> (x Lude..?> "ExperimentName")
            Lude.<*> (x Lude..?> "ExperimentArn")
            Lude.<*> (x Lude..?> "Source")
            Lude.<*> (x Lude..?> "DisplayName")
            Lude.<*> (x Lude..?> "LastModifiedBy")
            Lude.<*> (x Lude..?> "Description")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeExperiment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DescribeExperiment" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeExperiment where
  toJSON DescribeExperiment' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ExperimentName" Lude..= experimentName)]
      )

instance Lude.ToPath DescribeExperiment where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeExperiment where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeExperimentResponse' smart constructor.
data DescribeExperimentResponse = DescribeExperimentResponse'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    createdBy :: Lude.Maybe UserContext,
    lastModifiedTime ::
      Lude.Maybe Lude.Timestamp,
    experimentName ::
      Lude.Maybe Lude.Text,
    experimentARN :: Lude.Maybe Lude.Text,
    source :: Lude.Maybe ExperimentSource,
    displayName :: Lude.Maybe Lude.Text,
    lastModifiedBy ::
      Lude.Maybe UserContext,
    description :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeExperimentResponse' with the minimum fields required to make a request.
--
-- * 'createdBy' - Who created the experiment.
-- * 'creationTime' - When the experiment was created.
-- * 'description' - The description of the experiment.
-- * 'displayName' - The name of the experiment as displayed. If @DisplayName@ isn't specified, @ExperimentName@ is displayed.
-- * 'experimentARN' - The Amazon Resource Name (ARN) of the experiment.
-- * 'experimentName' - The name of the experiment.
-- * 'lastModifiedBy' - Who last modified the experiment.
-- * 'lastModifiedTime' - When the experiment was last modified.
-- * 'responseStatus' - The response status code.
-- * 'source' - The ARN of the source and, optionally, the type.
mkDescribeExperimentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeExperimentResponse
mkDescribeExperimentResponse pResponseStatus_ =
  DescribeExperimentResponse'
    { creationTime = Lude.Nothing,
      createdBy = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      experimentName = Lude.Nothing,
      experimentARN = Lude.Nothing,
      source = Lude.Nothing,
      displayName = Lude.Nothing,
      lastModifiedBy = Lude.Nothing,
      description = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | When the experiment was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deersCreationTime :: Lens.Lens' DescribeExperimentResponse (Lude.Maybe Lude.Timestamp)
deersCreationTime = Lens.lens (creationTime :: DescribeExperimentResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeExperimentResponse)
{-# DEPRECATED deersCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Who created the experiment.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deersCreatedBy :: Lens.Lens' DescribeExperimentResponse (Lude.Maybe UserContext)
deersCreatedBy = Lens.lens (createdBy :: DescribeExperimentResponse -> Lude.Maybe UserContext) (\s a -> s {createdBy = a} :: DescribeExperimentResponse)
{-# DEPRECATED deersCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | When the experiment was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deersLastModifiedTime :: Lens.Lens' DescribeExperimentResponse (Lude.Maybe Lude.Timestamp)
deersLastModifiedTime = Lens.lens (lastModifiedTime :: DescribeExperimentResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: DescribeExperimentResponse)
{-# DEPRECATED deersLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The name of the experiment.
--
-- /Note:/ Consider using 'experimentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deersExperimentName :: Lens.Lens' DescribeExperimentResponse (Lude.Maybe Lude.Text)
deersExperimentName = Lens.lens (experimentName :: DescribeExperimentResponse -> Lude.Maybe Lude.Text) (\s a -> s {experimentName = a} :: DescribeExperimentResponse)
{-# DEPRECATED deersExperimentName "Use generic-lens or generic-optics with 'experimentName' instead." #-}

-- | The Amazon Resource Name (ARN) of the experiment.
--
-- /Note:/ Consider using 'experimentARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deersExperimentARN :: Lens.Lens' DescribeExperimentResponse (Lude.Maybe Lude.Text)
deersExperimentARN = Lens.lens (experimentARN :: DescribeExperimentResponse -> Lude.Maybe Lude.Text) (\s a -> s {experimentARN = a} :: DescribeExperimentResponse)
{-# DEPRECATED deersExperimentARN "Use generic-lens or generic-optics with 'experimentARN' instead." #-}

-- | The ARN of the source and, optionally, the type.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deersSource :: Lens.Lens' DescribeExperimentResponse (Lude.Maybe ExperimentSource)
deersSource = Lens.lens (source :: DescribeExperimentResponse -> Lude.Maybe ExperimentSource) (\s a -> s {source = a} :: DescribeExperimentResponse)
{-# DEPRECATED deersSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The name of the experiment as displayed. If @DisplayName@ isn't specified, @ExperimentName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deersDisplayName :: Lens.Lens' DescribeExperimentResponse (Lude.Maybe Lude.Text)
deersDisplayName = Lens.lens (displayName :: DescribeExperimentResponse -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: DescribeExperimentResponse)
{-# DEPRECATED deersDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | Who last modified the experiment.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deersLastModifiedBy :: Lens.Lens' DescribeExperimentResponse (Lude.Maybe UserContext)
deersLastModifiedBy = Lens.lens (lastModifiedBy :: DescribeExperimentResponse -> Lude.Maybe UserContext) (\s a -> s {lastModifiedBy = a} :: DescribeExperimentResponse)
{-# DEPRECATED deersLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | The description of the experiment.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deersDescription :: Lens.Lens' DescribeExperimentResponse (Lude.Maybe Lude.Text)
deersDescription = Lens.lens (description :: DescribeExperimentResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DescribeExperimentResponse)
{-# DEPRECATED deersDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deersResponseStatus :: Lens.Lens' DescribeExperimentResponse Lude.Int
deersResponseStatus = Lens.lens (responseStatus :: DescribeExperimentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeExperimentResponse)
{-# DEPRECATED deersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
