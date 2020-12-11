-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.PipelineDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.PipelineDescription
  ( PipelineDescription (..),

    -- * Smart constructor
    mkPipelineDescription,

    -- * Lenses
    pdDescription,
    pdTags,
    pdPipelineId,
    pdName,
    pdFields,
  )
where

import Network.AWS.DataPipeline.Types.Field
import Network.AWS.DataPipeline.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains pipeline metadata.
--
-- /See:/ 'mkPipelineDescription' smart constructor.
data PipelineDescription = PipelineDescription'
  { description ::
      Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    pipelineId :: Lude.Text,
    name :: Lude.Text,
    fields :: [Field]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PipelineDescription' with the minimum fields required to make a request.
--
-- * 'description' - Description of the pipeline.
-- * 'fields' - A list of read-only fields that contain metadata about the pipeline: @userId, @accountId, and @pipelineState.
-- * 'name' - The name of the pipeline.
-- * 'pipelineId' - The pipeline identifier that was assigned by AWS Data Pipeline. This is a string of the form @df-297EG78HU43EEXAMPLE@ .
-- * 'tags' - A list of tags to associated with a pipeline. Tags let you control access to pipelines. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-control-access.html Controlling User Access to Pipelines> in the /AWS Data Pipeline Developer Guide/ .
mkPipelineDescription ::
  -- | 'pipelineId'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  PipelineDescription
mkPipelineDescription pPipelineId_ pName_ =
  PipelineDescription'
    { description = Lude.Nothing,
      tags = Lude.Nothing,
      pipelineId = pPipelineId_,
      name = pName_,
      fields = Lude.mempty
    }

-- | Description of the pipeline.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdDescription :: Lens.Lens' PipelineDescription (Lude.Maybe Lude.Text)
pdDescription = Lens.lens (description :: PipelineDescription -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: PipelineDescription)
{-# DEPRECATED pdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of tags to associated with a pipeline. Tags let you control access to pipelines. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-control-access.html Controlling User Access to Pipelines> in the /AWS Data Pipeline Developer Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdTags :: Lens.Lens' PipelineDescription (Lude.Maybe [Tag])
pdTags = Lens.lens (tags :: PipelineDescription -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: PipelineDescription)
{-# DEPRECATED pdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The pipeline identifier that was assigned by AWS Data Pipeline. This is a string of the form @df-297EG78HU43EEXAMPLE@ .
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPipelineId :: Lens.Lens' PipelineDescription Lude.Text
pdPipelineId = Lens.lens (pipelineId :: PipelineDescription -> Lude.Text) (\s a -> s {pipelineId = a} :: PipelineDescription)
{-# DEPRECATED pdPipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

-- | The name of the pipeline.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdName :: Lens.Lens' PipelineDescription Lude.Text
pdName = Lens.lens (name :: PipelineDescription -> Lude.Text) (\s a -> s {name = a} :: PipelineDescription)
{-# DEPRECATED pdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of read-only fields that contain metadata about the pipeline: @userId, @accountId, and @pipelineState.
--
-- /Note:/ Consider using 'fields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdFields :: Lens.Lens' PipelineDescription [Field]
pdFields = Lens.lens (fields :: PipelineDescription -> [Field]) (\s a -> s {fields = a} :: PipelineDescription)
{-# DEPRECATED pdFields "Use generic-lens or generic-optics with 'fields' instead." #-}

instance Lude.FromJSON PipelineDescription where
  parseJSON =
    Lude.withObject
      "PipelineDescription"
      ( \x ->
          PipelineDescription'
            Lude.<$> (x Lude..:? "description")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "pipelineId")
            Lude.<*> (x Lude..: "name")
            Lude.<*> (x Lude..:? "fields" Lude..!= Lude.mempty)
      )
