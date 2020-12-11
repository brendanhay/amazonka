{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.CreatePipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new, empty pipeline. Use 'PutPipelineDefinition' to populate the pipeline.
module Network.AWS.DataPipeline.CreatePipeline
  ( -- * Creating a request
    CreatePipeline (..),
    mkCreatePipeline,

    -- ** Request lenses
    cpDescription,
    cpTags,
    cpName,
    cpUniqueId,

    -- * Destructuring the response
    CreatePipelineResponse (..),
    mkCreatePipelineResponse,

    -- ** Response lenses
    cprsResponseStatus,
    cprsPipelineId,
  )
where

import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for CreatePipeline.
--
-- /See:/ 'mkCreatePipeline' smart constructor.
data CreatePipeline = CreatePipeline'
  { description ::
      Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    name :: Lude.Text,
    uniqueId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePipeline' with the minimum fields required to make a request.
--
-- * 'description' - The description for the pipeline.
-- * 'name' - The name for the pipeline. You can use the same name for multiple pipelines associated with your AWS account, because AWS Data Pipeline assigns each pipeline a unique pipeline identifier.
-- * 'tags' - A list of tags to associate with the pipeline at creation. Tags let you control access to pipelines. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-control-access.html Controlling User Access to Pipelines> in the /AWS Data Pipeline Developer Guide/ .
-- * 'uniqueId' - A unique identifier. This identifier is not the same as the pipeline identifier assigned by AWS Data Pipeline. You are responsible for defining the format and ensuring the uniqueness of this identifier. You use this parameter to ensure idempotency during repeated calls to @CreatePipeline@ . For example, if the first call to @CreatePipeline@ does not succeed, you can pass in the same unique identifier and pipeline name combination on a subsequent call to @CreatePipeline@ . @CreatePipeline@ ensures that if a pipeline already exists with the same name and unique identifier, a new pipeline is not created. Instead, you'll receive the pipeline identifier from the previous attempt. The uniqueness of the name and unique identifier combination is scoped to the AWS account or IAM user credentials.
mkCreatePipeline ::
  -- | 'name'
  Lude.Text ->
  -- | 'uniqueId'
  Lude.Text ->
  CreatePipeline
mkCreatePipeline pName_ pUniqueId_ =
  CreatePipeline'
    { description = Lude.Nothing,
      tags = Lude.Nothing,
      name = pName_,
      uniqueId = pUniqueId_
    }

-- | The description for the pipeline.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpDescription :: Lens.Lens' CreatePipeline (Lude.Maybe Lude.Text)
cpDescription = Lens.lens (description :: CreatePipeline -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreatePipeline)
{-# DEPRECATED cpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of tags to associate with the pipeline at creation. Tags let you control access to pipelines. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-control-access.html Controlling User Access to Pipelines> in the /AWS Data Pipeline Developer Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTags :: Lens.Lens' CreatePipeline (Lude.Maybe [Tag])
cpTags = Lens.lens (tags :: CreatePipeline -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreatePipeline)
{-# DEPRECATED cpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name for the pipeline. You can use the same name for multiple pipelines associated with your AWS account, because AWS Data Pipeline assigns each pipeline a unique pipeline identifier.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpName :: Lens.Lens' CreatePipeline Lude.Text
cpName = Lens.lens (name :: CreatePipeline -> Lude.Text) (\s a -> s {name = a} :: CreatePipeline)
{-# DEPRECATED cpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A unique identifier. This identifier is not the same as the pipeline identifier assigned by AWS Data Pipeline. You are responsible for defining the format and ensuring the uniqueness of this identifier. You use this parameter to ensure idempotency during repeated calls to @CreatePipeline@ . For example, if the first call to @CreatePipeline@ does not succeed, you can pass in the same unique identifier and pipeline name combination on a subsequent call to @CreatePipeline@ . @CreatePipeline@ ensures that if a pipeline already exists with the same name and unique identifier, a new pipeline is not created. Instead, you'll receive the pipeline identifier from the previous attempt. The uniqueness of the name and unique identifier combination is scoped to the AWS account or IAM user credentials.
--
-- /Note:/ Consider using 'uniqueId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpUniqueId :: Lens.Lens' CreatePipeline Lude.Text
cpUniqueId = Lens.lens (uniqueId :: CreatePipeline -> Lude.Text) (\s a -> s {uniqueId = a} :: CreatePipeline)
{-# DEPRECATED cpUniqueId "Use generic-lens or generic-optics with 'uniqueId' instead." #-}

instance Lude.AWSRequest CreatePipeline where
  type Rs CreatePipeline = CreatePipelineResponse
  request = Req.postJSON dataPipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreatePipelineResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "pipelineId")
      )

instance Lude.ToHeaders CreatePipeline where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DataPipeline.CreatePipeline" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreatePipeline where
  toJSON CreatePipeline' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("description" Lude..=) Lude.<$> description,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("name" Lude..= name),
            Lude.Just ("uniqueId" Lude..= uniqueId)
          ]
      )

instance Lude.ToPath CreatePipeline where
  toPath = Lude.const "/"

instance Lude.ToQuery CreatePipeline where
  toQuery = Lude.const Lude.mempty

-- | Contains the output of CreatePipeline.
--
-- /See:/ 'mkCreatePipelineResponse' smart constructor.
data CreatePipelineResponse = CreatePipelineResponse'
  { responseStatus ::
      Lude.Int,
    pipelineId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePipelineResponse' with the minimum fields required to make a request.
--
-- * 'pipelineId' - The ID that AWS Data Pipeline assigns the newly created pipeline. For example, @df-06372391ZG65EXAMPLE@ .
-- * 'responseStatus' - The response status code.
mkCreatePipelineResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'pipelineId'
  Lude.Text ->
  CreatePipelineResponse
mkCreatePipelineResponse pResponseStatus_ pPipelineId_ =
  CreatePipelineResponse'
    { responseStatus = pResponseStatus_,
      pipelineId = pPipelineId_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsResponseStatus :: Lens.Lens' CreatePipelineResponse Lude.Int
cprsResponseStatus = Lens.lens (responseStatus :: CreatePipelineResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePipelineResponse)
{-# DEPRECATED cprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The ID that AWS Data Pipeline assigns the newly created pipeline. For example, @df-06372391ZG65EXAMPLE@ .
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsPipelineId :: Lens.Lens' CreatePipelineResponse Lude.Text
cprsPipelineId = Lens.lens (pipelineId :: CreatePipelineResponse -> Lude.Text) (\s a -> s {pipelineId = a} :: CreatePipelineResponse)
{-# DEPRECATED cprsPipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}
