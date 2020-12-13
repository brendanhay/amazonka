{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.DeletePipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified pipeline.
module Network.AWS.CodePipeline.DeletePipeline
  ( -- * Creating a request
    DeletePipeline (..),
    mkDeletePipeline,

    -- ** Request lenses
    dpName,

    -- * Destructuring the response
    DeletePipelineResponse (..),
    mkDeletePipelineResponse,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @DeletePipeline@ action.
--
-- /See:/ 'mkDeletePipeline' smart constructor.
newtype DeletePipeline = DeletePipeline'
  { -- | The name of the pipeline to be deleted.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePipeline' with the minimum fields required to make a request.
--
-- * 'name' - The name of the pipeline to be deleted.
mkDeletePipeline ::
  -- | 'name'
  Lude.Text ->
  DeletePipeline
mkDeletePipeline pName_ = DeletePipeline' {name = pName_}

-- | The name of the pipeline to be deleted.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpName :: Lens.Lens' DeletePipeline Lude.Text
dpName = Lens.lens (name :: DeletePipeline -> Lude.Text) (\s a -> s {name = a} :: DeletePipeline)
{-# DEPRECATED dpName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeletePipeline where
  type Rs DeletePipeline = DeletePipelineResponse
  request = Req.postJSON codePipelineService
  response = Res.receiveNull DeletePipelineResponse'

instance Lude.ToHeaders DeletePipeline where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodePipeline_20150709.DeletePipeline" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeletePipeline where
  toJSON DeletePipeline' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("name" Lude..= name)])

instance Lude.ToPath DeletePipeline where
  toPath = Lude.const "/"

instance Lude.ToQuery DeletePipeline where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeletePipelineResponse' smart constructor.
data DeletePipelineResponse = DeletePipelineResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePipelineResponse' with the minimum fields required to make a request.
mkDeletePipelineResponse ::
  DeletePipelineResponse
mkDeletePipelineResponse = DeletePipelineResponse'
