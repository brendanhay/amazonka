{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.DeleteBuildBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a batch build.
module Network.AWS.CodeBuild.DeleteBuildBatch
  ( -- * Creating a request
    DeleteBuildBatch (..),
    mkDeleteBuildBatch,

    -- ** Request lenses
    dbbId,

    -- * Destructuring the response
    DeleteBuildBatchResponse (..),
    mkDeleteBuildBatchResponse,

    -- ** Response lenses
    dbbrsBuildsNotDeleted,
    dbbrsBuildsDeleted,
    dbbrsStatusCode,
    dbbrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteBuildBatch' smart constructor.
newtype DeleteBuildBatch = DeleteBuildBatch'
  { -- | The identifier of the batch build to delete.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBuildBatch' with the minimum fields required to make a request.
--
-- * 'id' - The identifier of the batch build to delete.
mkDeleteBuildBatch ::
  -- | 'id'
  Lude.Text ->
  DeleteBuildBatch
mkDeleteBuildBatch pId_ = DeleteBuildBatch' {id = pId_}

-- | The identifier of the batch build to delete.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbbId :: Lens.Lens' DeleteBuildBatch Lude.Text
dbbId = Lens.lens (id :: DeleteBuildBatch -> Lude.Text) (\s a -> s {id = a} :: DeleteBuildBatch)
{-# DEPRECATED dbbId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DeleteBuildBatch where
  type Rs DeleteBuildBatch = DeleteBuildBatchResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteBuildBatchResponse'
            Lude.<$> (x Lude..?> "buildsNotDeleted" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "buildsDeleted")
            Lude.<*> (x Lude..?> "statusCode")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteBuildBatch where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.DeleteBuildBatch" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteBuildBatch where
  toJSON DeleteBuildBatch' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("id" Lude..= id)])

instance Lude.ToPath DeleteBuildBatch where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteBuildBatch where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteBuildBatchResponse' smart constructor.
data DeleteBuildBatchResponse = DeleteBuildBatchResponse'
  { -- | An array of @BuildNotDeleted@ objects that specify the builds that could not be deleted.
    buildsNotDeleted :: Lude.Maybe [BuildNotDeleted],
    -- | An array of strings that contain the identifiers of the builds that were deleted.
    buildsDeleted :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | The status code.
    statusCode :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBuildBatchResponse' with the minimum fields required to make a request.
--
-- * 'buildsNotDeleted' - An array of @BuildNotDeleted@ objects that specify the builds that could not be deleted.
-- * 'buildsDeleted' - An array of strings that contain the identifiers of the builds that were deleted.
-- * 'statusCode' - The status code.
-- * 'responseStatus' - The response status code.
mkDeleteBuildBatchResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteBuildBatchResponse
mkDeleteBuildBatchResponse pResponseStatus_ =
  DeleteBuildBatchResponse'
    { buildsNotDeleted = Lude.Nothing,
      buildsDeleted = Lude.Nothing,
      statusCode = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of @BuildNotDeleted@ objects that specify the builds that could not be deleted.
--
-- /Note:/ Consider using 'buildsNotDeleted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbbrsBuildsNotDeleted :: Lens.Lens' DeleteBuildBatchResponse (Lude.Maybe [BuildNotDeleted])
dbbrsBuildsNotDeleted = Lens.lens (buildsNotDeleted :: DeleteBuildBatchResponse -> Lude.Maybe [BuildNotDeleted]) (\s a -> s {buildsNotDeleted = a} :: DeleteBuildBatchResponse)
{-# DEPRECATED dbbrsBuildsNotDeleted "Use generic-lens or generic-optics with 'buildsNotDeleted' instead." #-}

-- | An array of strings that contain the identifiers of the builds that were deleted.
--
-- /Note:/ Consider using 'buildsDeleted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbbrsBuildsDeleted :: Lens.Lens' DeleteBuildBatchResponse (Lude.Maybe (Lude.NonEmpty Lude.Text))
dbbrsBuildsDeleted = Lens.lens (buildsDeleted :: DeleteBuildBatchResponse -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {buildsDeleted = a} :: DeleteBuildBatchResponse)
{-# DEPRECATED dbbrsBuildsDeleted "Use generic-lens or generic-optics with 'buildsDeleted' instead." #-}

-- | The status code.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbbrsStatusCode :: Lens.Lens' DeleteBuildBatchResponse (Lude.Maybe Lude.Text)
dbbrsStatusCode = Lens.lens (statusCode :: DeleteBuildBatchResponse -> Lude.Maybe Lude.Text) (\s a -> s {statusCode = a} :: DeleteBuildBatchResponse)
{-# DEPRECATED dbbrsStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbbrsResponseStatus :: Lens.Lens' DeleteBuildBatchResponse Lude.Int
dbbrsResponseStatus = Lens.lens (responseStatus :: DeleteBuildBatchResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteBuildBatchResponse)
{-# DEPRECATED dbbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
