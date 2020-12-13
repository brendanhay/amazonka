{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DeleteLabels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified list of labels from a resource.
module Network.AWS.WorkDocs.DeleteLabels
  ( -- * Creating a request
    DeleteLabels (..),
    mkDeleteLabels,

    -- ** Request lenses
    dlResourceId,
    dlDeleteAll,
    dlAuthenticationToken,
    dlLabels,

    -- * Destructuring the response
    DeleteLabelsResponse (..),
    mkDeleteLabelsResponse,

    -- ** Response lenses
    dlrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkDeleteLabels' smart constructor.
data DeleteLabels = DeleteLabels'
  { -- | The ID of the resource.
    resourceId :: Lude.Text,
    -- | Flag to request removal of all labels from the specified resource.
    deleteAll :: Lude.Maybe Lude.Bool,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | List of labels to delete from the resource.
    labels :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLabels' with the minimum fields required to make a request.
--
-- * 'resourceId' - The ID of the resource.
-- * 'deleteAll' - Flag to request removal of all labels from the specified resource.
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'labels' - List of labels to delete from the resource.
mkDeleteLabels ::
  -- | 'resourceId'
  Lude.Text ->
  DeleteLabels
mkDeleteLabels pResourceId_ =
  DeleteLabels'
    { resourceId = pResourceId_,
      deleteAll = Lude.Nothing,
      authenticationToken = Lude.Nothing,
      labels = Lude.Nothing
    }

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlResourceId :: Lens.Lens' DeleteLabels Lude.Text
dlResourceId = Lens.lens (resourceId :: DeleteLabels -> Lude.Text) (\s a -> s {resourceId = a} :: DeleteLabels)
{-# DEPRECATED dlResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | Flag to request removal of all labels from the specified resource.
--
-- /Note:/ Consider using 'deleteAll' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlDeleteAll :: Lens.Lens' DeleteLabels (Lude.Maybe Lude.Bool)
dlDeleteAll = Lens.lens (deleteAll :: DeleteLabels -> Lude.Maybe Lude.Bool) (\s a -> s {deleteAll = a} :: DeleteLabels)
{-# DEPRECATED dlDeleteAll "Use generic-lens or generic-optics with 'deleteAll' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlAuthenticationToken :: Lens.Lens' DeleteLabels (Lude.Maybe (Lude.Sensitive Lude.Text))
dlAuthenticationToken = Lens.lens (authenticationToken :: DeleteLabels -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: DeleteLabels)
{-# DEPRECATED dlAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | List of labels to delete from the resource.
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlLabels :: Lens.Lens' DeleteLabels (Lude.Maybe [Lude.Text])
dlLabels = Lens.lens (labels :: DeleteLabels -> Lude.Maybe [Lude.Text]) (\s a -> s {labels = a} :: DeleteLabels)
{-# DEPRECATED dlLabels "Use generic-lens or generic-optics with 'labels' instead." #-}

instance Lude.AWSRequest DeleteLabels where
  type Rs DeleteLabels = DeleteLabelsResponse
  request = Req.delete workDocsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteLabelsResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteLabels where
  toHeaders DeleteLabels' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToPath DeleteLabels where
  toPath DeleteLabels' {..} =
    Lude.mconcat
      ["/api/v1/resources/", Lude.toBS resourceId, "/labels"]

instance Lude.ToQuery DeleteLabels where
  toQuery DeleteLabels' {..} =
    Lude.mconcat
      [ "deleteAll" Lude.=: deleteAll,
        "labels"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> labels)
      ]

-- | /See:/ 'mkDeleteLabelsResponse' smart constructor.
newtype DeleteLabelsResponse = DeleteLabelsResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLabelsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteLabelsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteLabelsResponse
mkDeleteLabelsResponse pResponseStatus_ =
  DeleteLabelsResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrsResponseStatus :: Lens.Lens' DeleteLabelsResponse Lude.Int
dlrsResponseStatus = Lens.lens (responseStatus :: DeleteLabelsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteLabelsResponse)
{-# DEPRECATED dlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
