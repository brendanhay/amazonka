{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DeleteCustomMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes custom metadata from the specified resource.
module Network.AWS.WorkDocs.DeleteCustomMetadata
  ( -- * Creating a request
    DeleteCustomMetadata (..),
    mkDeleteCustomMetadata,

    -- ** Request lenses
    dcmVersionId,
    dcmDeleteAll,
    dcmAuthenticationToken,
    dcmKeys,
    dcmResourceId,

    -- * Destructuring the response
    DeleteCustomMetadataResponse (..),
    mkDeleteCustomMetadataResponse,

    -- ** Response lenses
    dcmrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkDeleteCustomMetadata' smart constructor.
data DeleteCustomMetadata = DeleteCustomMetadata'
  { versionId ::
      Lude.Maybe Lude.Text,
    deleteAll :: Lude.Maybe Lude.Bool,
    authenticationToken ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    keys :: Lude.Maybe [Lude.Text],
    resourceId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCustomMetadata' with the minimum fields required to make a request.
--
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'deleteAll' - Flag to indicate removal of all custom metadata properties from the specified resource.
-- * 'keys' - List of properties to remove.
-- * 'resourceId' - The ID of the resource, either a document or folder.
-- * 'versionId' - The ID of the version, if the custom metadata is being deleted from a document version.
mkDeleteCustomMetadata ::
  -- | 'resourceId'
  Lude.Text ->
  DeleteCustomMetadata
mkDeleteCustomMetadata pResourceId_ =
  DeleteCustomMetadata'
    { versionId = Lude.Nothing,
      deleteAll = Lude.Nothing,
      authenticationToken = Lude.Nothing,
      keys = Lude.Nothing,
      resourceId = pResourceId_
    }

-- | The ID of the version, if the custom metadata is being deleted from a document version.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcmVersionId :: Lens.Lens' DeleteCustomMetadata (Lude.Maybe Lude.Text)
dcmVersionId = Lens.lens (versionId :: DeleteCustomMetadata -> Lude.Maybe Lude.Text) (\s a -> s {versionId = a} :: DeleteCustomMetadata)
{-# DEPRECATED dcmVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | Flag to indicate removal of all custom metadata properties from the specified resource.
--
-- /Note:/ Consider using 'deleteAll' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcmDeleteAll :: Lens.Lens' DeleteCustomMetadata (Lude.Maybe Lude.Bool)
dcmDeleteAll = Lens.lens (deleteAll :: DeleteCustomMetadata -> Lude.Maybe Lude.Bool) (\s a -> s {deleteAll = a} :: DeleteCustomMetadata)
{-# DEPRECATED dcmDeleteAll "Use generic-lens or generic-optics with 'deleteAll' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcmAuthenticationToken :: Lens.Lens' DeleteCustomMetadata (Lude.Maybe (Lude.Sensitive Lude.Text))
dcmAuthenticationToken = Lens.lens (authenticationToken :: DeleteCustomMetadata -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: DeleteCustomMetadata)
{-# DEPRECATED dcmAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | List of properties to remove.
--
-- /Note:/ Consider using 'keys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcmKeys :: Lens.Lens' DeleteCustomMetadata (Lude.Maybe [Lude.Text])
dcmKeys = Lens.lens (keys :: DeleteCustomMetadata -> Lude.Maybe [Lude.Text]) (\s a -> s {keys = a} :: DeleteCustomMetadata)
{-# DEPRECATED dcmKeys "Use generic-lens or generic-optics with 'keys' instead." #-}

-- | The ID of the resource, either a document or folder.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcmResourceId :: Lens.Lens' DeleteCustomMetadata Lude.Text
dcmResourceId = Lens.lens (resourceId :: DeleteCustomMetadata -> Lude.Text) (\s a -> s {resourceId = a} :: DeleteCustomMetadata)
{-# DEPRECATED dcmResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

instance Lude.AWSRequest DeleteCustomMetadata where
  type Rs DeleteCustomMetadata = DeleteCustomMetadataResponse
  request = Req.delete workDocsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteCustomMetadataResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteCustomMetadata where
  toHeaders DeleteCustomMetadata' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToPath DeleteCustomMetadata where
  toPath DeleteCustomMetadata' {..} =
    Lude.mconcat
      ["/api/v1/resources/", Lude.toBS resourceId, "/customMetadata"]

instance Lude.ToQuery DeleteCustomMetadata where
  toQuery DeleteCustomMetadata' {..} =
    Lude.mconcat
      [ "versionId" Lude.=: versionId,
        "deleteAll" Lude.=: deleteAll,
        "keys"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> keys)
      ]

-- | /See:/ 'mkDeleteCustomMetadataResponse' smart constructor.
newtype DeleteCustomMetadataResponse = DeleteCustomMetadataResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCustomMetadataResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteCustomMetadataResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteCustomMetadataResponse
mkDeleteCustomMetadataResponse pResponseStatus_ =
  DeleteCustomMetadataResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcmrsResponseStatus :: Lens.Lens' DeleteCustomMetadataResponse Lude.Int
dcmrsResponseStatus = Lens.lens (responseStatus :: DeleteCustomMetadataResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteCustomMetadataResponse)
{-# DEPRECATED dcmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
