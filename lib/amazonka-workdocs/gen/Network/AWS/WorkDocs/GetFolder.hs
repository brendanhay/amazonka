{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.GetFolder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the metadata of the specified folder.
module Network.AWS.WorkDocs.GetFolder
  ( -- * Creating a request
    GetFolder (..),
    mkGetFolder,

    -- ** Request lenses
    gfAuthenticationToken,
    gfIncludeCustomMetadata,
    gfFolderId,

    -- * Destructuring the response
    GetFolderResponse (..),
    mkGetFolderResponse,

    -- ** Response lenses
    gfrsCustomMetadata,
    gfrsMetadata,
    gfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkGetFolder' smart constructor.
data GetFolder = GetFolder'
  { authenticationToken ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    includeCustomMetadata :: Lude.Maybe Lude.Bool,
    folderId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFolder' with the minimum fields required to make a request.
--
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'folderId' - The ID of the folder.
-- * 'includeCustomMetadata' - Set to TRUE to include custom metadata in the response.
mkGetFolder ::
  -- | 'folderId'
  Lude.Text ->
  GetFolder
mkGetFolder pFolderId_ =
  GetFolder'
    { authenticationToken = Lude.Nothing,
      includeCustomMetadata = Lude.Nothing,
      folderId = pFolderId_
    }

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfAuthenticationToken :: Lens.Lens' GetFolder (Lude.Maybe (Lude.Sensitive Lude.Text))
gfAuthenticationToken = Lens.lens (authenticationToken :: GetFolder -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: GetFolder)
{-# DEPRECATED gfAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | Set to TRUE to include custom metadata in the response.
--
-- /Note:/ Consider using 'includeCustomMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfIncludeCustomMetadata :: Lens.Lens' GetFolder (Lude.Maybe Lude.Bool)
gfIncludeCustomMetadata = Lens.lens (includeCustomMetadata :: GetFolder -> Lude.Maybe Lude.Bool) (\s a -> s {includeCustomMetadata = a} :: GetFolder)
{-# DEPRECATED gfIncludeCustomMetadata "Use generic-lens or generic-optics with 'includeCustomMetadata' instead." #-}

-- | The ID of the folder.
--
-- /Note:/ Consider using 'folderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfFolderId :: Lens.Lens' GetFolder Lude.Text
gfFolderId = Lens.lens (folderId :: GetFolder -> Lude.Text) (\s a -> s {folderId = a} :: GetFolder)
{-# DEPRECATED gfFolderId "Use generic-lens or generic-optics with 'folderId' instead." #-}

instance Lude.AWSRequest GetFolder where
  type Rs GetFolder = GetFolderResponse
  request = Req.get workDocsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetFolderResponse'
            Lude.<$> (x Lude..?> "CustomMetadata" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Metadata")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetFolder where
  toHeaders GetFolder' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToPath GetFolder where
  toPath GetFolder' {..} =
    Lude.mconcat ["/api/v1/folders/", Lude.toBS folderId]

instance Lude.ToQuery GetFolder where
  toQuery GetFolder' {..} =
    Lude.mconcat
      ["includeCustomMetadata" Lude.=: includeCustomMetadata]

-- | /See:/ 'mkGetFolderResponse' smart constructor.
data GetFolderResponse = GetFolderResponse'
  { customMetadata ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    metadata :: Lude.Maybe FolderMetadata,
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

-- | Creates a value of 'GetFolderResponse' with the minimum fields required to make a request.
--
-- * 'customMetadata' - The custom metadata on the folder.
-- * 'metadata' - The metadata of the folder.
-- * 'responseStatus' - The response status code.
mkGetFolderResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetFolderResponse
mkGetFolderResponse pResponseStatus_ =
  GetFolderResponse'
    { customMetadata = Lude.Nothing,
      metadata = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The custom metadata on the folder.
--
-- /Note:/ Consider using 'customMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrsCustomMetadata :: Lens.Lens' GetFolderResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gfrsCustomMetadata = Lens.lens (customMetadata :: GetFolderResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {customMetadata = a} :: GetFolderResponse)
{-# DEPRECATED gfrsCustomMetadata "Use generic-lens or generic-optics with 'customMetadata' instead." #-}

-- | The metadata of the folder.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrsMetadata :: Lens.Lens' GetFolderResponse (Lude.Maybe FolderMetadata)
gfrsMetadata = Lens.lens (metadata :: GetFolderResponse -> Lude.Maybe FolderMetadata) (\s a -> s {metadata = a} :: GetFolderResponse)
{-# DEPRECATED gfrsMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrsResponseStatus :: Lens.Lens' GetFolderResponse Lude.Int
gfrsResponseStatus = Lens.lens (responseStatus :: GetFolderResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetFolderResponse)
{-# DEPRECATED gfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
