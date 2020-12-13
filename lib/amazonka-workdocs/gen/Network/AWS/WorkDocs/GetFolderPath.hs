{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.GetFolderPath
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the path information (the hierarchy from the root folder) for the specified folder.
--
-- By default, Amazon WorkDocs returns a maximum of 100 levels upwards from the requested folder and only includes the IDs of the parent folders in the path. You can limit the maximum number of levels. You can also request the parent folder names.
module Network.AWS.WorkDocs.GetFolderPath
  ( -- * Creating a request
    GetFolderPath (..),
    mkGetFolderPath,

    -- ** Request lenses
    gfpAuthenticationToken,
    gfpFolderId,
    gfpMarker,
    gfpLimit,
    gfpFields,

    -- * Destructuring the response
    GetFolderPathResponse (..),
    mkGetFolderPathResponse,

    -- ** Response lenses
    gfprsPath,
    gfprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkGetFolderPath' smart constructor.
data GetFolderPath = GetFolderPath'
  { -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The ID of the folder.
    folderId :: Lude.Text,
    -- | This value is not supported.
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of levels in the hierarchy to return.
    limit :: Lude.Maybe Lude.Natural,
    -- | A comma-separated list of values. Specify "NAME" to include the names of the parent folders.
    fields :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFolderPath' with the minimum fields required to make a request.
--
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'folderId' - The ID of the folder.
-- * 'marker' - This value is not supported.
-- * 'limit' - The maximum number of levels in the hierarchy to return.
-- * 'fields' - A comma-separated list of values. Specify "NAME" to include the names of the parent folders.
mkGetFolderPath ::
  -- | 'folderId'
  Lude.Text ->
  GetFolderPath
mkGetFolderPath pFolderId_ =
  GetFolderPath'
    { authenticationToken = Lude.Nothing,
      folderId = pFolderId_,
      marker = Lude.Nothing,
      limit = Lude.Nothing,
      fields = Lude.Nothing
    }

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfpAuthenticationToken :: Lens.Lens' GetFolderPath (Lude.Maybe (Lude.Sensitive Lude.Text))
gfpAuthenticationToken = Lens.lens (authenticationToken :: GetFolderPath -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: GetFolderPath)
{-# DEPRECATED gfpAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The ID of the folder.
--
-- /Note:/ Consider using 'folderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfpFolderId :: Lens.Lens' GetFolderPath Lude.Text
gfpFolderId = Lens.lens (folderId :: GetFolderPath -> Lude.Text) (\s a -> s {folderId = a} :: GetFolderPath)
{-# DEPRECATED gfpFolderId "Use generic-lens or generic-optics with 'folderId' instead." #-}

-- | This value is not supported.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfpMarker :: Lens.Lens' GetFolderPath (Lude.Maybe Lude.Text)
gfpMarker = Lens.lens (marker :: GetFolderPath -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: GetFolderPath)
{-# DEPRECATED gfpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of levels in the hierarchy to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfpLimit :: Lens.Lens' GetFolderPath (Lude.Maybe Lude.Natural)
gfpLimit = Lens.lens (limit :: GetFolderPath -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: GetFolderPath)
{-# DEPRECATED gfpLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | A comma-separated list of values. Specify "NAME" to include the names of the parent folders.
--
-- /Note:/ Consider using 'fields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfpFields :: Lens.Lens' GetFolderPath (Lude.Maybe Lude.Text)
gfpFields = Lens.lens (fields :: GetFolderPath -> Lude.Maybe Lude.Text) (\s a -> s {fields = a} :: GetFolderPath)
{-# DEPRECATED gfpFields "Use generic-lens or generic-optics with 'fields' instead." #-}

instance Lude.AWSRequest GetFolderPath where
  type Rs GetFolderPath = GetFolderPathResponse
  request = Req.get workDocsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetFolderPathResponse'
            Lude.<$> (x Lude..?> "Path") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetFolderPath where
  toHeaders GetFolderPath' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToPath GetFolderPath where
  toPath GetFolderPath' {..} =
    Lude.mconcat ["/api/v1/folders/", Lude.toBS folderId, "/path"]

instance Lude.ToQuery GetFolderPath where
  toQuery GetFolderPath' {..} =
    Lude.mconcat
      [ "marker" Lude.=: marker,
        "limit" Lude.=: limit,
        "fields" Lude.=: fields
      ]

-- | /See:/ 'mkGetFolderPathResponse' smart constructor.
data GetFolderPathResponse = GetFolderPathResponse'
  { -- | The path information.
    path :: Lude.Maybe ResourcePath,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFolderPathResponse' with the minimum fields required to make a request.
--
-- * 'path' - The path information.
-- * 'responseStatus' - The response status code.
mkGetFolderPathResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetFolderPathResponse
mkGetFolderPathResponse pResponseStatus_ =
  GetFolderPathResponse'
    { path = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The path information.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfprsPath :: Lens.Lens' GetFolderPathResponse (Lude.Maybe ResourcePath)
gfprsPath = Lens.lens (path :: GetFolderPathResponse -> Lude.Maybe ResourcePath) (\s a -> s {path = a} :: GetFolderPathResponse)
{-# DEPRECATED gfprsPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfprsResponseStatus :: Lens.Lens' GetFolderPathResponse Lude.Int
gfprsResponseStatus = Lens.lens (responseStatus :: GetFolderPathResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetFolderPathResponse)
{-# DEPRECATED gfprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
