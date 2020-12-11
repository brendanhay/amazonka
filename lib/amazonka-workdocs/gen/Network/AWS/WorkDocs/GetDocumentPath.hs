{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.GetDocumentPath
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the path information (the hierarchy from the root folder) for the requested document.
--
-- By default, Amazon WorkDocs returns a maximum of 100 levels upwards from the requested document and only includes the IDs of the parent folders in the path. You can limit the maximum number of levels. You can also request the names of the parent folders.
module Network.AWS.WorkDocs.GetDocumentPath
  ( -- * Creating a request
    GetDocumentPath (..),
    mkGetDocumentPath,

    -- ** Request lenses
    gdpAuthenticationToken,
    gdpMarker,
    gdpLimit,
    gdpFields,
    gdpDocumentId,

    -- * Destructuring the response
    GetDocumentPathResponse (..),
    mkGetDocumentPathResponse,

    -- ** Response lenses
    gdprsPath,
    gdprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkGetDocumentPath' smart constructor.
data GetDocumentPath = GetDocumentPath'
  { authenticationToken ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    marker :: Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural,
    fields :: Lude.Maybe Lude.Text,
    documentId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDocumentPath' with the minimum fields required to make a request.
--
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'documentId' - The ID of the document.
-- * 'fields' - A comma-separated list of values. Specify @NAME@ to include the names of the parent folders.
-- * 'limit' - The maximum number of levels in the hierarchy to return.
-- * 'marker' - This value is not supported.
mkGetDocumentPath ::
  -- | 'documentId'
  Lude.Text ->
  GetDocumentPath
mkGetDocumentPath pDocumentId_ =
  GetDocumentPath'
    { authenticationToken = Lude.Nothing,
      marker = Lude.Nothing,
      limit = Lude.Nothing,
      fields = Lude.Nothing,
      documentId = pDocumentId_
    }

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpAuthenticationToken :: Lens.Lens' GetDocumentPath (Lude.Maybe (Lude.Sensitive Lude.Text))
gdpAuthenticationToken = Lens.lens (authenticationToken :: GetDocumentPath -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: GetDocumentPath)
{-# DEPRECATED gdpAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | This value is not supported.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpMarker :: Lens.Lens' GetDocumentPath (Lude.Maybe Lude.Text)
gdpMarker = Lens.lens (marker :: GetDocumentPath -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: GetDocumentPath)
{-# DEPRECATED gdpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of levels in the hierarchy to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpLimit :: Lens.Lens' GetDocumentPath (Lude.Maybe Lude.Natural)
gdpLimit = Lens.lens (limit :: GetDocumentPath -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: GetDocumentPath)
{-# DEPRECATED gdpLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | A comma-separated list of values. Specify @NAME@ to include the names of the parent folders.
--
-- /Note:/ Consider using 'fields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpFields :: Lens.Lens' GetDocumentPath (Lude.Maybe Lude.Text)
gdpFields = Lens.lens (fields :: GetDocumentPath -> Lude.Maybe Lude.Text) (\s a -> s {fields = a} :: GetDocumentPath)
{-# DEPRECATED gdpFields "Use generic-lens or generic-optics with 'fields' instead." #-}

-- | The ID of the document.
--
-- /Note:/ Consider using 'documentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpDocumentId :: Lens.Lens' GetDocumentPath Lude.Text
gdpDocumentId = Lens.lens (documentId :: GetDocumentPath -> Lude.Text) (\s a -> s {documentId = a} :: GetDocumentPath)
{-# DEPRECATED gdpDocumentId "Use generic-lens or generic-optics with 'documentId' instead." #-}

instance Lude.AWSRequest GetDocumentPath where
  type Rs GetDocumentPath = GetDocumentPathResponse
  request = Req.get workDocsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDocumentPathResponse'
            Lude.<$> (x Lude..?> "Path") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDocumentPath where
  toHeaders GetDocumentPath' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToPath GetDocumentPath where
  toPath GetDocumentPath' {..} =
    Lude.mconcat
      ["/api/v1/documents/", Lude.toBS documentId, "/path"]

instance Lude.ToQuery GetDocumentPath where
  toQuery GetDocumentPath' {..} =
    Lude.mconcat
      [ "marker" Lude.=: marker,
        "limit" Lude.=: limit,
        "fields" Lude.=: fields
      ]

-- | /See:/ 'mkGetDocumentPathResponse' smart constructor.
data GetDocumentPathResponse = GetDocumentPathResponse'
  { path ::
      Lude.Maybe ResourcePath,
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

-- | Creates a value of 'GetDocumentPathResponse' with the minimum fields required to make a request.
--
-- * 'path' - The path information.
-- * 'responseStatus' - The response status code.
mkGetDocumentPathResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDocumentPathResponse
mkGetDocumentPathResponse pResponseStatus_ =
  GetDocumentPathResponse'
    { path = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The path information.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdprsPath :: Lens.Lens' GetDocumentPathResponse (Lude.Maybe ResourcePath)
gdprsPath = Lens.lens (path :: GetDocumentPathResponse -> Lude.Maybe ResourcePath) (\s a -> s {path = a} :: GetDocumentPathResponse)
{-# DEPRECATED gdprsPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdprsResponseStatus :: Lens.Lens' GetDocumentPathResponse Lude.Int
gdprsResponseStatus = Lens.lens (responseStatus :: GetDocumentPathResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDocumentPathResponse)
{-# DEPRECATED gdprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
