{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.CreateCustomMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more custom properties to the specified resource (a folder, document, or version).
module Network.AWS.WorkDocs.CreateCustomMetadata
  ( -- * Creating a request
    CreateCustomMetadata (..),
    mkCreateCustomMetadata,

    -- ** Request lenses
    ccmVersionId,
    ccmAuthenticationToken,
    ccmResourceId,
    ccmCustomMetadata,

    -- * Destructuring the response
    CreateCustomMetadataResponse (..),
    mkCreateCustomMetadataResponse,

    -- ** Response lenses
    ccmrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkCreateCustomMetadata' smart constructor.
data CreateCustomMetadata = CreateCustomMetadata'
  { versionId ::
      Lude.Maybe Lude.Text,
    authenticationToken ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    resourceId :: Lude.Text,
    customMetadata ::
      Lude.HashMap Lude.Text (Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCustomMetadata' with the minimum fields required to make a request.
--
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'customMetadata' - Custom metadata in the form of name-value pairs.
-- * 'resourceId' - The ID of the resource.
-- * 'versionId' - The ID of the version, if the custom metadata is being added to a document version.
mkCreateCustomMetadata ::
  -- | 'resourceId'
  Lude.Text ->
  CreateCustomMetadata
mkCreateCustomMetadata pResourceId_ =
  CreateCustomMetadata'
    { versionId = Lude.Nothing,
      authenticationToken = Lude.Nothing,
      resourceId = pResourceId_,
      customMetadata = Lude.mempty
    }

-- | The ID of the version, if the custom metadata is being added to a document version.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccmVersionId :: Lens.Lens' CreateCustomMetadata (Lude.Maybe Lude.Text)
ccmVersionId = Lens.lens (versionId :: CreateCustomMetadata -> Lude.Maybe Lude.Text) (\s a -> s {versionId = a} :: CreateCustomMetadata)
{-# DEPRECATED ccmVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccmAuthenticationToken :: Lens.Lens' CreateCustomMetadata (Lude.Maybe (Lude.Sensitive Lude.Text))
ccmAuthenticationToken = Lens.lens (authenticationToken :: CreateCustomMetadata -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: CreateCustomMetadata)
{-# DEPRECATED ccmAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccmResourceId :: Lens.Lens' CreateCustomMetadata Lude.Text
ccmResourceId = Lens.lens (resourceId :: CreateCustomMetadata -> Lude.Text) (\s a -> s {resourceId = a} :: CreateCustomMetadata)
{-# DEPRECATED ccmResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | Custom metadata in the form of name-value pairs.
--
-- /Note:/ Consider using 'customMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccmCustomMetadata :: Lens.Lens' CreateCustomMetadata (Lude.HashMap Lude.Text (Lude.Text))
ccmCustomMetadata = Lens.lens (customMetadata :: CreateCustomMetadata -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {customMetadata = a} :: CreateCustomMetadata)
{-# DEPRECATED ccmCustomMetadata "Use generic-lens or generic-optics with 'customMetadata' instead." #-}

instance Lude.AWSRequest CreateCustomMetadata where
  type Rs CreateCustomMetadata = CreateCustomMetadataResponse
  request = Req.putJSON workDocsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateCustomMetadataResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCustomMetadata where
  toHeaders CreateCustomMetadata' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON CreateCustomMetadata where
  toJSON CreateCustomMetadata' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("CustomMetadata" Lude..= customMetadata)]
      )

instance Lude.ToPath CreateCustomMetadata where
  toPath CreateCustomMetadata' {..} =
    Lude.mconcat
      ["/api/v1/resources/", Lude.toBS resourceId, "/customMetadata"]

instance Lude.ToQuery CreateCustomMetadata where
  toQuery CreateCustomMetadata' {..} =
    Lude.mconcat ["versionid" Lude.=: versionId]

-- | /See:/ 'mkCreateCustomMetadataResponse' smart constructor.
newtype CreateCustomMetadataResponse = CreateCustomMetadataResponse'
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

-- | Creates a value of 'CreateCustomMetadataResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateCustomMetadataResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateCustomMetadataResponse
mkCreateCustomMetadataResponse pResponseStatus_ =
  CreateCustomMetadataResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccmrsResponseStatus :: Lens.Lens' CreateCustomMetadataResponse Lude.Int
ccmrsResponseStatus = Lens.lens (responseStatus :: CreateCustomMetadataResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCustomMetadataResponse)
{-# DEPRECATED ccmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
