{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.CreateLabels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified list of labels to the given resource (a document or folder)
module Network.AWS.WorkDocs.CreateLabels
  ( -- * Creating a request
    CreateLabels (..),
    mkCreateLabels,

    -- ** Request lenses
    clResourceId,
    clAuthenticationToken,
    clLabels,

    -- * Destructuring the response
    CreateLabelsResponse (..),
    mkCreateLabelsResponse,

    -- ** Response lenses
    clrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkCreateLabels' smart constructor.
data CreateLabels = CreateLabels'
  { -- | The ID of the resource.
    resourceId :: Lude.Text,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | List of labels to add to the resource.
    labels :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLabels' with the minimum fields required to make a request.
--
-- * 'resourceId' - The ID of the resource.
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'labels' - List of labels to add to the resource.
mkCreateLabels ::
  -- | 'resourceId'
  Lude.Text ->
  CreateLabels
mkCreateLabels pResourceId_ =
  CreateLabels'
    { resourceId = pResourceId_,
      authenticationToken = Lude.Nothing,
      labels = Lude.mempty
    }

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clResourceId :: Lens.Lens' CreateLabels Lude.Text
clResourceId = Lens.lens (resourceId :: CreateLabels -> Lude.Text) (\s a -> s {resourceId = a} :: CreateLabels)
{-# DEPRECATED clResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clAuthenticationToken :: Lens.Lens' CreateLabels (Lude.Maybe (Lude.Sensitive Lude.Text))
clAuthenticationToken = Lens.lens (authenticationToken :: CreateLabels -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: CreateLabels)
{-# DEPRECATED clAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | List of labels to add to the resource.
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clLabels :: Lens.Lens' CreateLabels [Lude.Text]
clLabels = Lens.lens (labels :: CreateLabels -> [Lude.Text]) (\s a -> s {labels = a} :: CreateLabels)
{-# DEPRECATED clLabels "Use generic-lens or generic-optics with 'labels' instead." #-}

instance Lude.AWSRequest CreateLabels where
  type Rs CreateLabels = CreateLabelsResponse
  request = Req.putJSON workDocsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateLabelsResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateLabels where
  toHeaders CreateLabels' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON CreateLabels where
  toJSON CreateLabels' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("Labels" Lude..= labels)])

instance Lude.ToPath CreateLabels where
  toPath CreateLabels' {..} =
    Lude.mconcat
      ["/api/v1/resources/", Lude.toBS resourceId, "/labels"]

instance Lude.ToQuery CreateLabels where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateLabelsResponse' smart constructor.
newtype CreateLabelsResponse = CreateLabelsResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLabelsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateLabelsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateLabelsResponse
mkCreateLabelsResponse pResponseStatus_ =
  CreateLabelsResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clrsResponseStatus :: Lens.Lens' CreateLabelsResponse Lude.Int
clrsResponseStatus = Lens.lens (responseStatus :: CreateLabelsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateLabelsResponse)
{-# DEPRECATED clrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
