{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListArtifacts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about artifacts.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListArtifacts
  ( -- * Creating a request
    ListArtifacts (..),
    mkListArtifacts,

    -- ** Request lenses
    laArn,
    laNextToken,
    laType,

    -- * Destructuring the response
    ListArtifactsResponse (..),
    mkListArtifactsResponse,

    -- ** Response lenses
    larsArtifacts,
    larsNextToken,
    larsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a request to the list artifacts operation.
--
-- /See:/ 'mkListArtifacts' smart constructor.
data ListArtifacts = ListArtifacts'
  { -- | The run, job, suite, or test ARN.
    arn :: Lude.Text,
    -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The artifacts' type.
    --
    -- Allowed values include:
    --
    --     * FILE
    --
    --
    --     * LOG
    --
    --
    --     * SCREENSHOT
    type' :: ArtifactCategory
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListArtifacts' with the minimum fields required to make a request.
--
-- * 'arn' - The run, job, suite, or test ARN.
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
-- * 'type'' - The artifacts' type.
--
-- Allowed values include:
--
--     * FILE
--
--
--     * LOG
--
--
--     * SCREENSHOT
mkListArtifacts ::
  -- | 'arn'
  Lude.Text ->
  -- | 'type''
  ArtifactCategory ->
  ListArtifacts
mkListArtifacts pArn_ pType_ =
  ListArtifacts'
    { arn = pArn_,
      nextToken = Lude.Nothing,
      type' = pType_
    }

-- | The run, job, suite, or test ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laArn :: Lens.Lens' ListArtifacts Lude.Text
laArn = Lens.lens (arn :: ListArtifacts -> Lude.Text) (\s a -> s {arn = a} :: ListArtifacts)
{-# DEPRECATED laArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laNextToken :: Lens.Lens' ListArtifacts (Lude.Maybe Lude.Text)
laNextToken = Lens.lens (nextToken :: ListArtifacts -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListArtifacts)
{-# DEPRECATED laNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The artifacts' type.
--
-- Allowed values include:
--
--     * FILE
--
--
--     * LOG
--
--
--     * SCREENSHOT
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laType :: Lens.Lens' ListArtifacts ArtifactCategory
laType = Lens.lens (type' :: ListArtifacts -> ArtifactCategory) (\s a -> s {type' = a} :: ListArtifacts)
{-# DEPRECATED laType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Page.AWSPager ListArtifacts where
  page rq rs
    | Page.stop (rs Lens.^. larsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. larsArtifacts) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& laNextToken Lens..~ rs Lens.^. larsNextToken

instance Lude.AWSRequest ListArtifacts where
  type Rs ListArtifacts = ListArtifactsResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListArtifactsResponse'
            Lude.<$> (x Lude..?> "artifacts" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListArtifacts where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.ListArtifacts" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListArtifacts where
  toJSON ListArtifacts' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("arn" Lude..= arn),
            ("nextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("type" Lude..= type')
          ]
      )

instance Lude.ToPath ListArtifacts where
  toPath = Lude.const "/"

instance Lude.ToQuery ListArtifacts where
  toQuery = Lude.const Lude.mempty

-- | Represents the result of a list artifacts operation.
--
-- /See:/ 'mkListArtifactsResponse' smart constructor.
data ListArtifactsResponse = ListArtifactsResponse'
  { -- | Information about the artifacts.
    artifacts :: Lude.Maybe [Artifact],
    -- | If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListArtifactsResponse' with the minimum fields required to make a request.
--
-- * 'artifacts' - Information about the artifacts.
-- * 'nextToken' - If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
-- * 'responseStatus' - The response status code.
mkListArtifactsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListArtifactsResponse
mkListArtifactsResponse pResponseStatus_ =
  ListArtifactsResponse'
    { artifacts = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the artifacts.
--
-- /Note:/ Consider using 'artifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsArtifacts :: Lens.Lens' ListArtifactsResponse (Lude.Maybe [Artifact])
larsArtifacts = Lens.lens (artifacts :: ListArtifactsResponse -> Lude.Maybe [Artifact]) (\s a -> s {artifacts = a} :: ListArtifactsResponse)
{-# DEPRECATED larsArtifacts "Use generic-lens or generic-optics with 'artifacts' instead." #-}

-- | If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsNextToken :: Lens.Lens' ListArtifactsResponse (Lude.Maybe Lude.Text)
larsNextToken = Lens.lens (nextToken :: ListArtifactsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListArtifactsResponse)
{-# DEPRECATED larsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsResponseStatus :: Lens.Lens' ListArtifactsResponse Lude.Int
larsResponseStatus = Lens.lens (responseStatus :: ListArtifactsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListArtifactsResponse)
{-# DEPRECATED larsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
