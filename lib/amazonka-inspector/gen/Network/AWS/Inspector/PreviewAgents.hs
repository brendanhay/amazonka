{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.PreviewAgents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Previews the agents installed on the EC2 instances that are part of the specified assessment target.
--
-- This operation returns paginated results.
module Network.AWS.Inspector.PreviewAgents
  ( -- * Creating a request
    PreviewAgents (..),
    mkPreviewAgents,

    -- ** Request lenses
    paNextToken,
    paMaxResults,
    paPreviewAgentsARN,

    -- * Destructuring the response
    PreviewAgentsResponse (..),
    mkPreviewAgentsResponse,

    -- ** Response lenses
    parsNextToken,
    parsResponseStatus,
    parsAgentPreviews,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPreviewAgents' smart constructor.
data PreviewAgents = PreviewAgents'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Int,
    previewAgentsARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PreviewAgents' with the minimum fields required to make a request.
--
-- * 'maxResults' - You can use this parameter to indicate the maximum number of items you want in the response. The default value is 10. The maximum value is 500.
-- * 'nextToken' - You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __PreviewAgents__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
-- * 'previewAgentsARN' - The ARN of the assessment target whose agents you want to preview.
mkPreviewAgents ::
  -- | 'previewAgentsARN'
  Lude.Text ->
  PreviewAgents
mkPreviewAgents pPreviewAgentsARN_ =
  PreviewAgents'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      previewAgentsARN = pPreviewAgentsARN_
    }

-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __PreviewAgents__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paNextToken :: Lens.Lens' PreviewAgents (Lude.Maybe Lude.Text)
paNextToken = Lens.lens (nextToken :: PreviewAgents -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: PreviewAgents)
{-# DEPRECATED paNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | You can use this parameter to indicate the maximum number of items you want in the response. The default value is 10. The maximum value is 500.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paMaxResults :: Lens.Lens' PreviewAgents (Lude.Maybe Lude.Int)
paMaxResults = Lens.lens (maxResults :: PreviewAgents -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: PreviewAgents)
{-# DEPRECATED paMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ARN of the assessment target whose agents you want to preview.
--
-- /Note:/ Consider using 'previewAgentsARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paPreviewAgentsARN :: Lens.Lens' PreviewAgents Lude.Text
paPreviewAgentsARN = Lens.lens (previewAgentsARN :: PreviewAgents -> Lude.Text) (\s a -> s {previewAgentsARN = a} :: PreviewAgents)
{-# DEPRECATED paPreviewAgentsARN "Use generic-lens or generic-optics with 'previewAgentsARN' instead." #-}

instance Page.AWSPager PreviewAgents where
  page rq rs
    | Page.stop (rs Lens.^. parsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. parsAgentPreviews) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& paNextToken Lens..~ rs Lens.^. parsNextToken

instance Lude.AWSRequest PreviewAgents where
  type Rs PreviewAgents = PreviewAgentsResponse
  request = Req.postJSON inspectorService
  response =
    Res.receiveJSON
      ( \s h x ->
          PreviewAgentsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "agentPreviews" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders PreviewAgents where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("InspectorService.PreviewAgents" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PreviewAgents where
  toJSON PreviewAgents' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("previewAgentsArn" Lude..= previewAgentsARN)
          ]
      )

instance Lude.ToPath PreviewAgents where
  toPath = Lude.const "/"

instance Lude.ToQuery PreviewAgents where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPreviewAgentsResponse' smart constructor.
data PreviewAgentsResponse = PreviewAgentsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    agentPreviews :: [AgentPreview]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PreviewAgentsResponse' with the minimum fields required to make a request.
--
-- * 'agentPreviews' - The resulting list of agents.
-- * 'nextToken' - When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
-- * 'responseStatus' - The response status code.
mkPreviewAgentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PreviewAgentsResponse
mkPreviewAgentsResponse pResponseStatus_ =
  PreviewAgentsResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      agentPreviews = Lude.mempty
    }

-- | When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parsNextToken :: Lens.Lens' PreviewAgentsResponse (Lude.Maybe Lude.Text)
parsNextToken = Lens.lens (nextToken :: PreviewAgentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: PreviewAgentsResponse)
{-# DEPRECATED parsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parsResponseStatus :: Lens.Lens' PreviewAgentsResponse Lude.Int
parsResponseStatus = Lens.lens (responseStatus :: PreviewAgentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PreviewAgentsResponse)
{-# DEPRECATED parsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The resulting list of agents.
--
-- /Note:/ Consider using 'agentPreviews' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parsAgentPreviews :: Lens.Lens' PreviewAgentsResponse [AgentPreview]
parsAgentPreviews = Lens.lens (agentPreviews :: PreviewAgentsResponse -> [AgentPreview]) (\s a -> s {agentPreviews = a} :: PreviewAgentsResponse)
{-# DEPRECATED parsAgentPreviews "Use generic-lens or generic-optics with 'agentPreviews' instead." #-}
