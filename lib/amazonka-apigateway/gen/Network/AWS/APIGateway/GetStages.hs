{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetStages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more 'Stage' resources.
module Network.AWS.APIGateway.GetStages
  ( -- * Creating a request
    GetStages (..),
    mkGetStages,

    -- ** Request lenses
    gsDeploymentId,
    gsRestAPIId,

    -- * Destructuring the response
    GetStagesResponse (..),
    mkGetStagesResponse,

    -- ** Response lenses
    gsrsItem,
    gsrsResponseStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Requests API Gateway to get information about one or more 'Stage' resources.
--
-- /See:/ 'mkGetStages' smart constructor.
data GetStages = GetStages'
  { deploymentId :: Lude.Maybe Lude.Text,
    restAPIId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetStages' with the minimum fields required to make a request.
--
-- * 'deploymentId' - The stages' deployment identifiers.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkGetStages ::
  -- | 'restAPIId'
  Lude.Text ->
  GetStages
mkGetStages pRestAPIId_ =
  GetStages' {deploymentId = Lude.Nothing, restAPIId = pRestAPIId_}

-- | The stages' deployment identifiers.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsDeploymentId :: Lens.Lens' GetStages (Lude.Maybe Lude.Text)
gsDeploymentId = Lens.lens (deploymentId :: GetStages -> Lude.Maybe Lude.Text) (\s a -> s {deploymentId = a} :: GetStages)
{-# DEPRECATED gsDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsRestAPIId :: Lens.Lens' GetStages Lude.Text
gsRestAPIId = Lens.lens (restAPIId :: GetStages -> Lude.Text) (\s a -> s {restAPIId = a} :: GetStages)
{-# DEPRECATED gsRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

instance Lude.AWSRequest GetStages where
  type Rs GetStages = GetStagesResponse
  request = Req.get apiGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetStagesResponse'
            Lude.<$> (x Lude..?> "item" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetStages where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetStages where
  toPath GetStages' {..} =
    Lude.mconcat ["/restapis/", Lude.toBS restAPIId, "/stages"]

instance Lude.ToQuery GetStages where
  toQuery GetStages' {..} =
    Lude.mconcat ["deploymentId" Lude.=: deploymentId]

-- | A list of 'Stage' resources that are associated with the 'ApiKey' resource.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/stages.html Deploying API in Stages>
--
-- /See:/ 'mkGetStagesResponse' smart constructor.
data GetStagesResponse = GetStagesResponse'
  { item ::
      Lude.Maybe [Stage],
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

-- | Creates a value of 'GetStagesResponse' with the minimum fields required to make a request.
--
-- * 'item' - The current page of elements from this collection.
-- * 'responseStatus' - The response status code.
mkGetStagesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetStagesResponse
mkGetStagesResponse pResponseStatus_ =
  GetStagesResponse'
    { item = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsItem :: Lens.Lens' GetStagesResponse (Lude.Maybe [Stage])
gsrsItem = Lens.lens (item :: GetStagesResponse -> Lude.Maybe [Stage]) (\s a -> s {item = a} :: GetStagesResponse)
{-# DEPRECATED gsrsItem "Use generic-lens or generic-optics with 'item' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsResponseStatus :: Lens.Lens' GetStagesResponse Lude.Int
gsrsResponseStatus = Lens.lens (responseStatus :: GetStagesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetStagesResponse)
{-# DEPRECATED gsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
