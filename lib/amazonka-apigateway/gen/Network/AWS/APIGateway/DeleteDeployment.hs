{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.DeleteDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a 'Deployment' resource. Deleting a deployment will only succeed if there are no 'Stage' resources associated with it.
module Network.AWS.APIGateway.DeleteDeployment
  ( -- * Creating a request
    DeleteDeployment (..),
    mkDeleteDeployment,

    -- ** Request lenses
    ddDeploymentId,
    ddRestAPIId,

    -- * Destructuring the response
    DeleteDeploymentResponse (..),
    mkDeleteDeploymentResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Requests API Gateway to delete a 'Deployment' resource.
--
-- /See:/ 'mkDeleteDeployment' smart constructor.
data DeleteDeployment = DeleteDeployment'
  { -- | [Required] The identifier of the 'Deployment' resource to delete.
    deploymentId :: Lude.Text,
    -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDeployment' with the minimum fields required to make a request.
--
-- * 'deploymentId' - [Required] The identifier of the 'Deployment' resource to delete.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkDeleteDeployment ::
  -- | 'deploymentId'
  Lude.Text ->
  -- | 'restAPIId'
  Lude.Text ->
  DeleteDeployment
mkDeleteDeployment pDeploymentId_ pRestAPIId_ =
  DeleteDeployment'
    { deploymentId = pDeploymentId_,
      restAPIId = pRestAPIId_
    }

-- | [Required] The identifier of the 'Deployment' resource to delete.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDeploymentId :: Lens.Lens' DeleteDeployment Lude.Text
ddDeploymentId = Lens.lens (deploymentId :: DeleteDeployment -> Lude.Text) (\s a -> s {deploymentId = a} :: DeleteDeployment)
{-# DEPRECATED ddDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddRestAPIId :: Lens.Lens' DeleteDeployment Lude.Text
ddRestAPIId = Lens.lens (restAPIId :: DeleteDeployment -> Lude.Text) (\s a -> s {restAPIId = a} :: DeleteDeployment)
{-# DEPRECATED ddRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

instance Lude.AWSRequest DeleteDeployment where
  type Rs DeleteDeployment = DeleteDeploymentResponse
  request = Req.delete apiGatewayService
  response = Res.receiveNull DeleteDeploymentResponse'

instance Lude.ToHeaders DeleteDeployment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath DeleteDeployment where
  toPath DeleteDeployment' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/deployments/",
        Lude.toBS deploymentId
      ]

instance Lude.ToQuery DeleteDeployment where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDeploymentResponse' smart constructor.
data DeleteDeploymentResponse = DeleteDeploymentResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDeploymentResponse' with the minimum fields required to make a request.
mkDeleteDeploymentResponse ::
  DeleteDeploymentResponse
mkDeleteDeploymentResponse = DeleteDeploymentResponse'
