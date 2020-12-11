{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.UpdateDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about a 'Deployment' resource.
module Network.AWS.APIGateway.UpdateDeployment
  ( -- * Creating a request
    UpdateDeployment (..),
    mkUpdateDeployment,

    -- ** Request lenses
    udPatchOperations,
    udRestAPIId,
    udDeploymentId,

    -- * Destructuring the response
    Deployment (..),
    mkDeployment,

    -- ** Response lenses
    dApiSummary,
    dCreatedDate,
    dId,
    dDescription,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Requests API Gateway to change information about a 'Deployment' resource.
--
-- /See:/ 'mkUpdateDeployment' smart constructor.
data UpdateDeployment = UpdateDeployment'
  { patchOperations ::
      Lude.Maybe [PatchOperation],
    restAPIId :: Lude.Text,
    deploymentId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDeployment' with the minimum fields required to make a request.
--
-- * 'deploymentId' - The replacement identifier for the 'Deployment' resource to change information about.
-- * 'patchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkUpdateDeployment ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'deploymentId'
  Lude.Text ->
  UpdateDeployment
mkUpdateDeployment pRestAPIId_ pDeploymentId_ =
  UpdateDeployment'
    { patchOperations = Lude.Nothing,
      restAPIId = pRestAPIId_,
      deploymentId = pDeploymentId_
    }

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udPatchOperations :: Lens.Lens' UpdateDeployment (Lude.Maybe [PatchOperation])
udPatchOperations = Lens.lens (patchOperations :: UpdateDeployment -> Lude.Maybe [PatchOperation]) (\s a -> s {patchOperations = a} :: UpdateDeployment)
{-# DEPRECATED udPatchOperations "Use generic-lens or generic-optics with 'patchOperations' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udRestAPIId :: Lens.Lens' UpdateDeployment Lude.Text
udRestAPIId = Lens.lens (restAPIId :: UpdateDeployment -> Lude.Text) (\s a -> s {restAPIId = a} :: UpdateDeployment)
{-# DEPRECATED udRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | The replacement identifier for the 'Deployment' resource to change information about.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDeploymentId :: Lens.Lens' UpdateDeployment Lude.Text
udDeploymentId = Lens.lens (deploymentId :: UpdateDeployment -> Lude.Text) (\s a -> s {deploymentId = a} :: UpdateDeployment)
{-# DEPRECATED udDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

instance Lude.AWSRequest UpdateDeployment where
  type Rs UpdateDeployment = Deployment
  request = Req.patchJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders UpdateDeployment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON UpdateDeployment where
  toJSON UpdateDeployment' {..} =
    Lude.object
      ( Lude.catMaybes
          [("patchOperations" Lude..=) Lude.<$> patchOperations]
      )

instance Lude.ToPath UpdateDeployment where
  toPath UpdateDeployment' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/deployments/",
        Lude.toBS deploymentId
      ]

instance Lude.ToQuery UpdateDeployment where
  toQuery = Lude.const Lude.mempty
