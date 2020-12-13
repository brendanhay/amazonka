{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.UpdateStage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about a 'Stage' resource.
module Network.AWS.APIGateway.UpdateStage
  ( -- * Creating a request
    UpdateStage (..),
    mkUpdateStage,

    -- ** Request lenses
    usRestAPIId,
    usPatchOperations,
    usStageName,

    -- * Destructuring the response
    Stage (..),
    mkStage,

    -- ** Response lenses
    sDeploymentId,
    sVariables,
    sAccessLogSettings,
    sDocumentationVersion,
    sClientCertificateId,
    sTracingEnabled,
    sCreatedDate,
    sCacheClusterStatus,
    sMethodSettings,
    sLastUpdatedDate,
    sCacheClusterSize,
    sWebACLARN,
    sCanarySettings,
    sCacheClusterEnabled,
    sStageName,
    sDescription,
    sTags,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Requests API Gateway to change information about a 'Stage' resource.
--
-- /See:/ 'mkUpdateStage' smart constructor.
data UpdateStage = UpdateStage'
  { -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text,
    -- | A list of update operations to be applied to the specified resource and in the order specified in this list.
    patchOperations :: Lude.Maybe [PatchOperation],
    -- | [Required] The name of the 'Stage' resource to change information about.
    stageName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateStage' with the minimum fields required to make a request.
--
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'patchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
-- * 'stageName' - [Required] The name of the 'Stage' resource to change information about.
mkUpdateStage ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'stageName'
  Lude.Text ->
  UpdateStage
mkUpdateStage pRestAPIId_ pStageName_ =
  UpdateStage'
    { restAPIId = pRestAPIId_,
      patchOperations = Lude.Nothing,
      stageName = pStageName_
    }

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usRestAPIId :: Lens.Lens' UpdateStage Lude.Text
usRestAPIId = Lens.lens (restAPIId :: UpdateStage -> Lude.Text) (\s a -> s {restAPIId = a} :: UpdateStage)
{-# DEPRECATED usRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usPatchOperations :: Lens.Lens' UpdateStage (Lude.Maybe [PatchOperation])
usPatchOperations = Lens.lens (patchOperations :: UpdateStage -> Lude.Maybe [PatchOperation]) (\s a -> s {patchOperations = a} :: UpdateStage)
{-# DEPRECATED usPatchOperations "Use generic-lens or generic-optics with 'patchOperations' instead." #-}

-- | [Required] The name of the 'Stage' resource to change information about.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usStageName :: Lens.Lens' UpdateStage Lude.Text
usStageName = Lens.lens (stageName :: UpdateStage -> Lude.Text) (\s a -> s {stageName = a} :: UpdateStage)
{-# DEPRECATED usStageName "Use generic-lens or generic-optics with 'stageName' instead." #-}

instance Lude.AWSRequest UpdateStage where
  type Rs UpdateStage = Stage
  request = Req.patchJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders UpdateStage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON UpdateStage where
  toJSON UpdateStage' {..} =
    Lude.object
      ( Lude.catMaybes
          [("patchOperations" Lude..=) Lude.<$> patchOperations]
      )

instance Lude.ToPath UpdateStage where
  toPath UpdateStage' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/stages/",
        Lude.toBS stageName
      ]

instance Lude.ToQuery UpdateStage where
  toQuery = Lude.const Lude.mempty
