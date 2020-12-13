{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetStage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a 'Stage' resource.
module Network.AWS.APIGateway.GetStage
  ( -- * Creating a request
    GetStage (..),
    mkGetStage,

    -- ** Request lenses
    gsfRestAPIId,
    gsfStageName,

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

-- | Requests API Gateway to get information about a 'Stage' resource.
--
-- /See:/ 'mkGetStage' smart constructor.
data GetStage = GetStage'
  { -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text,
    -- | [Required] The name of the 'Stage' resource to get information about.
    stageName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetStage' with the minimum fields required to make a request.
--
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'stageName' - [Required] The name of the 'Stage' resource to get information about.
mkGetStage ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'stageName'
  Lude.Text ->
  GetStage
mkGetStage pRestAPIId_ pStageName_ =
  GetStage' {restAPIId = pRestAPIId_, stageName = pStageName_}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsfRestAPIId :: Lens.Lens' GetStage Lude.Text
gsfRestAPIId = Lens.lens (restAPIId :: GetStage -> Lude.Text) (\s a -> s {restAPIId = a} :: GetStage)
{-# DEPRECATED gsfRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] The name of the 'Stage' resource to get information about.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsfStageName :: Lens.Lens' GetStage Lude.Text
gsfStageName = Lens.lens (stageName :: GetStage -> Lude.Text) (\s a -> s {stageName = a} :: GetStage)
{-# DEPRECATED gsfStageName "Use generic-lens or generic-optics with 'stageName' instead." #-}

instance Lude.AWSRequest GetStage where
  type Rs GetStage = Stage
  request = Req.get apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders GetStage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetStage where
  toPath GetStage' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/stages/",
        Lude.toBS stageName
      ]

instance Lude.ToQuery GetStage where
  toQuery = Lude.const Lude.mempty
