{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.GetLayerVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a version of an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> , with a link to download the layer archive that's valid for 10 minutes.
module Network.AWS.Lambda.GetLayerVersion
  ( -- * Creating a request
    GetLayerVersion (..),
    mkGetLayerVersion,

    -- ** Request lenses
    glvLayerName,
    glvVersionNumber,

    -- * Destructuring the response
    GetLayerVersionResponse (..),
    mkGetLayerVersionResponse,

    -- ** Response lenses
    glvLayerVersionARN,
    glvContent,
    glvCreatedDate,
    glvVersion,
    glvLicenseInfo,
    glvLayerARN,
    glvDescription,
    glvCompatibleRuntimes,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetLayerVersion' smart constructor.
data GetLayerVersion = GetLayerVersion'
  { -- | The name or Amazon Resource Name (ARN) of the layer.
    layerName :: Lude.Text,
    -- | The version number.
    versionNumber :: Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetLayerVersion' with the minimum fields required to make a request.
--
-- * 'layerName' - The name or Amazon Resource Name (ARN) of the layer.
-- * 'versionNumber' - The version number.
mkGetLayerVersion ::
  -- | 'layerName'
  Lude.Text ->
  -- | 'versionNumber'
  Lude.Integer ->
  GetLayerVersion
mkGetLayerVersion pLayerName_ pVersionNumber_ =
  GetLayerVersion'
    { layerName = pLayerName_,
      versionNumber = pVersionNumber_
    }

-- | The name or Amazon Resource Name (ARN) of the layer.
--
-- /Note:/ Consider using 'layerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvLayerName :: Lens.Lens' GetLayerVersion Lude.Text
glvLayerName = Lens.lens (layerName :: GetLayerVersion -> Lude.Text) (\s a -> s {layerName = a} :: GetLayerVersion)
{-# DEPRECATED glvLayerName "Use generic-lens or generic-optics with 'layerName' instead." #-}

-- | The version number.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvVersionNumber :: Lens.Lens' GetLayerVersion Lude.Integer
glvVersionNumber = Lens.lens (versionNumber :: GetLayerVersion -> Lude.Integer) (\s a -> s {versionNumber = a} :: GetLayerVersion)
{-# DEPRECATED glvVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

instance Lude.AWSRequest GetLayerVersion where
  type Rs GetLayerVersion = GetLayerVersionResponse
  request = Req.get lambdaService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders GetLayerVersion where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetLayerVersion where
  toPath GetLayerVersion' {..} =
    Lude.mconcat
      [ "/2018-10-31/layers/",
        Lude.toBS layerName,
        "/versions/",
        Lude.toBS versionNumber
      ]

instance Lude.ToQuery GetLayerVersion where
  toQuery = Lude.const Lude.mempty
