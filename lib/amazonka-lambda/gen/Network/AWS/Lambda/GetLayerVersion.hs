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
    Types.GetLayerVersionResponse (..),
    Types.mkGetLayerVersionResponse,

    -- ** Response lenses
    Types.glvrCompatibleRuntimes,
    Types.glvrContent,
    Types.glvrCreatedDate,
    Types.glvrDescription,
    Types.glvrLayerArn,
    Types.glvrLayerVersionArn,
    Types.glvrLicenseInfo,
    Types.glvrVersion,
  )
where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetLayerVersion' smart constructor.
data GetLayerVersion = GetLayerVersion'
  { -- | The name or Amazon Resource Name (ARN) of the layer.
    layerName :: Types.LayerName,
    -- | The version number.
    versionNumber :: Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetLayerVersion' value with any optional fields omitted.
mkGetLayerVersion ::
  -- | 'layerName'
  Types.LayerName ->
  -- | 'versionNumber'
  Core.Integer ->
  GetLayerVersion
mkGetLayerVersion layerName versionNumber =
  GetLayerVersion' {layerName, versionNumber}

-- | The name or Amazon Resource Name (ARN) of the layer.
--
-- /Note:/ Consider using 'layerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvLayerName :: Lens.Lens' GetLayerVersion Types.LayerName
glvLayerName = Lens.field @"layerName"
{-# DEPRECATED glvLayerName "Use generic-lens or generic-optics with 'layerName' instead." #-}

-- | The version number.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvVersionNumber :: Lens.Lens' GetLayerVersion Core.Integer
glvVersionNumber = Lens.field @"versionNumber"
{-# DEPRECATED glvVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

instance Core.AWSRequest GetLayerVersion where
  type Rs GetLayerVersion = Types.GetLayerVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/2018-10-31/layers/" Core.<> (Core.toText layerName)
                Core.<> ("/versions/")
                Core.<> (Core.toText versionNumber)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response = Response.receiveJSON (\s h x -> Core.eitherParseJSON x)
