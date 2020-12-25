{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.GetLayerVersionPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the permission policy for a version of an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> . For more information, see 'AddLayerVersionPermission' .
module Network.AWS.Lambda.GetLayerVersionPolicy
  ( -- * Creating a request
    GetLayerVersionPolicy (..),
    mkGetLayerVersionPolicy,

    -- ** Request lenses
    glvpLayerName,
    glvpVersionNumber,

    -- * Destructuring the response
    GetLayerVersionPolicyResponse (..),
    mkGetLayerVersionPolicyResponse,

    -- ** Response lenses
    glvprrsPolicy,
    glvprrsRevisionId,
    glvprrsResponseStatus,
  )
where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetLayerVersionPolicy' smart constructor.
data GetLayerVersionPolicy = GetLayerVersionPolicy'
  { -- | The name or Amazon Resource Name (ARN) of the layer.
    layerName :: Types.LayerName,
    -- | The version number.
    versionNumber :: Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetLayerVersionPolicy' value with any optional fields omitted.
mkGetLayerVersionPolicy ::
  -- | 'layerName'
  Types.LayerName ->
  -- | 'versionNumber'
  Core.Integer ->
  GetLayerVersionPolicy
mkGetLayerVersionPolicy layerName versionNumber =
  GetLayerVersionPolicy' {layerName, versionNumber}

-- | The name or Amazon Resource Name (ARN) of the layer.
--
-- /Note:/ Consider using 'layerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvpLayerName :: Lens.Lens' GetLayerVersionPolicy Types.LayerName
glvpLayerName = Lens.field @"layerName"
{-# DEPRECATED glvpLayerName "Use generic-lens or generic-optics with 'layerName' instead." #-}

-- | The version number.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvpVersionNumber :: Lens.Lens' GetLayerVersionPolicy Core.Integer
glvpVersionNumber = Lens.field @"versionNumber"
{-# DEPRECATED glvpVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

instance Core.AWSRequest GetLayerVersionPolicy where
  type Rs GetLayerVersionPolicy = GetLayerVersionPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/2018-10-31/layers/" Core.<> (Core.toText layerName)
                Core.<> ("/versions/")
                Core.<> (Core.toText versionNumber)
                Core.<> ("/policy")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLayerVersionPolicyResponse'
            Core.<$> (x Core..:? "Policy")
            Core.<*> (x Core..:? "RevisionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetLayerVersionPolicyResponse' smart constructor.
data GetLayerVersionPolicyResponse = GetLayerVersionPolicyResponse'
  { -- | The policy document.
    policy :: Core.Maybe Types.String,
    -- | A unique identifier for the current revision of the policy.
    revisionId :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetLayerVersionPolicyResponse' value with any optional fields omitted.
mkGetLayerVersionPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetLayerVersionPolicyResponse
mkGetLayerVersionPolicyResponse responseStatus =
  GetLayerVersionPolicyResponse'
    { policy = Core.Nothing,
      revisionId = Core.Nothing,
      responseStatus
    }

-- | The policy document.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvprrsPolicy :: Lens.Lens' GetLayerVersionPolicyResponse (Core.Maybe Types.String)
glvprrsPolicy = Lens.field @"policy"
{-# DEPRECATED glvprrsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | A unique identifier for the current revision of the policy.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvprrsRevisionId :: Lens.Lens' GetLayerVersionPolicyResponse (Core.Maybe Types.String)
glvprrsRevisionId = Lens.field @"revisionId"
{-# DEPRECATED glvprrsRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvprrsResponseStatus :: Lens.Lens' GetLayerVersionPolicyResponse Core.Int
glvprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED glvprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
