{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.RemoveLayerVersionPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a statement from the permissions policy for a version of an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> . For more information, see 'AddLayerVersionPermission' .
module Network.AWS.Lambda.RemoveLayerVersionPermission
  ( -- * Creating a request
    RemoveLayerVersionPermission (..),
    mkRemoveLayerVersionPermission,

    -- ** Request lenses
    rlvpLayerName,
    rlvpVersionNumber,
    rlvpStatementId,
    rlvpRevisionId,

    -- * Destructuring the response
    RemoveLayerVersionPermissionResponse (..),
    mkRemoveLayerVersionPermissionResponse,
  )
where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRemoveLayerVersionPermission' smart constructor.
data RemoveLayerVersionPermission = RemoveLayerVersionPermission'
  { -- | The name or Amazon Resource Name (ARN) of the layer.
    layerName :: Types.LayerName,
    -- | The version number.
    versionNumber :: Core.Integer,
    -- | The identifier that was specified when the statement was added.
    statementId :: Types.StatementId,
    -- | Only update the policy if the revision ID matches the ID specified. Use this option to avoid modifying a policy that has changed since you last read it.
    revisionId :: Core.Maybe Types.RevisionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveLayerVersionPermission' value with any optional fields omitted.
mkRemoveLayerVersionPermission ::
  -- | 'layerName'
  Types.LayerName ->
  -- | 'versionNumber'
  Core.Integer ->
  -- | 'statementId'
  Types.StatementId ->
  RemoveLayerVersionPermission
mkRemoveLayerVersionPermission layerName versionNumber statementId =
  RemoveLayerVersionPermission'
    { layerName,
      versionNumber,
      statementId,
      revisionId = Core.Nothing
    }

-- | The name or Amazon Resource Name (ARN) of the layer.
--
-- /Note:/ Consider using 'layerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlvpLayerName :: Lens.Lens' RemoveLayerVersionPermission Types.LayerName
rlvpLayerName = Lens.field @"layerName"
{-# DEPRECATED rlvpLayerName "Use generic-lens or generic-optics with 'layerName' instead." #-}

-- | The version number.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlvpVersionNumber :: Lens.Lens' RemoveLayerVersionPermission Core.Integer
rlvpVersionNumber = Lens.field @"versionNumber"
{-# DEPRECATED rlvpVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

-- | The identifier that was specified when the statement was added.
--
-- /Note:/ Consider using 'statementId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlvpStatementId :: Lens.Lens' RemoveLayerVersionPermission Types.StatementId
rlvpStatementId = Lens.field @"statementId"
{-# DEPRECATED rlvpStatementId "Use generic-lens or generic-optics with 'statementId' instead." #-}

-- | Only update the policy if the revision ID matches the ID specified. Use this option to avoid modifying a policy that has changed since you last read it.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlvpRevisionId :: Lens.Lens' RemoveLayerVersionPermission (Core.Maybe Types.RevisionId)
rlvpRevisionId = Lens.field @"revisionId"
{-# DEPRECATED rlvpRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

instance Core.AWSRequest RemoveLayerVersionPermission where
  type
    Rs RemoveLayerVersionPermission =
      RemoveLayerVersionPermissionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/2018-10-31/layers/" Core.<> (Core.toText layerName)
                Core.<> ("/versions/")
                Core.<> (Core.toText versionNumber)
                Core.<> ("/policy/")
                Core.<> (Core.toText statementId)
            ),
        Core._rqQuery = Core.toQueryValue "RevisionId" Core.<$> revisionId,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveNull RemoveLayerVersionPermissionResponse'

-- | /See:/ 'mkRemoveLayerVersionPermissionResponse' smart constructor.
data RemoveLayerVersionPermissionResponse = RemoveLayerVersionPermissionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveLayerVersionPermissionResponse' value with any optional fields omitted.
mkRemoveLayerVersionPermissionResponse ::
  RemoveLayerVersionPermissionResponse
mkRemoveLayerVersionPermissionResponse =
  RemoveLayerVersionPermissionResponse'
