{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.AddLayerVersionPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds permissions to the resource-based policy of a version of an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> . Use this action to grant layer usage permission to other accounts. You can grant permission to a single account, all AWS accounts, or all accounts in an organization.
--
-- To revoke permission, call 'RemoveLayerVersionPermission' with the statement ID that you specified when you added it.
module Network.AWS.Lambda.AddLayerVersionPermission
  ( -- * Creating a request
    AddLayerVersionPermission (..),
    mkAddLayerVersionPermission,

    -- ** Request lenses
    alvpLayerName,
    alvpVersionNumber,
    alvpStatementId,
    alvpAction,
    alvpPrincipal,
    alvpOrganizationId,
    alvpRevisionId,

    -- * Destructuring the response
    AddLayerVersionPermissionResponse (..),
    mkAddLayerVersionPermissionResponse,

    -- ** Response lenses
    alvprrsRevisionId,
    alvprrsStatement,
    alvprrsResponseStatus,
  )
where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAddLayerVersionPermission' smart constructor.
data AddLayerVersionPermission = AddLayerVersionPermission'
  { -- | The name or Amazon Resource Name (ARN) of the layer.
    layerName :: Types.LayerName,
    -- | The version number.
    versionNumber :: Core.Integer,
    -- | An identifier that distinguishes the policy from others on the same layer version.
    statementId :: Types.StatementId,
    -- | The API action that grants access to the layer. For example, @lambda:GetLayerVersion@ .
    action :: Types.LayerPermissionAllowedAction,
    -- | An account ID, or @*@ to grant permission to all AWS accounts.
    principal :: Types.LayerPermissionAllowedPrincipal,
    -- | With the principal set to @*@ , grant permission to all accounts in the specified organization.
    organizationId :: Core.Maybe Types.OrganizationId,
    -- | Only update the policy if the revision ID matches the ID specified. Use this option to avoid modifying a policy that has changed since you last read it.
    revisionId :: Core.Maybe Types.RevisionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddLayerVersionPermission' value with any optional fields omitted.
mkAddLayerVersionPermission ::
  -- | 'layerName'
  Types.LayerName ->
  -- | 'versionNumber'
  Core.Integer ->
  -- | 'statementId'
  Types.StatementId ->
  -- | 'action'
  Types.LayerPermissionAllowedAction ->
  -- | 'principal'
  Types.LayerPermissionAllowedPrincipal ->
  AddLayerVersionPermission
mkAddLayerVersionPermission
  layerName
  versionNumber
  statementId
  action
  principal =
    AddLayerVersionPermission'
      { layerName,
        versionNumber,
        statementId,
        action,
        principal,
        organizationId = Core.Nothing,
        revisionId = Core.Nothing
      }

-- | The name or Amazon Resource Name (ARN) of the layer.
--
-- /Note:/ Consider using 'layerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alvpLayerName :: Lens.Lens' AddLayerVersionPermission Types.LayerName
alvpLayerName = Lens.field @"layerName"
{-# DEPRECATED alvpLayerName "Use generic-lens or generic-optics with 'layerName' instead." #-}

-- | The version number.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alvpVersionNumber :: Lens.Lens' AddLayerVersionPermission Core.Integer
alvpVersionNumber = Lens.field @"versionNumber"
{-# DEPRECATED alvpVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

-- | An identifier that distinguishes the policy from others on the same layer version.
--
-- /Note:/ Consider using 'statementId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alvpStatementId :: Lens.Lens' AddLayerVersionPermission Types.StatementId
alvpStatementId = Lens.field @"statementId"
{-# DEPRECATED alvpStatementId "Use generic-lens or generic-optics with 'statementId' instead." #-}

-- | The API action that grants access to the layer. For example, @lambda:GetLayerVersion@ .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alvpAction :: Lens.Lens' AddLayerVersionPermission Types.LayerPermissionAllowedAction
alvpAction = Lens.field @"action"
{-# DEPRECATED alvpAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | An account ID, or @*@ to grant permission to all AWS accounts.
--
-- /Note:/ Consider using 'principal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alvpPrincipal :: Lens.Lens' AddLayerVersionPermission Types.LayerPermissionAllowedPrincipal
alvpPrincipal = Lens.field @"principal"
{-# DEPRECATED alvpPrincipal "Use generic-lens or generic-optics with 'principal' instead." #-}

-- | With the principal set to @*@ , grant permission to all accounts in the specified organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alvpOrganizationId :: Lens.Lens' AddLayerVersionPermission (Core.Maybe Types.OrganizationId)
alvpOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED alvpOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | Only update the policy if the revision ID matches the ID specified. Use this option to avoid modifying a policy that has changed since you last read it.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alvpRevisionId :: Lens.Lens' AddLayerVersionPermission (Core.Maybe Types.RevisionId)
alvpRevisionId = Lens.field @"revisionId"
{-# DEPRECATED alvpRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

instance Core.FromJSON AddLayerVersionPermission where
  toJSON AddLayerVersionPermission {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StatementId" Core..= statementId),
            Core.Just ("Action" Core..= action),
            Core.Just ("Principal" Core..= principal),
            ("OrganizationId" Core..=) Core.<$> organizationId
          ]
      )

instance Core.AWSRequest AddLayerVersionPermission where
  type
    Rs AddLayerVersionPermission =
      AddLayerVersionPermissionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/2018-10-31/layers/" Core.<> (Core.toText layerName)
                Core.<> ("/versions/")
                Core.<> (Core.toText versionNumber)
                Core.<> ("/policy")
            ),
        Core._rqQuery = Core.toQueryValue "RevisionId" Core.<$> revisionId,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          AddLayerVersionPermissionResponse'
            Core.<$> (x Core..:? "RevisionId")
            Core.<*> (x Core..:? "Statement")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAddLayerVersionPermissionResponse' smart constructor.
data AddLayerVersionPermissionResponse = AddLayerVersionPermissionResponse'
  { -- | A unique identifier for the current revision of the policy.
    revisionId :: Core.Maybe Types.String,
    -- | The permission statement.
    statement :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddLayerVersionPermissionResponse' value with any optional fields omitted.
mkAddLayerVersionPermissionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AddLayerVersionPermissionResponse
mkAddLayerVersionPermissionResponse responseStatus =
  AddLayerVersionPermissionResponse'
    { revisionId = Core.Nothing,
      statement = Core.Nothing,
      responseStatus
    }

-- | A unique identifier for the current revision of the policy.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alvprrsRevisionId :: Lens.Lens' AddLayerVersionPermissionResponse (Core.Maybe Types.String)
alvprrsRevisionId = Lens.field @"revisionId"
{-# DEPRECATED alvprrsRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

-- | The permission statement.
--
-- /Note:/ Consider using 'statement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alvprrsStatement :: Lens.Lens' AddLayerVersionPermissionResponse (Core.Maybe Types.String)
alvprrsStatement = Lens.field @"statement"
{-# DEPRECATED alvprrsStatement "Use generic-lens or generic-optics with 'statement' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alvprrsResponseStatus :: Lens.Lens' AddLayerVersionPermissionResponse Core.Int
alvprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED alvprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
