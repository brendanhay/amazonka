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
    alvpAction,
    alvpVersionNumber,
    alvpPrincipal,
    alvpStatementId,
    alvpRevisionId,
    alvpOrganizationId,

    -- * Destructuring the response
    AddLayerVersionPermissionResponse (..),
    mkAddLayerVersionPermissionResponse,

    -- ** Response lenses
    alvprsStatement,
    alvprsRevisionId,
    alvprsResponseStatus,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAddLayerVersionPermission' smart constructor.
data AddLayerVersionPermission = AddLayerVersionPermission'
  { -- | The name or Amazon Resource Name (ARN) of the layer.
    layerName :: Lude.Text,
    -- | The API action that grants access to the layer. For example, @lambda:GetLayerVersion@ .
    action :: Lude.Text,
    -- | The version number.
    versionNumber :: Lude.Integer,
    -- | An account ID, or @*@ to grant permission to all AWS accounts.
    principal :: Lude.Text,
    -- | An identifier that distinguishes the policy from others on the same layer version.
    statementId :: Lude.Text,
    -- | Only update the policy if the revision ID matches the ID specified. Use this option to avoid modifying a policy that has changed since you last read it.
    revisionId :: Lude.Maybe Lude.Text,
    -- | With the principal set to @*@ , grant permission to all accounts in the specified organization.
    organizationId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddLayerVersionPermission' with the minimum fields required to make a request.
--
-- * 'layerName' - The name or Amazon Resource Name (ARN) of the layer.
-- * 'action' - The API action that grants access to the layer. For example, @lambda:GetLayerVersion@ .
-- * 'versionNumber' - The version number.
-- * 'principal' - An account ID, or @*@ to grant permission to all AWS accounts.
-- * 'statementId' - An identifier that distinguishes the policy from others on the same layer version.
-- * 'revisionId' - Only update the policy if the revision ID matches the ID specified. Use this option to avoid modifying a policy that has changed since you last read it.
-- * 'organizationId' - With the principal set to @*@ , grant permission to all accounts in the specified organization.
mkAddLayerVersionPermission ::
  -- | 'layerName'
  Lude.Text ->
  -- | 'action'
  Lude.Text ->
  -- | 'versionNumber'
  Lude.Integer ->
  -- | 'principal'
  Lude.Text ->
  -- | 'statementId'
  Lude.Text ->
  AddLayerVersionPermission
mkAddLayerVersionPermission
  pLayerName_
  pAction_
  pVersionNumber_
  pPrincipal_
  pStatementId_ =
    AddLayerVersionPermission'
      { layerName = pLayerName_,
        action = pAction_,
        versionNumber = pVersionNumber_,
        principal = pPrincipal_,
        statementId = pStatementId_,
        revisionId = Lude.Nothing,
        organizationId = Lude.Nothing
      }

-- | The name or Amazon Resource Name (ARN) of the layer.
--
-- /Note:/ Consider using 'layerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alvpLayerName :: Lens.Lens' AddLayerVersionPermission Lude.Text
alvpLayerName = Lens.lens (layerName :: AddLayerVersionPermission -> Lude.Text) (\s a -> s {layerName = a} :: AddLayerVersionPermission)
{-# DEPRECATED alvpLayerName "Use generic-lens or generic-optics with 'layerName' instead." #-}

-- | The API action that grants access to the layer. For example, @lambda:GetLayerVersion@ .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alvpAction :: Lens.Lens' AddLayerVersionPermission Lude.Text
alvpAction = Lens.lens (action :: AddLayerVersionPermission -> Lude.Text) (\s a -> s {action = a} :: AddLayerVersionPermission)
{-# DEPRECATED alvpAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The version number.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alvpVersionNumber :: Lens.Lens' AddLayerVersionPermission Lude.Integer
alvpVersionNumber = Lens.lens (versionNumber :: AddLayerVersionPermission -> Lude.Integer) (\s a -> s {versionNumber = a} :: AddLayerVersionPermission)
{-# DEPRECATED alvpVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

-- | An account ID, or @*@ to grant permission to all AWS accounts.
--
-- /Note:/ Consider using 'principal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alvpPrincipal :: Lens.Lens' AddLayerVersionPermission Lude.Text
alvpPrincipal = Lens.lens (principal :: AddLayerVersionPermission -> Lude.Text) (\s a -> s {principal = a} :: AddLayerVersionPermission)
{-# DEPRECATED alvpPrincipal "Use generic-lens or generic-optics with 'principal' instead." #-}

-- | An identifier that distinguishes the policy from others on the same layer version.
--
-- /Note:/ Consider using 'statementId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alvpStatementId :: Lens.Lens' AddLayerVersionPermission Lude.Text
alvpStatementId = Lens.lens (statementId :: AddLayerVersionPermission -> Lude.Text) (\s a -> s {statementId = a} :: AddLayerVersionPermission)
{-# DEPRECATED alvpStatementId "Use generic-lens or generic-optics with 'statementId' instead." #-}

-- | Only update the policy if the revision ID matches the ID specified. Use this option to avoid modifying a policy that has changed since you last read it.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alvpRevisionId :: Lens.Lens' AddLayerVersionPermission (Lude.Maybe Lude.Text)
alvpRevisionId = Lens.lens (revisionId :: AddLayerVersionPermission -> Lude.Maybe Lude.Text) (\s a -> s {revisionId = a} :: AddLayerVersionPermission)
{-# DEPRECATED alvpRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

-- | With the principal set to @*@ , grant permission to all accounts in the specified organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alvpOrganizationId :: Lens.Lens' AddLayerVersionPermission (Lude.Maybe Lude.Text)
alvpOrganizationId = Lens.lens (organizationId :: AddLayerVersionPermission -> Lude.Maybe Lude.Text) (\s a -> s {organizationId = a} :: AddLayerVersionPermission)
{-# DEPRECATED alvpOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Lude.AWSRequest AddLayerVersionPermission where
  type
    Rs AddLayerVersionPermission =
      AddLayerVersionPermissionResponse
  request = Req.postJSON lambdaService
  response =
    Res.receiveJSON
      ( \s h x ->
          AddLayerVersionPermissionResponse'
            Lude.<$> (x Lude..?> "Statement")
            Lude.<*> (x Lude..?> "RevisionId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddLayerVersionPermission where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON AddLayerVersionPermission where
  toJSON AddLayerVersionPermission' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Action" Lude..= action),
            Lude.Just ("Principal" Lude..= principal),
            Lude.Just ("StatementId" Lude..= statementId),
            ("OrganizationId" Lude..=) Lude.<$> organizationId
          ]
      )

instance Lude.ToPath AddLayerVersionPermission where
  toPath AddLayerVersionPermission' {..} =
    Lude.mconcat
      [ "/2018-10-31/layers/",
        Lude.toBS layerName,
        "/versions/",
        Lude.toBS versionNumber,
        "/policy"
      ]

instance Lude.ToQuery AddLayerVersionPermission where
  toQuery AddLayerVersionPermission' {..} =
    Lude.mconcat ["RevisionId" Lude.=: revisionId]

-- | /See:/ 'mkAddLayerVersionPermissionResponse' smart constructor.
data AddLayerVersionPermissionResponse = AddLayerVersionPermissionResponse'
  { -- | The permission statement.
    statement :: Lude.Maybe Lude.Text,
    -- | A unique identifier for the current revision of the policy.
    revisionId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddLayerVersionPermissionResponse' with the minimum fields required to make a request.
--
-- * 'statement' - The permission statement.
-- * 'revisionId' - A unique identifier for the current revision of the policy.
-- * 'responseStatus' - The response status code.
mkAddLayerVersionPermissionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddLayerVersionPermissionResponse
mkAddLayerVersionPermissionResponse pResponseStatus_ =
  AddLayerVersionPermissionResponse'
    { statement = Lude.Nothing,
      revisionId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The permission statement.
--
-- /Note:/ Consider using 'statement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alvprsStatement :: Lens.Lens' AddLayerVersionPermissionResponse (Lude.Maybe Lude.Text)
alvprsStatement = Lens.lens (statement :: AddLayerVersionPermissionResponse -> Lude.Maybe Lude.Text) (\s a -> s {statement = a} :: AddLayerVersionPermissionResponse)
{-# DEPRECATED alvprsStatement "Use generic-lens or generic-optics with 'statement' instead." #-}

-- | A unique identifier for the current revision of the policy.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alvprsRevisionId :: Lens.Lens' AddLayerVersionPermissionResponse (Lude.Maybe Lude.Text)
alvprsRevisionId = Lens.lens (revisionId :: AddLayerVersionPermissionResponse -> Lude.Maybe Lude.Text) (\s a -> s {revisionId = a} :: AddLayerVersionPermissionResponse)
{-# DEPRECATED alvprsRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alvprsResponseStatus :: Lens.Lens' AddLayerVersionPermissionResponse Lude.Int
alvprsResponseStatus = Lens.lens (responseStatus :: AddLayerVersionPermissionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddLayerVersionPermissionResponse)
{-# DEPRECATED alvprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
