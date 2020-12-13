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

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRemoveLayerVersionPermission' smart constructor.
data RemoveLayerVersionPermission = RemoveLayerVersionPermission'
  { -- | The name or Amazon Resource Name (ARN) of the layer.
    layerName :: Lude.Text,
    -- | The version number.
    versionNumber :: Lude.Integer,
    -- | The identifier that was specified when the statement was added.
    statementId :: Lude.Text,
    -- | Only update the policy if the revision ID matches the ID specified. Use this option to avoid modifying a policy that has changed since you last read it.
    revisionId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveLayerVersionPermission' with the minimum fields required to make a request.
--
-- * 'layerName' - The name or Amazon Resource Name (ARN) of the layer.
-- * 'versionNumber' - The version number.
-- * 'statementId' - The identifier that was specified when the statement was added.
-- * 'revisionId' - Only update the policy if the revision ID matches the ID specified. Use this option to avoid modifying a policy that has changed since you last read it.
mkRemoveLayerVersionPermission ::
  -- | 'layerName'
  Lude.Text ->
  -- | 'versionNumber'
  Lude.Integer ->
  -- | 'statementId'
  Lude.Text ->
  RemoveLayerVersionPermission
mkRemoveLayerVersionPermission
  pLayerName_
  pVersionNumber_
  pStatementId_ =
    RemoveLayerVersionPermission'
      { layerName = pLayerName_,
        versionNumber = pVersionNumber_,
        statementId = pStatementId_,
        revisionId = Lude.Nothing
      }

-- | The name or Amazon Resource Name (ARN) of the layer.
--
-- /Note:/ Consider using 'layerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlvpLayerName :: Lens.Lens' RemoveLayerVersionPermission Lude.Text
rlvpLayerName = Lens.lens (layerName :: RemoveLayerVersionPermission -> Lude.Text) (\s a -> s {layerName = a} :: RemoveLayerVersionPermission)
{-# DEPRECATED rlvpLayerName "Use generic-lens or generic-optics with 'layerName' instead." #-}

-- | The version number.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlvpVersionNumber :: Lens.Lens' RemoveLayerVersionPermission Lude.Integer
rlvpVersionNumber = Lens.lens (versionNumber :: RemoveLayerVersionPermission -> Lude.Integer) (\s a -> s {versionNumber = a} :: RemoveLayerVersionPermission)
{-# DEPRECATED rlvpVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

-- | The identifier that was specified when the statement was added.
--
-- /Note:/ Consider using 'statementId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlvpStatementId :: Lens.Lens' RemoveLayerVersionPermission Lude.Text
rlvpStatementId = Lens.lens (statementId :: RemoveLayerVersionPermission -> Lude.Text) (\s a -> s {statementId = a} :: RemoveLayerVersionPermission)
{-# DEPRECATED rlvpStatementId "Use generic-lens or generic-optics with 'statementId' instead." #-}

-- | Only update the policy if the revision ID matches the ID specified. Use this option to avoid modifying a policy that has changed since you last read it.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlvpRevisionId :: Lens.Lens' RemoveLayerVersionPermission (Lude.Maybe Lude.Text)
rlvpRevisionId = Lens.lens (revisionId :: RemoveLayerVersionPermission -> Lude.Maybe Lude.Text) (\s a -> s {revisionId = a} :: RemoveLayerVersionPermission)
{-# DEPRECATED rlvpRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

instance Lude.AWSRequest RemoveLayerVersionPermission where
  type
    Rs RemoveLayerVersionPermission =
      RemoveLayerVersionPermissionResponse
  request = Req.delete lambdaService
  response = Res.receiveNull RemoveLayerVersionPermissionResponse'

instance Lude.ToHeaders RemoveLayerVersionPermission where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RemoveLayerVersionPermission where
  toPath RemoveLayerVersionPermission' {..} =
    Lude.mconcat
      [ "/2018-10-31/layers/",
        Lude.toBS layerName,
        "/versions/",
        Lude.toBS versionNumber,
        "/policy/",
        Lude.toBS statementId
      ]

instance Lude.ToQuery RemoveLayerVersionPermission where
  toQuery RemoveLayerVersionPermission' {..} =
    Lude.mconcat ["RevisionId" Lude.=: revisionId]

-- | /See:/ 'mkRemoveLayerVersionPermissionResponse' smart constructor.
data RemoveLayerVersionPermissionResponse = RemoveLayerVersionPermissionResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveLayerVersionPermissionResponse' with the minimum fields required to make a request.
mkRemoveLayerVersionPermissionResponse ::
  RemoveLayerVersionPermissionResponse
mkRemoveLayerVersionPermissionResponse =
  RemoveLayerVersionPermissionResponse'
