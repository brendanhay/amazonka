{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.RemovePermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes function-use permission from an AWS service or another account. You can get the ID of the statement from the output of 'GetPolicy' .
module Network.AWS.Lambda.RemovePermission
  ( -- * Creating a request
    RemovePermission (..),
    mkRemovePermission,

    -- ** Request lenses
    rpFunctionName,
    rpQualifier,
    rpStatementId,
    rpRevisionId,

    -- * Destructuring the response
    RemovePermissionResponse (..),
    mkRemovePermissionResponse,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRemovePermission' smart constructor.
data RemovePermission = RemovePermission'
  { -- | The name of the Lambda function, version, or alias.
    --
    -- __Name formats__
    --
    --     * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).
    --
    --
    --     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
    --
    --
    --     * __Partial ARN__ - @123456789012:function:my-function@ .
    --
    --
    -- You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
    functionName :: Lude.Text,
    -- | Specify a version or alias to remove permissions from a published version of the function.
    qualifier :: Lude.Maybe Lude.Text,
    -- | Statement ID of the permission to remove.
    statementId :: Lude.Text,
    -- | Only update the policy if the revision ID matches the ID that's specified. Use this option to avoid modifying a policy that has changed since you last read it.
    revisionId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemovePermission' with the minimum fields required to make a request.
--
-- * 'functionName' - The name of the Lambda function, version, or alias.
--
-- __Name formats__
--
--     * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
--
--
--     * __Partial ARN__ - @123456789012:function:my-function@ .
--
--
-- You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
-- * 'qualifier' - Specify a version or alias to remove permissions from a published version of the function.
-- * 'statementId' - Statement ID of the permission to remove.
-- * 'revisionId' - Only update the policy if the revision ID matches the ID that's specified. Use this option to avoid modifying a policy that has changed since you last read it.
mkRemovePermission ::
  -- | 'functionName'
  Lude.Text ->
  -- | 'statementId'
  Lude.Text ->
  RemovePermission
mkRemovePermission pFunctionName_ pStatementId_ =
  RemovePermission'
    { functionName = pFunctionName_,
      qualifier = Lude.Nothing,
      statementId = pStatementId_,
      revisionId = Lude.Nothing
    }

-- | The name of the Lambda function, version, or alias.
--
-- __Name formats__
--
--     * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
--
--
--     * __Partial ARN__ - @123456789012:function:my-function@ .
--
--
-- You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpFunctionName :: Lens.Lens' RemovePermission Lude.Text
rpFunctionName = Lens.lens (functionName :: RemovePermission -> Lude.Text) (\s a -> s {functionName = a} :: RemovePermission)
{-# DEPRECATED rpFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | Specify a version or alias to remove permissions from a published version of the function.
--
-- /Note:/ Consider using 'qualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpQualifier :: Lens.Lens' RemovePermission (Lude.Maybe Lude.Text)
rpQualifier = Lens.lens (qualifier :: RemovePermission -> Lude.Maybe Lude.Text) (\s a -> s {qualifier = a} :: RemovePermission)
{-# DEPRECATED rpQualifier "Use generic-lens or generic-optics with 'qualifier' instead." #-}

-- | Statement ID of the permission to remove.
--
-- /Note:/ Consider using 'statementId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpStatementId :: Lens.Lens' RemovePermission Lude.Text
rpStatementId = Lens.lens (statementId :: RemovePermission -> Lude.Text) (\s a -> s {statementId = a} :: RemovePermission)
{-# DEPRECATED rpStatementId "Use generic-lens or generic-optics with 'statementId' instead." #-}

-- | Only update the policy if the revision ID matches the ID that's specified. Use this option to avoid modifying a policy that has changed since you last read it.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpRevisionId :: Lens.Lens' RemovePermission (Lude.Maybe Lude.Text)
rpRevisionId = Lens.lens (revisionId :: RemovePermission -> Lude.Maybe Lude.Text) (\s a -> s {revisionId = a} :: RemovePermission)
{-# DEPRECATED rpRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

instance Lude.AWSRequest RemovePermission where
  type Rs RemovePermission = RemovePermissionResponse
  request = Req.delete lambdaService
  response = Res.receiveNull RemovePermissionResponse'

instance Lude.ToHeaders RemovePermission where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RemovePermission where
  toPath RemovePermission' {..} =
    Lude.mconcat
      [ "/2015-03-31/functions/",
        Lude.toBS functionName,
        "/policy/",
        Lude.toBS statementId
      ]

instance Lude.ToQuery RemovePermission where
  toQuery RemovePermission' {..} =
    Lude.mconcat
      ["Qualifier" Lude.=: qualifier, "RevisionId" Lude.=: revisionId]

-- | /See:/ 'mkRemovePermissionResponse' smart constructor.
data RemovePermissionResponse = RemovePermissionResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemovePermissionResponse' with the minimum fields required to make a request.
mkRemovePermissionResponse ::
  RemovePermissionResponse
mkRemovePermissionResponse = RemovePermissionResponse'
