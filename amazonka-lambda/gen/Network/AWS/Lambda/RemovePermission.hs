{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.RemovePermission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes function-use permission from an AWS service or another account.
-- You can get the ID of the statement from the output of GetPolicy.
module Network.AWS.Lambda.RemovePermission
  ( -- * Creating a Request
    RemovePermission (..),
    newRemovePermission,

    -- * Request Lenses
    removePermission_revisionId,
    removePermission_qualifier,
    removePermission_functionName,
    removePermission_statementId,

    -- * Destructuring the Response
    RemovePermissionResponse (..),
    newRemovePermissionResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRemovePermission' smart constructor.
data RemovePermission = RemovePermission'
  { -- | Only update the policy if the revision ID matches the ID that\'s
    -- specified. Use this option to avoid modifying a policy that has changed
    -- since you last read it.
    revisionId :: Core.Maybe Core.Text,
    -- | Specify a version or alias to remove permissions from a published
    -- version of the function.
    qualifier :: Core.Maybe Core.Text,
    -- | The name of the Lambda function, version, or alias.
    --
    -- __Name formats__
    --
    -- -   __Function name__ - @my-function@ (name-only), @my-function:v1@
    --     (with alias).
    --
    -- -   __Function ARN__ -
    --     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
    --
    -- -   __Partial ARN__ - @123456789012:function:my-function@.
    --
    -- You can append a version number or alias to any of the formats. The
    -- length constraint applies only to the full ARN. If you specify only the
    -- function name, it is limited to 64 characters in length.
    functionName :: Core.Text,
    -- | Statement ID of the permission to remove.
    statementId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemovePermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revisionId', 'removePermission_revisionId' - Only update the policy if the revision ID matches the ID that\'s
-- specified. Use this option to avoid modifying a policy that has changed
-- since you last read it.
--
-- 'qualifier', 'removePermission_qualifier' - Specify a version or alias to remove permissions from a published
-- version of the function.
--
-- 'functionName', 'removePermission_functionName' - The name of the Lambda function, version, or alias.
--
-- __Name formats__
--
-- -   __Function name__ - @my-function@ (name-only), @my-function:v1@
--     (with alias).
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
--
-- You can append a version number or alias to any of the formats. The
-- length constraint applies only to the full ARN. If you specify only the
-- function name, it is limited to 64 characters in length.
--
-- 'statementId', 'removePermission_statementId' - Statement ID of the permission to remove.
newRemovePermission ::
  -- | 'functionName'
  Core.Text ->
  -- | 'statementId'
  Core.Text ->
  RemovePermission
newRemovePermission pFunctionName_ pStatementId_ =
  RemovePermission'
    { revisionId = Core.Nothing,
      qualifier = Core.Nothing,
      functionName = pFunctionName_,
      statementId = pStatementId_
    }

-- | Only update the policy if the revision ID matches the ID that\'s
-- specified. Use this option to avoid modifying a policy that has changed
-- since you last read it.
removePermission_revisionId :: Lens.Lens' RemovePermission (Core.Maybe Core.Text)
removePermission_revisionId = Lens.lens (\RemovePermission' {revisionId} -> revisionId) (\s@RemovePermission' {} a -> s {revisionId = a} :: RemovePermission)

-- | Specify a version or alias to remove permissions from a published
-- version of the function.
removePermission_qualifier :: Lens.Lens' RemovePermission (Core.Maybe Core.Text)
removePermission_qualifier = Lens.lens (\RemovePermission' {qualifier} -> qualifier) (\s@RemovePermission' {} a -> s {qualifier = a} :: RemovePermission)

-- | The name of the Lambda function, version, or alias.
--
-- __Name formats__
--
-- -   __Function name__ - @my-function@ (name-only), @my-function:v1@
--     (with alias).
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
--
-- You can append a version number or alias to any of the formats. The
-- length constraint applies only to the full ARN. If you specify only the
-- function name, it is limited to 64 characters in length.
removePermission_functionName :: Lens.Lens' RemovePermission Core.Text
removePermission_functionName = Lens.lens (\RemovePermission' {functionName} -> functionName) (\s@RemovePermission' {} a -> s {functionName = a} :: RemovePermission)

-- | Statement ID of the permission to remove.
removePermission_statementId :: Lens.Lens' RemovePermission Core.Text
removePermission_statementId = Lens.lens (\RemovePermission' {statementId} -> statementId) (\s@RemovePermission' {} a -> s {statementId = a} :: RemovePermission)

instance Core.AWSRequest RemovePermission where
  type
    AWSResponse RemovePermission =
      RemovePermissionResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull RemovePermissionResponse'

instance Core.Hashable RemovePermission

instance Core.NFData RemovePermission

instance Core.ToHeaders RemovePermission where
  toHeaders = Core.const Core.mempty

instance Core.ToPath RemovePermission where
  toPath RemovePermission' {..} =
    Core.mconcat
      [ "/2015-03-31/functions/",
        Core.toBS functionName,
        "/policy/",
        Core.toBS statementId
      ]

instance Core.ToQuery RemovePermission where
  toQuery RemovePermission' {..} =
    Core.mconcat
      [ "RevisionId" Core.=: revisionId,
        "Qualifier" Core.=: qualifier
      ]

-- | /See:/ 'newRemovePermissionResponse' smart constructor.
data RemovePermissionResponse = RemovePermissionResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemovePermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRemovePermissionResponse ::
  RemovePermissionResponse
newRemovePermissionResponse =
  RemovePermissionResponse'

instance Core.NFData RemovePermissionResponse
