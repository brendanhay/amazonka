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
-- Module      : Amazonka.Lambda.RemovePermission
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes function-use permission from an Amazon Web Services service or
-- another account. You can get the ID of the statement from the output of
-- GetPolicy.
module Amazonka.Lambda.RemovePermission
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemovePermission' smart constructor.
data RemovePermission = RemovePermission'
  { -- | Only update the policy if the revision ID matches the ID that\'s
    -- specified. Use this option to avoid modifying a policy that has changed
    -- since you last read it.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | Specify a version or alias to remove permissions from a published
    -- version of the function.
    qualifier :: Prelude.Maybe Prelude.Text,
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
    functionName :: Prelude.Text,
    -- | Statement ID of the permission to remove.
    statementId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'statementId'
  Prelude.Text ->
  RemovePermission
newRemovePermission pFunctionName_ pStatementId_ =
  RemovePermission'
    { revisionId = Prelude.Nothing,
      qualifier = Prelude.Nothing,
      functionName = pFunctionName_,
      statementId = pStatementId_
    }

-- | Only update the policy if the revision ID matches the ID that\'s
-- specified. Use this option to avoid modifying a policy that has changed
-- since you last read it.
removePermission_revisionId :: Lens.Lens' RemovePermission (Prelude.Maybe Prelude.Text)
removePermission_revisionId = Lens.lens (\RemovePermission' {revisionId} -> revisionId) (\s@RemovePermission' {} a -> s {revisionId = a} :: RemovePermission)

-- | Specify a version or alias to remove permissions from a published
-- version of the function.
removePermission_qualifier :: Lens.Lens' RemovePermission (Prelude.Maybe Prelude.Text)
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
removePermission_functionName :: Lens.Lens' RemovePermission Prelude.Text
removePermission_functionName = Lens.lens (\RemovePermission' {functionName} -> functionName) (\s@RemovePermission' {} a -> s {functionName = a} :: RemovePermission)

-- | Statement ID of the permission to remove.
removePermission_statementId :: Lens.Lens' RemovePermission Prelude.Text
removePermission_statementId = Lens.lens (\RemovePermission' {statementId} -> statementId) (\s@RemovePermission' {} a -> s {statementId = a} :: RemovePermission)

instance Core.AWSRequest RemovePermission where
  type
    AWSResponse RemovePermission =
      RemovePermissionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull RemovePermissionResponse'

instance Prelude.Hashable RemovePermission where
  hashWithSalt _salt RemovePermission' {..} =
    _salt `Prelude.hashWithSalt` revisionId
      `Prelude.hashWithSalt` qualifier
      `Prelude.hashWithSalt` functionName
      `Prelude.hashWithSalt` statementId

instance Prelude.NFData RemovePermission where
  rnf RemovePermission' {..} =
    Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf qualifier
      `Prelude.seq` Prelude.rnf functionName
      `Prelude.seq` Prelude.rnf statementId

instance Data.ToHeaders RemovePermission where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RemovePermission where
  toPath RemovePermission' {..} =
    Prelude.mconcat
      [ "/2015-03-31/functions/",
        Data.toBS functionName,
        "/policy/",
        Data.toBS statementId
      ]

instance Data.ToQuery RemovePermission where
  toQuery RemovePermission' {..} =
    Prelude.mconcat
      [ "RevisionId" Data.=: revisionId,
        "Qualifier" Data.=: qualifier
      ]

-- | /See:/ 'newRemovePermissionResponse' smart constructor.
data RemovePermissionResponse = RemovePermissionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemovePermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRemovePermissionResponse ::
  RemovePermissionResponse
newRemovePermissionResponse =
  RemovePermissionResponse'

instance Prelude.NFData RemovePermissionResponse where
  rnf _ = ()
