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
-- Module      : Amazonka.LexV2Models.DeleteResourcePolicyStatement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a policy statement from a resource policy. If you delete the
-- last statement from a policy, the policy is deleted. If you specify a
-- statement ID that doesn\'t exist in the policy, or if the bot or bot
-- alias doesn\'t have a policy attached, Amazon Lex returns an exception.
module Amazonka.LexV2Models.DeleteResourcePolicyStatement
  ( -- * Creating a Request
    DeleteResourcePolicyStatement (..),
    newDeleteResourcePolicyStatement,

    -- * Request Lenses
    deleteResourcePolicyStatement_expectedRevisionId,
    deleteResourcePolicyStatement_resourceArn,
    deleteResourcePolicyStatement_statementId,

    -- * Destructuring the Response
    DeleteResourcePolicyStatementResponse (..),
    newDeleteResourcePolicyStatementResponse,

    -- * Response Lenses
    deleteResourcePolicyStatementResponse_revisionId,
    deleteResourcePolicyStatementResponse_resourceArn,
    deleteResourcePolicyStatementResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteResourcePolicyStatement' smart constructor.
data DeleteResourcePolicyStatement = DeleteResourcePolicyStatement'
  { -- | The identifier of the revision of the policy to delete the statement
    -- from. If this revision ID doesn\'t match the current revision ID, Amazon
    -- Lex throws an exception.
    --
    -- If you don\'t specify a revision, Amazon Lex removes the current
    -- contents of the statement.
    expectedRevisionId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the bot or bot alias that the resource
    -- policy is attached to.
    resourceArn :: Prelude.Text,
    -- | The name of the statement (SID) to delete from the policy.
    statementId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourcePolicyStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedRevisionId', 'deleteResourcePolicyStatement_expectedRevisionId' - The identifier of the revision of the policy to delete the statement
-- from. If this revision ID doesn\'t match the current revision ID, Amazon
-- Lex throws an exception.
--
-- If you don\'t specify a revision, Amazon Lex removes the current
-- contents of the statement.
--
-- 'resourceArn', 'deleteResourcePolicyStatement_resourceArn' - The Amazon Resource Name (ARN) of the bot or bot alias that the resource
-- policy is attached to.
--
-- 'statementId', 'deleteResourcePolicyStatement_statementId' - The name of the statement (SID) to delete from the policy.
newDeleteResourcePolicyStatement ::
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'statementId'
  Prelude.Text ->
  DeleteResourcePolicyStatement
newDeleteResourcePolicyStatement
  pResourceArn_
  pStatementId_ =
    DeleteResourcePolicyStatement'
      { expectedRevisionId =
          Prelude.Nothing,
        resourceArn = pResourceArn_,
        statementId = pStatementId_
      }

-- | The identifier of the revision of the policy to delete the statement
-- from. If this revision ID doesn\'t match the current revision ID, Amazon
-- Lex throws an exception.
--
-- If you don\'t specify a revision, Amazon Lex removes the current
-- contents of the statement.
deleteResourcePolicyStatement_expectedRevisionId :: Lens.Lens' DeleteResourcePolicyStatement (Prelude.Maybe Prelude.Text)
deleteResourcePolicyStatement_expectedRevisionId = Lens.lens (\DeleteResourcePolicyStatement' {expectedRevisionId} -> expectedRevisionId) (\s@DeleteResourcePolicyStatement' {} a -> s {expectedRevisionId = a} :: DeleteResourcePolicyStatement)

-- | The Amazon Resource Name (ARN) of the bot or bot alias that the resource
-- policy is attached to.
deleteResourcePolicyStatement_resourceArn :: Lens.Lens' DeleteResourcePolicyStatement Prelude.Text
deleteResourcePolicyStatement_resourceArn = Lens.lens (\DeleteResourcePolicyStatement' {resourceArn} -> resourceArn) (\s@DeleteResourcePolicyStatement' {} a -> s {resourceArn = a} :: DeleteResourcePolicyStatement)

-- | The name of the statement (SID) to delete from the policy.
deleteResourcePolicyStatement_statementId :: Lens.Lens' DeleteResourcePolicyStatement Prelude.Text
deleteResourcePolicyStatement_statementId = Lens.lens (\DeleteResourcePolicyStatement' {statementId} -> statementId) (\s@DeleteResourcePolicyStatement' {} a -> s {statementId = a} :: DeleteResourcePolicyStatement)

instance
  Core.AWSRequest
    DeleteResourcePolicyStatement
  where
  type
    AWSResponse DeleteResourcePolicyStatement =
      DeleteResourcePolicyStatementResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteResourcePolicyStatementResponse'
            Prelude.<$> (x Data..?> "revisionId")
            Prelude.<*> (x Data..?> "resourceArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteResourcePolicyStatement
  where
  hashWithSalt _salt DeleteResourcePolicyStatement' {..} =
    _salt `Prelude.hashWithSalt` expectedRevisionId
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` statementId

instance Prelude.NFData DeleteResourcePolicyStatement where
  rnf DeleteResourcePolicyStatement' {..} =
    Prelude.rnf expectedRevisionId
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf statementId

instance Data.ToHeaders DeleteResourcePolicyStatement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteResourcePolicyStatement where
  toPath DeleteResourcePolicyStatement' {..} =
    Prelude.mconcat
      [ "/policy/",
        Data.toBS resourceArn,
        "/statements/",
        Data.toBS statementId,
        "/"
      ]

instance Data.ToQuery DeleteResourcePolicyStatement where
  toQuery DeleteResourcePolicyStatement' {..} =
    Prelude.mconcat
      ["expectedRevisionId" Data.=: expectedRevisionId]

-- | /See:/ 'newDeleteResourcePolicyStatementResponse' smart constructor.
data DeleteResourcePolicyStatementResponse = DeleteResourcePolicyStatementResponse'
  { -- | The current revision of the resource policy. Use the revision ID to make
    -- sure that you are updating the most current version of a resource policy
    -- when you add a policy statement to a resource, delete a resource, or
    -- update a resource.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the bot or bot alias that the resource
    -- policy statement was removed from.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourcePolicyStatementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revisionId', 'deleteResourcePolicyStatementResponse_revisionId' - The current revision of the resource policy. Use the revision ID to make
-- sure that you are updating the most current version of a resource policy
-- when you add a policy statement to a resource, delete a resource, or
-- update a resource.
--
-- 'resourceArn', 'deleteResourcePolicyStatementResponse_resourceArn' - The Amazon Resource Name (ARN) of the bot or bot alias that the resource
-- policy statement was removed from.
--
-- 'httpStatus', 'deleteResourcePolicyStatementResponse_httpStatus' - The response's http status code.
newDeleteResourcePolicyStatementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteResourcePolicyStatementResponse
newDeleteResourcePolicyStatementResponse pHttpStatus_ =
  DeleteResourcePolicyStatementResponse'
    { revisionId =
        Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current revision of the resource policy. Use the revision ID to make
-- sure that you are updating the most current version of a resource policy
-- when you add a policy statement to a resource, delete a resource, or
-- update a resource.
deleteResourcePolicyStatementResponse_revisionId :: Lens.Lens' DeleteResourcePolicyStatementResponse (Prelude.Maybe Prelude.Text)
deleteResourcePolicyStatementResponse_revisionId = Lens.lens (\DeleteResourcePolicyStatementResponse' {revisionId} -> revisionId) (\s@DeleteResourcePolicyStatementResponse' {} a -> s {revisionId = a} :: DeleteResourcePolicyStatementResponse)

-- | The Amazon Resource Name (ARN) of the bot or bot alias that the resource
-- policy statement was removed from.
deleteResourcePolicyStatementResponse_resourceArn :: Lens.Lens' DeleteResourcePolicyStatementResponse (Prelude.Maybe Prelude.Text)
deleteResourcePolicyStatementResponse_resourceArn = Lens.lens (\DeleteResourcePolicyStatementResponse' {resourceArn} -> resourceArn) (\s@DeleteResourcePolicyStatementResponse' {} a -> s {resourceArn = a} :: DeleteResourcePolicyStatementResponse)

-- | The response's http status code.
deleteResourcePolicyStatementResponse_httpStatus :: Lens.Lens' DeleteResourcePolicyStatementResponse Prelude.Int
deleteResourcePolicyStatementResponse_httpStatus = Lens.lens (\DeleteResourcePolicyStatementResponse' {httpStatus} -> httpStatus) (\s@DeleteResourcePolicyStatementResponse' {} a -> s {httpStatus = a} :: DeleteResourcePolicyStatementResponse)

instance
  Prelude.NFData
    DeleteResourcePolicyStatementResponse
  where
  rnf DeleteResourcePolicyStatementResponse' {..} =
    Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf httpStatus
