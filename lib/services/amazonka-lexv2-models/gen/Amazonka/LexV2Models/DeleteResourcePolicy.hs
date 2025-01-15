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
-- Module      : Amazonka.LexV2Models.DeleteResourcePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an existing policy from a bot or bot alias. If the resource
-- doesn\'t have a policy attached, Amazon Lex returns an exception.
module Amazonka.LexV2Models.DeleteResourcePolicy
  ( -- * Creating a Request
    DeleteResourcePolicy (..),
    newDeleteResourcePolicy,

    -- * Request Lenses
    deleteResourcePolicy_expectedRevisionId,
    deleteResourcePolicy_resourceArn,

    -- * Destructuring the Response
    DeleteResourcePolicyResponse (..),
    newDeleteResourcePolicyResponse,

    -- * Response Lenses
    deleteResourcePolicyResponse_resourceArn,
    deleteResourcePolicyResponse_revisionId,
    deleteResourcePolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteResourcePolicy' smart constructor.
data DeleteResourcePolicy = DeleteResourcePolicy'
  { -- | The identifier of the revision to edit. If this ID doesn\'t match the
    -- current revision number, Amazon Lex returns an exception
    --
    -- If you don\'t specify a revision ID, Amazon Lex will delete the current
    -- policy.
    expectedRevisionId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the bot or bot alias that has the
    -- resource policy attached.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourcePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedRevisionId', 'deleteResourcePolicy_expectedRevisionId' - The identifier of the revision to edit. If this ID doesn\'t match the
-- current revision number, Amazon Lex returns an exception
--
-- If you don\'t specify a revision ID, Amazon Lex will delete the current
-- policy.
--
-- 'resourceArn', 'deleteResourcePolicy_resourceArn' - The Amazon Resource Name (ARN) of the bot or bot alias that has the
-- resource policy attached.
newDeleteResourcePolicy ::
  -- | 'resourceArn'
  Prelude.Text ->
  DeleteResourcePolicy
newDeleteResourcePolicy pResourceArn_ =
  DeleteResourcePolicy'
    { expectedRevisionId =
        Prelude.Nothing,
      resourceArn = pResourceArn_
    }

-- | The identifier of the revision to edit. If this ID doesn\'t match the
-- current revision number, Amazon Lex returns an exception
--
-- If you don\'t specify a revision ID, Amazon Lex will delete the current
-- policy.
deleteResourcePolicy_expectedRevisionId :: Lens.Lens' DeleteResourcePolicy (Prelude.Maybe Prelude.Text)
deleteResourcePolicy_expectedRevisionId = Lens.lens (\DeleteResourcePolicy' {expectedRevisionId} -> expectedRevisionId) (\s@DeleteResourcePolicy' {} a -> s {expectedRevisionId = a} :: DeleteResourcePolicy)

-- | The Amazon Resource Name (ARN) of the bot or bot alias that has the
-- resource policy attached.
deleteResourcePolicy_resourceArn :: Lens.Lens' DeleteResourcePolicy Prelude.Text
deleteResourcePolicy_resourceArn = Lens.lens (\DeleteResourcePolicy' {resourceArn} -> resourceArn) (\s@DeleteResourcePolicy' {} a -> s {resourceArn = a} :: DeleteResourcePolicy)

instance Core.AWSRequest DeleteResourcePolicy where
  type
    AWSResponse DeleteResourcePolicy =
      DeleteResourcePolicyResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteResourcePolicyResponse'
            Prelude.<$> (x Data..?> "resourceArn")
            Prelude.<*> (x Data..?> "revisionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteResourcePolicy where
  hashWithSalt _salt DeleteResourcePolicy' {..} =
    _salt
      `Prelude.hashWithSalt` expectedRevisionId
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData DeleteResourcePolicy where
  rnf DeleteResourcePolicy' {..} =
    Prelude.rnf expectedRevisionId `Prelude.seq`
      Prelude.rnf resourceArn

instance Data.ToHeaders DeleteResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteResourcePolicy where
  toPath DeleteResourcePolicy' {..} =
    Prelude.mconcat
      ["/policy/", Data.toBS resourceArn, "/"]

instance Data.ToQuery DeleteResourcePolicy where
  toQuery DeleteResourcePolicy' {..} =
    Prelude.mconcat
      ["expectedRevisionId" Data.=: expectedRevisionId]

-- | /See:/ 'newDeleteResourcePolicyResponse' smart constructor.
data DeleteResourcePolicyResponse = DeleteResourcePolicyResponse'
  { -- | The Amazon Resource Name (ARN) of the bot or bot alias that the resource
    -- policy was deleted from.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The current revision of the resource policy. Use the revision ID to make
    -- sure that you are updating the most current version of a resource policy
    -- when you add a policy statement to a resource, delete a resource, or
    -- update a resource.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourcePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'deleteResourcePolicyResponse_resourceArn' - The Amazon Resource Name (ARN) of the bot or bot alias that the resource
-- policy was deleted from.
--
-- 'revisionId', 'deleteResourcePolicyResponse_revisionId' - The current revision of the resource policy. Use the revision ID to make
-- sure that you are updating the most current version of a resource policy
-- when you add a policy statement to a resource, delete a resource, or
-- update a resource.
--
-- 'httpStatus', 'deleteResourcePolicyResponse_httpStatus' - The response's http status code.
newDeleteResourcePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteResourcePolicyResponse
newDeleteResourcePolicyResponse pHttpStatus_ =
  DeleteResourcePolicyResponse'
    { resourceArn =
        Prelude.Nothing,
      revisionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the bot or bot alias that the resource
-- policy was deleted from.
deleteResourcePolicyResponse_resourceArn :: Lens.Lens' DeleteResourcePolicyResponse (Prelude.Maybe Prelude.Text)
deleteResourcePolicyResponse_resourceArn = Lens.lens (\DeleteResourcePolicyResponse' {resourceArn} -> resourceArn) (\s@DeleteResourcePolicyResponse' {} a -> s {resourceArn = a} :: DeleteResourcePolicyResponse)

-- | The current revision of the resource policy. Use the revision ID to make
-- sure that you are updating the most current version of a resource policy
-- when you add a policy statement to a resource, delete a resource, or
-- update a resource.
deleteResourcePolicyResponse_revisionId :: Lens.Lens' DeleteResourcePolicyResponse (Prelude.Maybe Prelude.Text)
deleteResourcePolicyResponse_revisionId = Lens.lens (\DeleteResourcePolicyResponse' {revisionId} -> revisionId) (\s@DeleteResourcePolicyResponse' {} a -> s {revisionId = a} :: DeleteResourcePolicyResponse)

-- | The response's http status code.
deleteResourcePolicyResponse_httpStatus :: Lens.Lens' DeleteResourcePolicyResponse Prelude.Int
deleteResourcePolicyResponse_httpStatus = Lens.lens (\DeleteResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteResourcePolicyResponse' {} a -> s {httpStatus = a} :: DeleteResourcePolicyResponse)

instance Prelude.NFData DeleteResourcePolicyResponse where
  rnf DeleteResourcePolicyResponse' {..} =
    Prelude.rnf resourceArn `Prelude.seq`
      Prelude.rnf revisionId `Prelude.seq`
        Prelude.rnf httpStatus
