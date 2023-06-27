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
-- Module      : Amazonka.VerifiedPermissions.CreatePolicyTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a policy template. A template can use placeholders for the
-- principal and resource. A template must be instantiated into a policy by
-- associating it with specific principals and resources to use for the
-- placeholders. That instantiated policy can then be considered in
-- authorization decisions. The instantiated policy works identically to
-- any other policy, except that it is dynamically linked to the template.
-- If the template changes, then any policies that are linked to that
-- template are immediately updated as well.
module Amazonka.VerifiedPermissions.CreatePolicyTemplate
  ( -- * Creating a Request
    CreatePolicyTemplate (..),
    newCreatePolicyTemplate,

    -- * Request Lenses
    createPolicyTemplate_clientToken,
    createPolicyTemplate_description,
    createPolicyTemplate_policyStoreId,
    createPolicyTemplate_statement,

    -- * Destructuring the Response
    CreatePolicyTemplateResponse (..),
    newCreatePolicyTemplateResponse,

    -- * Response Lenses
    createPolicyTemplateResponse_httpStatus,
    createPolicyTemplateResponse_policyStoreId,
    createPolicyTemplateResponse_policyTemplateId,
    createPolicyTemplateResponse_createdDate,
    createPolicyTemplateResponse_lastUpdatedDate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VerifiedPermissions.Types

-- | /See:/ 'newCreatePolicyTemplate' smart constructor.
data CreatePolicyTemplate = CreatePolicyTemplate'
  { -- | Specifies a unique, case-sensitive ID that you provide to ensure the
    -- idempotency of the request. This lets you safely retry the request
    -- without accidentally performing the same operation a second time.
    -- Passing the same value to a later call to an operation requires that you
    -- also pass the same value for all other parameters. We recommend that you
    -- use a
    -- <https://wikipedia.org/wiki/Universally_unique_Id UUID type of value.>.
    --
    -- If you don\'t provide this value, then Amazon Web Services generates a
    -- random one for you.
    --
    -- If you retry the operation with the same @ClientToken@, but with
    -- different parameters, the retry fails with an
    -- @IdempotentParameterMismatch@ error.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies a description for the policy template.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the policy store in which to create the policy template.
    policyStoreId :: Prelude.Text,
    -- | Specifies the content that you want to use for the new policy template,
    -- written in the Cedar policy language.
    statement :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePolicyTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createPolicyTemplate_clientToken' - Specifies a unique, case-sensitive ID that you provide to ensure the
-- idempotency of the request. This lets you safely retry the request
-- without accidentally performing the same operation a second time.
-- Passing the same value to a later call to an operation requires that you
-- also pass the same value for all other parameters. We recommend that you
-- use a
-- <https://wikipedia.org/wiki/Universally_unique_Id UUID type of value.>.
--
-- If you don\'t provide this value, then Amazon Web Services generates a
-- random one for you.
--
-- If you retry the operation with the same @ClientToken@, but with
-- different parameters, the retry fails with an
-- @IdempotentParameterMismatch@ error.
--
-- 'description', 'createPolicyTemplate_description' - Specifies a description for the policy template.
--
-- 'policyStoreId', 'createPolicyTemplate_policyStoreId' - The ID of the policy store in which to create the policy template.
--
-- 'statement', 'createPolicyTemplate_statement' - Specifies the content that you want to use for the new policy template,
-- written in the Cedar policy language.
newCreatePolicyTemplate ::
  -- | 'policyStoreId'
  Prelude.Text ->
  -- | 'statement'
  Prelude.Text ->
  CreatePolicyTemplate
newCreatePolicyTemplate pPolicyStoreId_ pStatement_ =
  CreatePolicyTemplate'
    { clientToken =
        Prelude.Nothing,
      description = Prelude.Nothing,
      policyStoreId = pPolicyStoreId_,
      statement = pStatement_
    }

-- | Specifies a unique, case-sensitive ID that you provide to ensure the
-- idempotency of the request. This lets you safely retry the request
-- without accidentally performing the same operation a second time.
-- Passing the same value to a later call to an operation requires that you
-- also pass the same value for all other parameters. We recommend that you
-- use a
-- <https://wikipedia.org/wiki/Universally_unique_Id UUID type of value.>.
--
-- If you don\'t provide this value, then Amazon Web Services generates a
-- random one for you.
--
-- If you retry the operation with the same @ClientToken@, but with
-- different parameters, the retry fails with an
-- @IdempotentParameterMismatch@ error.
createPolicyTemplate_clientToken :: Lens.Lens' CreatePolicyTemplate (Prelude.Maybe Prelude.Text)
createPolicyTemplate_clientToken = Lens.lens (\CreatePolicyTemplate' {clientToken} -> clientToken) (\s@CreatePolicyTemplate' {} a -> s {clientToken = a} :: CreatePolicyTemplate)

-- | Specifies a description for the policy template.
createPolicyTemplate_description :: Lens.Lens' CreatePolicyTemplate (Prelude.Maybe Prelude.Text)
createPolicyTemplate_description = Lens.lens (\CreatePolicyTemplate' {description} -> description) (\s@CreatePolicyTemplate' {} a -> s {description = a} :: CreatePolicyTemplate)

-- | The ID of the policy store in which to create the policy template.
createPolicyTemplate_policyStoreId :: Lens.Lens' CreatePolicyTemplate Prelude.Text
createPolicyTemplate_policyStoreId = Lens.lens (\CreatePolicyTemplate' {policyStoreId} -> policyStoreId) (\s@CreatePolicyTemplate' {} a -> s {policyStoreId = a} :: CreatePolicyTemplate)

-- | Specifies the content that you want to use for the new policy template,
-- written in the Cedar policy language.
createPolicyTemplate_statement :: Lens.Lens' CreatePolicyTemplate Prelude.Text
createPolicyTemplate_statement = Lens.lens (\CreatePolicyTemplate' {statement} -> statement) (\s@CreatePolicyTemplate' {} a -> s {statement = a} :: CreatePolicyTemplate)

instance Core.AWSRequest CreatePolicyTemplate where
  type
    AWSResponse CreatePolicyTemplate =
      CreatePolicyTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePolicyTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "policyStoreId")
            Prelude.<*> (x Data..:> "policyTemplateId")
            Prelude.<*> (x Data..:> "createdDate")
            Prelude.<*> (x Data..:> "lastUpdatedDate")
      )

instance Prelude.Hashable CreatePolicyTemplate where
  hashWithSalt _salt CreatePolicyTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` policyStoreId
      `Prelude.hashWithSalt` statement

instance Prelude.NFData CreatePolicyTemplate where
  rnf CreatePolicyTemplate' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf policyStoreId
      `Prelude.seq` Prelude.rnf statement

instance Data.ToHeaders CreatePolicyTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "VerifiedPermissions.CreatePolicyTemplate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreatePolicyTemplate where
  toJSON CreatePolicyTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("description" Data..=) Prelude.<$> description,
            Prelude.Just ("policyStoreId" Data..= policyStoreId),
            Prelude.Just ("statement" Data..= statement)
          ]
      )

instance Data.ToPath CreatePolicyTemplate where
  toPath = Prelude.const "/"

instance Data.ToQuery CreatePolicyTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePolicyTemplateResponse' smart constructor.
data CreatePolicyTemplateResponse = CreatePolicyTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the policy store that contains the policy template.
    policyStoreId :: Prelude.Text,
    -- | The unique ID of the new policy template.
    policyTemplateId :: Prelude.Text,
    -- | The date and time the policy template was originally created.
    createdDate :: Data.ISO8601,
    -- | The date and time the policy template was most recently updated.
    lastUpdatedDate :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePolicyTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createPolicyTemplateResponse_httpStatus' - The response's http status code.
--
-- 'policyStoreId', 'createPolicyTemplateResponse_policyStoreId' - The ID of the policy store that contains the policy template.
--
-- 'policyTemplateId', 'createPolicyTemplateResponse_policyTemplateId' - The unique ID of the new policy template.
--
-- 'createdDate', 'createPolicyTemplateResponse_createdDate' - The date and time the policy template was originally created.
--
-- 'lastUpdatedDate', 'createPolicyTemplateResponse_lastUpdatedDate' - The date and time the policy template was most recently updated.
newCreatePolicyTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'policyStoreId'
  Prelude.Text ->
  -- | 'policyTemplateId'
  Prelude.Text ->
  -- | 'createdDate'
  Prelude.UTCTime ->
  -- | 'lastUpdatedDate'
  Prelude.UTCTime ->
  CreatePolicyTemplateResponse
newCreatePolicyTemplateResponse
  pHttpStatus_
  pPolicyStoreId_
  pPolicyTemplateId_
  pCreatedDate_
  pLastUpdatedDate_ =
    CreatePolicyTemplateResponse'
      { httpStatus =
          pHttpStatus_,
        policyStoreId = pPolicyStoreId_,
        policyTemplateId = pPolicyTemplateId_,
        createdDate = Data._Time Lens.# pCreatedDate_,
        lastUpdatedDate =
          Data._Time Lens.# pLastUpdatedDate_
      }

-- | The response's http status code.
createPolicyTemplateResponse_httpStatus :: Lens.Lens' CreatePolicyTemplateResponse Prelude.Int
createPolicyTemplateResponse_httpStatus = Lens.lens (\CreatePolicyTemplateResponse' {httpStatus} -> httpStatus) (\s@CreatePolicyTemplateResponse' {} a -> s {httpStatus = a} :: CreatePolicyTemplateResponse)

-- | The ID of the policy store that contains the policy template.
createPolicyTemplateResponse_policyStoreId :: Lens.Lens' CreatePolicyTemplateResponse Prelude.Text
createPolicyTemplateResponse_policyStoreId = Lens.lens (\CreatePolicyTemplateResponse' {policyStoreId} -> policyStoreId) (\s@CreatePolicyTemplateResponse' {} a -> s {policyStoreId = a} :: CreatePolicyTemplateResponse)

-- | The unique ID of the new policy template.
createPolicyTemplateResponse_policyTemplateId :: Lens.Lens' CreatePolicyTemplateResponse Prelude.Text
createPolicyTemplateResponse_policyTemplateId = Lens.lens (\CreatePolicyTemplateResponse' {policyTemplateId} -> policyTemplateId) (\s@CreatePolicyTemplateResponse' {} a -> s {policyTemplateId = a} :: CreatePolicyTemplateResponse)

-- | The date and time the policy template was originally created.
createPolicyTemplateResponse_createdDate :: Lens.Lens' CreatePolicyTemplateResponse Prelude.UTCTime
createPolicyTemplateResponse_createdDate = Lens.lens (\CreatePolicyTemplateResponse' {createdDate} -> createdDate) (\s@CreatePolicyTemplateResponse' {} a -> s {createdDate = a} :: CreatePolicyTemplateResponse) Prelude.. Data._Time

-- | The date and time the policy template was most recently updated.
createPolicyTemplateResponse_lastUpdatedDate :: Lens.Lens' CreatePolicyTemplateResponse Prelude.UTCTime
createPolicyTemplateResponse_lastUpdatedDate = Lens.lens (\CreatePolicyTemplateResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@CreatePolicyTemplateResponse' {} a -> s {lastUpdatedDate = a} :: CreatePolicyTemplateResponse) Prelude.. Data._Time

instance Prelude.NFData CreatePolicyTemplateResponse where
  rnf CreatePolicyTemplateResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf policyStoreId
      `Prelude.seq` Prelude.rnf policyTemplateId
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf lastUpdatedDate
