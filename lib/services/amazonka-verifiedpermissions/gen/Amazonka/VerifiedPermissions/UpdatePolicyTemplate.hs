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
-- Module      : Amazonka.VerifiedPermissions.UpdatePolicyTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified policy template. You can update only the
-- description and the some elements of the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_UpdatePolicyTemplate.html#amazonverifiedpermissions-UpdatePolicyTemplate-request-policyBody policyBody>.
--
-- Changes you make to the policy template content are immediately
-- reflected in authorization decisions that involve all template-linked
-- policies instantiated from this template.
module Amazonka.VerifiedPermissions.UpdatePolicyTemplate
  ( -- * Creating a Request
    UpdatePolicyTemplate (..),
    newUpdatePolicyTemplate,

    -- * Request Lenses
    updatePolicyTemplate_description,
    updatePolicyTemplate_policyStoreId,
    updatePolicyTemplate_policyTemplateId,
    updatePolicyTemplate_statement,

    -- * Destructuring the Response
    UpdatePolicyTemplateResponse (..),
    newUpdatePolicyTemplateResponse,

    -- * Response Lenses
    updatePolicyTemplateResponse_httpStatus,
    updatePolicyTemplateResponse_policyStoreId,
    updatePolicyTemplateResponse_policyTemplateId,
    updatePolicyTemplateResponse_createdDate,
    updatePolicyTemplateResponse_lastUpdatedDate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VerifiedPermissions.Types

-- | /See:/ 'newUpdatePolicyTemplate' smart constructor.
data UpdatePolicyTemplate = UpdatePolicyTemplate'
  { -- | Specifies a new description to apply to the policy template.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies the ID of the policy store that contains the policy template
    -- that you want to update.
    policyStoreId :: Prelude.Text,
    -- | Specifies the ID of the policy template that you want to update.
    policyTemplateId :: Prelude.Text,
    -- | Specifies new statement content written in Cedar policy language to
    -- replace the current body of the policy template.
    --
    -- You can change only the following elements of the policy body:
    --
    -- -   The @action@ referenced by the policy template.
    --
    -- -   Any conditional clauses, such as @when@ or @unless@ clauses.
    --
    -- You __can\'t__ change the following elements:
    --
    -- -   The effect (@permit@ or @forbid@) of the policy template.
    --
    -- -   The @principal@ referenced by the policy template.
    --
    -- -   The @resource@ referenced by the policy template.
    statement :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePolicyTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updatePolicyTemplate_description' - Specifies a new description to apply to the policy template.
--
-- 'policyStoreId', 'updatePolicyTemplate_policyStoreId' - Specifies the ID of the policy store that contains the policy template
-- that you want to update.
--
-- 'policyTemplateId', 'updatePolicyTemplate_policyTemplateId' - Specifies the ID of the policy template that you want to update.
--
-- 'statement', 'updatePolicyTemplate_statement' - Specifies new statement content written in Cedar policy language to
-- replace the current body of the policy template.
--
-- You can change only the following elements of the policy body:
--
-- -   The @action@ referenced by the policy template.
--
-- -   Any conditional clauses, such as @when@ or @unless@ clauses.
--
-- You __can\'t__ change the following elements:
--
-- -   The effect (@permit@ or @forbid@) of the policy template.
--
-- -   The @principal@ referenced by the policy template.
--
-- -   The @resource@ referenced by the policy template.
newUpdatePolicyTemplate ::
  -- | 'policyStoreId'
  Prelude.Text ->
  -- | 'policyTemplateId'
  Prelude.Text ->
  -- | 'statement'
  Prelude.Text ->
  UpdatePolicyTemplate
newUpdatePolicyTemplate
  pPolicyStoreId_
  pPolicyTemplateId_
  pStatement_ =
    UpdatePolicyTemplate'
      { description =
          Prelude.Nothing,
        policyStoreId = pPolicyStoreId_,
        policyTemplateId = pPolicyTemplateId_,
        statement = pStatement_
      }

-- | Specifies a new description to apply to the policy template.
updatePolicyTemplate_description :: Lens.Lens' UpdatePolicyTemplate (Prelude.Maybe Prelude.Text)
updatePolicyTemplate_description = Lens.lens (\UpdatePolicyTemplate' {description} -> description) (\s@UpdatePolicyTemplate' {} a -> s {description = a} :: UpdatePolicyTemplate)

-- | Specifies the ID of the policy store that contains the policy template
-- that you want to update.
updatePolicyTemplate_policyStoreId :: Lens.Lens' UpdatePolicyTemplate Prelude.Text
updatePolicyTemplate_policyStoreId = Lens.lens (\UpdatePolicyTemplate' {policyStoreId} -> policyStoreId) (\s@UpdatePolicyTemplate' {} a -> s {policyStoreId = a} :: UpdatePolicyTemplate)

-- | Specifies the ID of the policy template that you want to update.
updatePolicyTemplate_policyTemplateId :: Lens.Lens' UpdatePolicyTemplate Prelude.Text
updatePolicyTemplate_policyTemplateId = Lens.lens (\UpdatePolicyTemplate' {policyTemplateId} -> policyTemplateId) (\s@UpdatePolicyTemplate' {} a -> s {policyTemplateId = a} :: UpdatePolicyTemplate)

-- | Specifies new statement content written in Cedar policy language to
-- replace the current body of the policy template.
--
-- You can change only the following elements of the policy body:
--
-- -   The @action@ referenced by the policy template.
--
-- -   Any conditional clauses, such as @when@ or @unless@ clauses.
--
-- You __can\'t__ change the following elements:
--
-- -   The effect (@permit@ or @forbid@) of the policy template.
--
-- -   The @principal@ referenced by the policy template.
--
-- -   The @resource@ referenced by the policy template.
updatePolicyTemplate_statement :: Lens.Lens' UpdatePolicyTemplate Prelude.Text
updatePolicyTemplate_statement = Lens.lens (\UpdatePolicyTemplate' {statement} -> statement) (\s@UpdatePolicyTemplate' {} a -> s {statement = a} :: UpdatePolicyTemplate)

instance Core.AWSRequest UpdatePolicyTemplate where
  type
    AWSResponse UpdatePolicyTemplate =
      UpdatePolicyTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePolicyTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "policyStoreId")
            Prelude.<*> (x Data..:> "policyTemplateId")
            Prelude.<*> (x Data..:> "createdDate")
            Prelude.<*> (x Data..:> "lastUpdatedDate")
      )

instance Prelude.Hashable UpdatePolicyTemplate where
  hashWithSalt _salt UpdatePolicyTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` policyStoreId
      `Prelude.hashWithSalt` policyTemplateId
      `Prelude.hashWithSalt` statement

instance Prelude.NFData UpdatePolicyTemplate where
  rnf UpdatePolicyTemplate' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf policyStoreId
      `Prelude.seq` Prelude.rnf policyTemplateId
      `Prelude.seq` Prelude.rnf statement

instance Data.ToHeaders UpdatePolicyTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "VerifiedPermissions.UpdatePolicyTemplate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdatePolicyTemplate where
  toJSON UpdatePolicyTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            Prelude.Just ("policyStoreId" Data..= policyStoreId),
            Prelude.Just
              ("policyTemplateId" Data..= policyTemplateId),
            Prelude.Just ("statement" Data..= statement)
          ]
      )

instance Data.ToPath UpdatePolicyTemplate where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdatePolicyTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePolicyTemplateResponse' smart constructor.
data UpdatePolicyTemplateResponse = UpdatePolicyTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the policy store that contains the updated policy template.
    policyStoreId :: Prelude.Text,
    -- | The ID of the updated policy template.
    policyTemplateId :: Prelude.Text,
    -- | The date and time that the policy template was originally created.
    createdDate :: Data.ISO8601,
    -- | The date and time that the policy template was most recently updated.
    lastUpdatedDate :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePolicyTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updatePolicyTemplateResponse_httpStatus' - The response's http status code.
--
-- 'policyStoreId', 'updatePolicyTemplateResponse_policyStoreId' - The ID of the policy store that contains the updated policy template.
--
-- 'policyTemplateId', 'updatePolicyTemplateResponse_policyTemplateId' - The ID of the updated policy template.
--
-- 'createdDate', 'updatePolicyTemplateResponse_createdDate' - The date and time that the policy template was originally created.
--
-- 'lastUpdatedDate', 'updatePolicyTemplateResponse_lastUpdatedDate' - The date and time that the policy template was most recently updated.
newUpdatePolicyTemplateResponse ::
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
  UpdatePolicyTemplateResponse
newUpdatePolicyTemplateResponse
  pHttpStatus_
  pPolicyStoreId_
  pPolicyTemplateId_
  pCreatedDate_
  pLastUpdatedDate_ =
    UpdatePolicyTemplateResponse'
      { httpStatus =
          pHttpStatus_,
        policyStoreId = pPolicyStoreId_,
        policyTemplateId = pPolicyTemplateId_,
        createdDate = Data._Time Lens.# pCreatedDate_,
        lastUpdatedDate =
          Data._Time Lens.# pLastUpdatedDate_
      }

-- | The response's http status code.
updatePolicyTemplateResponse_httpStatus :: Lens.Lens' UpdatePolicyTemplateResponse Prelude.Int
updatePolicyTemplateResponse_httpStatus = Lens.lens (\UpdatePolicyTemplateResponse' {httpStatus} -> httpStatus) (\s@UpdatePolicyTemplateResponse' {} a -> s {httpStatus = a} :: UpdatePolicyTemplateResponse)

-- | The ID of the policy store that contains the updated policy template.
updatePolicyTemplateResponse_policyStoreId :: Lens.Lens' UpdatePolicyTemplateResponse Prelude.Text
updatePolicyTemplateResponse_policyStoreId = Lens.lens (\UpdatePolicyTemplateResponse' {policyStoreId} -> policyStoreId) (\s@UpdatePolicyTemplateResponse' {} a -> s {policyStoreId = a} :: UpdatePolicyTemplateResponse)

-- | The ID of the updated policy template.
updatePolicyTemplateResponse_policyTemplateId :: Lens.Lens' UpdatePolicyTemplateResponse Prelude.Text
updatePolicyTemplateResponse_policyTemplateId = Lens.lens (\UpdatePolicyTemplateResponse' {policyTemplateId} -> policyTemplateId) (\s@UpdatePolicyTemplateResponse' {} a -> s {policyTemplateId = a} :: UpdatePolicyTemplateResponse)

-- | The date and time that the policy template was originally created.
updatePolicyTemplateResponse_createdDate :: Lens.Lens' UpdatePolicyTemplateResponse Prelude.UTCTime
updatePolicyTemplateResponse_createdDate = Lens.lens (\UpdatePolicyTemplateResponse' {createdDate} -> createdDate) (\s@UpdatePolicyTemplateResponse' {} a -> s {createdDate = a} :: UpdatePolicyTemplateResponse) Prelude.. Data._Time

-- | The date and time that the policy template was most recently updated.
updatePolicyTemplateResponse_lastUpdatedDate :: Lens.Lens' UpdatePolicyTemplateResponse Prelude.UTCTime
updatePolicyTemplateResponse_lastUpdatedDate = Lens.lens (\UpdatePolicyTemplateResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@UpdatePolicyTemplateResponse' {} a -> s {lastUpdatedDate = a} :: UpdatePolicyTemplateResponse) Prelude.. Data._Time

instance Prelude.NFData UpdatePolicyTemplateResponse where
  rnf UpdatePolicyTemplateResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf policyStoreId
      `Prelude.seq` Prelude.rnf policyTemplateId
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf lastUpdatedDate
