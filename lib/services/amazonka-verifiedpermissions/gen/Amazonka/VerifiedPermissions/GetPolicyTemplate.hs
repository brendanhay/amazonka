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
-- Module      : Amazonka.VerifiedPermissions.GetPolicyTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve the details for the specified policy template in the specified
-- policy store.
module Amazonka.VerifiedPermissions.GetPolicyTemplate
  ( -- * Creating a Request
    GetPolicyTemplate (..),
    newGetPolicyTemplate,

    -- * Request Lenses
    getPolicyTemplate_policyStoreId,
    getPolicyTemplate_policyTemplateId,

    -- * Destructuring the Response
    GetPolicyTemplateResponse (..),
    newGetPolicyTemplateResponse,

    -- * Response Lenses
    getPolicyTemplateResponse_description,
    getPolicyTemplateResponse_httpStatus,
    getPolicyTemplateResponse_policyStoreId,
    getPolicyTemplateResponse_policyTemplateId,
    getPolicyTemplateResponse_statement,
    getPolicyTemplateResponse_createdDate,
    getPolicyTemplateResponse_lastUpdatedDate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VerifiedPermissions.Types

-- | /See:/ 'newGetPolicyTemplate' smart constructor.
data GetPolicyTemplate = GetPolicyTemplate'
  { -- | Specifies the ID of the policy store that contains the policy template
    -- that you want information about.
    policyStoreId :: Prelude.Text,
    -- | Specifies the ID of the policy template that you want information about.
    policyTemplateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPolicyTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyStoreId', 'getPolicyTemplate_policyStoreId' - Specifies the ID of the policy store that contains the policy template
-- that you want information about.
--
-- 'policyTemplateId', 'getPolicyTemplate_policyTemplateId' - Specifies the ID of the policy template that you want information about.
newGetPolicyTemplate ::
  -- | 'policyStoreId'
  Prelude.Text ->
  -- | 'policyTemplateId'
  Prelude.Text ->
  GetPolicyTemplate
newGetPolicyTemplate
  pPolicyStoreId_
  pPolicyTemplateId_ =
    GetPolicyTemplate'
      { policyStoreId = pPolicyStoreId_,
        policyTemplateId = pPolicyTemplateId_
      }

-- | Specifies the ID of the policy store that contains the policy template
-- that you want information about.
getPolicyTemplate_policyStoreId :: Lens.Lens' GetPolicyTemplate Prelude.Text
getPolicyTemplate_policyStoreId = Lens.lens (\GetPolicyTemplate' {policyStoreId} -> policyStoreId) (\s@GetPolicyTemplate' {} a -> s {policyStoreId = a} :: GetPolicyTemplate)

-- | Specifies the ID of the policy template that you want information about.
getPolicyTemplate_policyTemplateId :: Lens.Lens' GetPolicyTemplate Prelude.Text
getPolicyTemplate_policyTemplateId = Lens.lens (\GetPolicyTemplate' {policyTemplateId} -> policyTemplateId) (\s@GetPolicyTemplate' {} a -> s {policyTemplateId = a} :: GetPolicyTemplate)

instance Core.AWSRequest GetPolicyTemplate where
  type
    AWSResponse GetPolicyTemplate =
      GetPolicyTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPolicyTemplateResponse'
            Prelude.<$> (x Data..?> "description")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "policyStoreId")
            Prelude.<*> (x Data..:> "policyTemplateId")
            Prelude.<*> (x Data..:> "statement")
            Prelude.<*> (x Data..:> "createdDate")
            Prelude.<*> (x Data..:> "lastUpdatedDate")
      )

instance Prelude.Hashable GetPolicyTemplate where
  hashWithSalt _salt GetPolicyTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` policyStoreId
      `Prelude.hashWithSalt` policyTemplateId

instance Prelude.NFData GetPolicyTemplate where
  rnf GetPolicyTemplate' {..} =
    Prelude.rnf policyStoreId
      `Prelude.seq` Prelude.rnf policyTemplateId

instance Data.ToHeaders GetPolicyTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "VerifiedPermissions.GetPolicyTemplate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetPolicyTemplate where
  toJSON GetPolicyTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("policyStoreId" Data..= policyStoreId),
            Prelude.Just
              ("policyTemplateId" Data..= policyTemplateId)
          ]
      )

instance Data.ToPath GetPolicyTemplate where
  toPath = Prelude.const "/"

instance Data.ToQuery GetPolicyTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPolicyTemplateResponse' smart constructor.
data GetPolicyTemplateResponse = GetPolicyTemplateResponse'
  { -- | The description of the policy template.
    description :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the policy store that contains the policy template.
    policyStoreId :: Prelude.Text,
    -- | The ID of the policy template.
    policyTemplateId :: Prelude.Text,
    -- | The content of the body of the policy template written in the Cedar
    -- policy language.
    statement :: Prelude.Text,
    -- | The date and time that the policy template was originally created.
    createdDate :: Data.ISO8601,
    -- | The date and time that the policy template was most recently updated.
    lastUpdatedDate :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPolicyTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'getPolicyTemplateResponse_description' - The description of the policy template.
--
-- 'httpStatus', 'getPolicyTemplateResponse_httpStatus' - The response's http status code.
--
-- 'policyStoreId', 'getPolicyTemplateResponse_policyStoreId' - The ID of the policy store that contains the policy template.
--
-- 'policyTemplateId', 'getPolicyTemplateResponse_policyTemplateId' - The ID of the policy template.
--
-- 'statement', 'getPolicyTemplateResponse_statement' - The content of the body of the policy template written in the Cedar
-- policy language.
--
-- 'createdDate', 'getPolicyTemplateResponse_createdDate' - The date and time that the policy template was originally created.
--
-- 'lastUpdatedDate', 'getPolicyTemplateResponse_lastUpdatedDate' - The date and time that the policy template was most recently updated.
newGetPolicyTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'policyStoreId'
  Prelude.Text ->
  -- | 'policyTemplateId'
  Prelude.Text ->
  -- | 'statement'
  Prelude.Text ->
  -- | 'createdDate'
  Prelude.UTCTime ->
  -- | 'lastUpdatedDate'
  Prelude.UTCTime ->
  GetPolicyTemplateResponse
newGetPolicyTemplateResponse
  pHttpStatus_
  pPolicyStoreId_
  pPolicyTemplateId_
  pStatement_
  pCreatedDate_
  pLastUpdatedDate_ =
    GetPolicyTemplateResponse'
      { description =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        policyStoreId = pPolicyStoreId_,
        policyTemplateId = pPolicyTemplateId_,
        statement = pStatement_,
        createdDate = Data._Time Lens.# pCreatedDate_,
        lastUpdatedDate =
          Data._Time Lens.# pLastUpdatedDate_
      }

-- | The description of the policy template.
getPolicyTemplateResponse_description :: Lens.Lens' GetPolicyTemplateResponse (Prelude.Maybe Prelude.Text)
getPolicyTemplateResponse_description = Lens.lens (\GetPolicyTemplateResponse' {description} -> description) (\s@GetPolicyTemplateResponse' {} a -> s {description = a} :: GetPolicyTemplateResponse)

-- | The response's http status code.
getPolicyTemplateResponse_httpStatus :: Lens.Lens' GetPolicyTemplateResponse Prelude.Int
getPolicyTemplateResponse_httpStatus = Lens.lens (\GetPolicyTemplateResponse' {httpStatus} -> httpStatus) (\s@GetPolicyTemplateResponse' {} a -> s {httpStatus = a} :: GetPolicyTemplateResponse)

-- | The ID of the policy store that contains the policy template.
getPolicyTemplateResponse_policyStoreId :: Lens.Lens' GetPolicyTemplateResponse Prelude.Text
getPolicyTemplateResponse_policyStoreId = Lens.lens (\GetPolicyTemplateResponse' {policyStoreId} -> policyStoreId) (\s@GetPolicyTemplateResponse' {} a -> s {policyStoreId = a} :: GetPolicyTemplateResponse)

-- | The ID of the policy template.
getPolicyTemplateResponse_policyTemplateId :: Lens.Lens' GetPolicyTemplateResponse Prelude.Text
getPolicyTemplateResponse_policyTemplateId = Lens.lens (\GetPolicyTemplateResponse' {policyTemplateId} -> policyTemplateId) (\s@GetPolicyTemplateResponse' {} a -> s {policyTemplateId = a} :: GetPolicyTemplateResponse)

-- | The content of the body of the policy template written in the Cedar
-- policy language.
getPolicyTemplateResponse_statement :: Lens.Lens' GetPolicyTemplateResponse Prelude.Text
getPolicyTemplateResponse_statement = Lens.lens (\GetPolicyTemplateResponse' {statement} -> statement) (\s@GetPolicyTemplateResponse' {} a -> s {statement = a} :: GetPolicyTemplateResponse)

-- | The date and time that the policy template was originally created.
getPolicyTemplateResponse_createdDate :: Lens.Lens' GetPolicyTemplateResponse Prelude.UTCTime
getPolicyTemplateResponse_createdDate = Lens.lens (\GetPolicyTemplateResponse' {createdDate} -> createdDate) (\s@GetPolicyTemplateResponse' {} a -> s {createdDate = a} :: GetPolicyTemplateResponse) Prelude.. Data._Time

-- | The date and time that the policy template was most recently updated.
getPolicyTemplateResponse_lastUpdatedDate :: Lens.Lens' GetPolicyTemplateResponse Prelude.UTCTime
getPolicyTemplateResponse_lastUpdatedDate = Lens.lens (\GetPolicyTemplateResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@GetPolicyTemplateResponse' {} a -> s {lastUpdatedDate = a} :: GetPolicyTemplateResponse) Prelude.. Data._Time

instance Prelude.NFData GetPolicyTemplateResponse where
  rnf GetPolicyTemplateResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf policyStoreId
      `Prelude.seq` Prelude.rnf policyTemplateId
      `Prelude.seq` Prelude.rnf statement
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf lastUpdatedDate
