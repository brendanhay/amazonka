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
-- Module      : Amazonka.ConnectCases.UpdateCase
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the values of fields on a case. Fields to be updated are
-- received as an array of id\/value pairs identical to the @CreateCase@
-- input .
--
-- If the action is successful, the service sends back an HTTP 200 response
-- with an empty HTTP body.
module Amazonka.ConnectCases.UpdateCase
  ( -- * Creating a Request
    UpdateCase (..),
    newUpdateCase,

    -- * Request Lenses
    updateCase_caseId,
    updateCase_domainId,
    updateCase_fields,

    -- * Destructuring the Response
    UpdateCaseResponse (..),
    newUpdateCaseResponse,

    -- * Response Lenses
    updateCaseResponse_httpStatus,
  )
where

import Amazonka.ConnectCases.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateCase' smart constructor.
data UpdateCase = UpdateCase'
  { -- | A unique identifier of the case.
    caseId :: Prelude.Text,
    -- | The unique identifier of the Cases domain.
    domainId :: Prelude.Text,
    -- | An array of objects with @fieldId@ (matching ListFields\/DescribeField)
    -- and value union data, structured identical to @CreateCase@.
    fields :: [FieldValue]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'caseId', 'updateCase_caseId' - A unique identifier of the case.
--
-- 'domainId', 'updateCase_domainId' - The unique identifier of the Cases domain.
--
-- 'fields', 'updateCase_fields' - An array of objects with @fieldId@ (matching ListFields\/DescribeField)
-- and value union data, structured identical to @CreateCase@.
newUpdateCase ::
  -- | 'caseId'
  Prelude.Text ->
  -- | 'domainId'
  Prelude.Text ->
  UpdateCase
newUpdateCase pCaseId_ pDomainId_ =
  UpdateCase'
    { caseId = pCaseId_,
      domainId = pDomainId_,
      fields = Prelude.mempty
    }

-- | A unique identifier of the case.
updateCase_caseId :: Lens.Lens' UpdateCase Prelude.Text
updateCase_caseId = Lens.lens (\UpdateCase' {caseId} -> caseId) (\s@UpdateCase' {} a -> s {caseId = a} :: UpdateCase)

-- | The unique identifier of the Cases domain.
updateCase_domainId :: Lens.Lens' UpdateCase Prelude.Text
updateCase_domainId = Lens.lens (\UpdateCase' {domainId} -> domainId) (\s@UpdateCase' {} a -> s {domainId = a} :: UpdateCase)

-- | An array of objects with @fieldId@ (matching ListFields\/DescribeField)
-- and value union data, structured identical to @CreateCase@.
updateCase_fields :: Lens.Lens' UpdateCase [FieldValue]
updateCase_fields = Lens.lens (\UpdateCase' {fields} -> fields) (\s@UpdateCase' {} a -> s {fields = a} :: UpdateCase) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateCase where
  type AWSResponse UpdateCase = UpdateCaseResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateCaseResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateCase where
  hashWithSalt _salt UpdateCase' {..} =
    _salt `Prelude.hashWithSalt` caseId
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` fields

instance Prelude.NFData UpdateCase where
  rnf UpdateCase' {..} =
    Prelude.rnf caseId
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf fields

instance Core.ToHeaders UpdateCase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateCase where
  toJSON UpdateCase' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("fields" Core..= fields)]
      )

instance Core.ToPath UpdateCase where
  toPath UpdateCase' {..} =
    Prelude.mconcat
      [ "/domains/",
        Core.toBS domainId,
        "/cases/",
        Core.toBS caseId
      ]

instance Core.ToQuery UpdateCase where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCaseResponse' smart constructor.
data UpdateCaseResponse = UpdateCaseResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateCaseResponse_httpStatus' - The response's http status code.
newUpdateCaseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateCaseResponse
newUpdateCaseResponse pHttpStatus_ =
  UpdateCaseResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateCaseResponse_httpStatus :: Lens.Lens' UpdateCaseResponse Prelude.Int
updateCaseResponse_httpStatus = Lens.lens (\UpdateCaseResponse' {httpStatus} -> httpStatus) (\s@UpdateCaseResponse' {} a -> s {httpStatus = a} :: UpdateCaseResponse)

instance Prelude.NFData UpdateCaseResponse where
  rnf UpdateCaseResponse' {..} = Prelude.rnf httpStatus
