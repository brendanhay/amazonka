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
-- Module      : Amazonka.ConnectCases.BatchGetField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description for the list of fields in the request
-- parameters.
module Amazonka.ConnectCases.BatchGetField
  ( -- * Creating a Request
    BatchGetField (..),
    newBatchGetField,

    -- * Request Lenses
    batchGetField_domainId,
    batchGetField_fields,

    -- * Destructuring the Response
    BatchGetFieldResponse (..),
    newBatchGetFieldResponse,

    -- * Response Lenses
    batchGetFieldResponse_httpStatus,
    batchGetFieldResponse_errors,
    batchGetFieldResponse_fields,
  )
where

import Amazonka.ConnectCases.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetField' smart constructor.
data BatchGetField = BatchGetField'
  { -- | The unique identifier of the Cases domain.
    domainId :: Prelude.Text,
    -- | A list of unique field identifiers.
    fields :: Prelude.NonEmpty FieldIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainId', 'batchGetField_domainId' - The unique identifier of the Cases domain.
--
-- 'fields', 'batchGetField_fields' - A list of unique field identifiers.
newBatchGetField ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'fields'
  Prelude.NonEmpty FieldIdentifier ->
  BatchGetField
newBatchGetField pDomainId_ pFields_ =
  BatchGetField'
    { domainId = pDomainId_,
      fields = Lens.coerced Lens.# pFields_
    }

-- | The unique identifier of the Cases domain.
batchGetField_domainId :: Lens.Lens' BatchGetField Prelude.Text
batchGetField_domainId = Lens.lens (\BatchGetField' {domainId} -> domainId) (\s@BatchGetField' {} a -> s {domainId = a} :: BatchGetField)

-- | A list of unique field identifiers.
batchGetField_fields :: Lens.Lens' BatchGetField (Prelude.NonEmpty FieldIdentifier)
batchGetField_fields = Lens.lens (\BatchGetField' {fields} -> fields) (\s@BatchGetField' {} a -> s {fields = a} :: BatchGetField) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetField where
  type
    AWSResponse BatchGetField =
      BatchGetFieldResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetFieldResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "errors" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "fields" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable BatchGetField where
  hashWithSalt _salt BatchGetField' {..} =
    _salt
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` fields

instance Prelude.NFData BatchGetField where
  rnf BatchGetField' {..} =
    Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf fields

instance Data.ToHeaders BatchGetField where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetField where
  toJSON BatchGetField' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("fields" Data..= fields)]
      )

instance Data.ToPath BatchGetField where
  toPath BatchGetField' {..} =
    Prelude.mconcat
      ["/domains/", Data.toBS domainId, "/fields-batch"]

instance Data.ToQuery BatchGetField where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetFieldResponse' smart constructor.
data BatchGetFieldResponse = BatchGetFieldResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of field errors.
    errors :: [FieldError],
    -- | A list of detailed field information.
    fields :: [GetFieldResponse]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetFieldResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'batchGetFieldResponse_httpStatus' - The response's http status code.
--
-- 'errors', 'batchGetFieldResponse_errors' - A list of field errors.
--
-- 'fields', 'batchGetFieldResponse_fields' - A list of detailed field information.
newBatchGetFieldResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetFieldResponse
newBatchGetFieldResponse pHttpStatus_ =
  BatchGetFieldResponse'
    { httpStatus = pHttpStatus_,
      errors = Prelude.mempty,
      fields = Prelude.mempty
    }

-- | The response's http status code.
batchGetFieldResponse_httpStatus :: Lens.Lens' BatchGetFieldResponse Prelude.Int
batchGetFieldResponse_httpStatus = Lens.lens (\BatchGetFieldResponse' {httpStatus} -> httpStatus) (\s@BatchGetFieldResponse' {} a -> s {httpStatus = a} :: BatchGetFieldResponse)

-- | A list of field errors.
batchGetFieldResponse_errors :: Lens.Lens' BatchGetFieldResponse [FieldError]
batchGetFieldResponse_errors = Lens.lens (\BatchGetFieldResponse' {errors} -> errors) (\s@BatchGetFieldResponse' {} a -> s {errors = a} :: BatchGetFieldResponse) Prelude.. Lens.coerced

-- | A list of detailed field information.
batchGetFieldResponse_fields :: Lens.Lens' BatchGetFieldResponse [GetFieldResponse]
batchGetFieldResponse_fields = Lens.lens (\BatchGetFieldResponse' {fields} -> fields) (\s@BatchGetFieldResponse' {} a -> s {fields = a} :: BatchGetFieldResponse) Prelude.. Lens.coerced

instance Prelude.NFData BatchGetFieldResponse where
  rnf BatchGetFieldResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf fields
