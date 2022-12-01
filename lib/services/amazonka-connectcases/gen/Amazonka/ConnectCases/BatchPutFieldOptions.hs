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
-- Module      : Amazonka.ConnectCases.BatchPutFieldOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates and updates a set of field options for a single select field in
-- a Cases domain.
module Amazonka.ConnectCases.BatchPutFieldOptions
  ( -- * Creating a Request
    BatchPutFieldOptions (..),
    newBatchPutFieldOptions,

    -- * Request Lenses
    batchPutFieldOptions_domainId,
    batchPutFieldOptions_fieldId,
    batchPutFieldOptions_options,

    -- * Destructuring the Response
    BatchPutFieldOptionsResponse (..),
    newBatchPutFieldOptionsResponse,

    -- * Response Lenses
    batchPutFieldOptionsResponse_errors,
    batchPutFieldOptionsResponse_httpStatus,
  )
where

import Amazonka.ConnectCases.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchPutFieldOptions' smart constructor.
data BatchPutFieldOptions = BatchPutFieldOptions'
  { -- | The unique identifier of the Cases domain.
    domainId :: Prelude.Text,
    -- | The unique identifier of a field.
    fieldId :: Prelude.Text,
    -- | A list of @FieldOption@ objects.
    options :: [FieldOption]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchPutFieldOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainId', 'batchPutFieldOptions_domainId' - The unique identifier of the Cases domain.
--
-- 'fieldId', 'batchPutFieldOptions_fieldId' - The unique identifier of a field.
--
-- 'options', 'batchPutFieldOptions_options' - A list of @FieldOption@ objects.
newBatchPutFieldOptions ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'fieldId'
  Prelude.Text ->
  BatchPutFieldOptions
newBatchPutFieldOptions pDomainId_ pFieldId_ =
  BatchPutFieldOptions'
    { domainId = pDomainId_,
      fieldId = pFieldId_,
      options = Prelude.mempty
    }

-- | The unique identifier of the Cases domain.
batchPutFieldOptions_domainId :: Lens.Lens' BatchPutFieldOptions Prelude.Text
batchPutFieldOptions_domainId = Lens.lens (\BatchPutFieldOptions' {domainId} -> domainId) (\s@BatchPutFieldOptions' {} a -> s {domainId = a} :: BatchPutFieldOptions)

-- | The unique identifier of a field.
batchPutFieldOptions_fieldId :: Lens.Lens' BatchPutFieldOptions Prelude.Text
batchPutFieldOptions_fieldId = Lens.lens (\BatchPutFieldOptions' {fieldId} -> fieldId) (\s@BatchPutFieldOptions' {} a -> s {fieldId = a} :: BatchPutFieldOptions)

-- | A list of @FieldOption@ objects.
batchPutFieldOptions_options :: Lens.Lens' BatchPutFieldOptions [FieldOption]
batchPutFieldOptions_options = Lens.lens (\BatchPutFieldOptions' {options} -> options) (\s@BatchPutFieldOptions' {} a -> s {options = a} :: BatchPutFieldOptions) Prelude.. Lens.coerced

instance Core.AWSRequest BatchPutFieldOptions where
  type
    AWSResponse BatchPutFieldOptions =
      BatchPutFieldOptionsResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchPutFieldOptionsResponse'
            Prelude.<$> (x Core..?> "errors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchPutFieldOptions where
  hashWithSalt _salt BatchPutFieldOptions' {..} =
    _salt `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` fieldId
      `Prelude.hashWithSalt` options

instance Prelude.NFData BatchPutFieldOptions where
  rnf BatchPutFieldOptions' {..} =
    Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf fieldId
      `Prelude.seq` Prelude.rnf options

instance Core.ToHeaders BatchPutFieldOptions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchPutFieldOptions where
  toJSON BatchPutFieldOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("options" Core..= options)]
      )

instance Core.ToPath BatchPutFieldOptions where
  toPath BatchPutFieldOptions' {..} =
    Prelude.mconcat
      [ "/domains/",
        Core.toBS domainId,
        "/fields/",
        Core.toBS fieldId,
        "/options"
      ]

instance Core.ToQuery BatchPutFieldOptions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchPutFieldOptionsResponse' smart constructor.
data BatchPutFieldOptionsResponse = BatchPutFieldOptionsResponse'
  { -- | A list of field errors.
    errors :: Prelude.Maybe [FieldOptionError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchPutFieldOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errors', 'batchPutFieldOptionsResponse_errors' - A list of field errors.
--
-- 'httpStatus', 'batchPutFieldOptionsResponse_httpStatus' - The response's http status code.
newBatchPutFieldOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchPutFieldOptionsResponse
newBatchPutFieldOptionsResponse pHttpStatus_ =
  BatchPutFieldOptionsResponse'
    { errors =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of field errors.
batchPutFieldOptionsResponse_errors :: Lens.Lens' BatchPutFieldOptionsResponse (Prelude.Maybe [FieldOptionError])
batchPutFieldOptionsResponse_errors = Lens.lens (\BatchPutFieldOptionsResponse' {errors} -> errors) (\s@BatchPutFieldOptionsResponse' {} a -> s {errors = a} :: BatchPutFieldOptionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchPutFieldOptionsResponse_httpStatus :: Lens.Lens' BatchPutFieldOptionsResponse Prelude.Int
batchPutFieldOptionsResponse_httpStatus = Lens.lens (\BatchPutFieldOptionsResponse' {httpStatus} -> httpStatus) (\s@BatchPutFieldOptionsResponse' {} a -> s {httpStatus = a} :: BatchPutFieldOptionsResponse)

instance Prelude.NFData BatchPutFieldOptionsResponse where
  rnf BatchPutFieldOptionsResponse' {..} =
    Prelude.rnf errors
      `Prelude.seq` Prelude.rnf httpStatus
