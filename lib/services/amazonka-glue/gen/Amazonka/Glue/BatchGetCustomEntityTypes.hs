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
-- Module      : Amazonka.Glue.BatchGetCustomEntityTypes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details for the custom patterns specified by a list of
-- names.
module Amazonka.Glue.BatchGetCustomEntityTypes
  ( -- * Creating a Request
    BatchGetCustomEntityTypes (..),
    newBatchGetCustomEntityTypes,

    -- * Request Lenses
    batchGetCustomEntityTypes_names,

    -- * Destructuring the Response
    BatchGetCustomEntityTypesResponse (..),
    newBatchGetCustomEntityTypesResponse,

    -- * Response Lenses
    batchGetCustomEntityTypesResponse_customEntityTypes,
    batchGetCustomEntityTypesResponse_customEntityTypesNotFound,
    batchGetCustomEntityTypesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetCustomEntityTypes' smart constructor.
data BatchGetCustomEntityTypes = BatchGetCustomEntityTypes'
  { -- | A list of names of the custom patterns that you want to retrieve.
    names :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetCustomEntityTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'names', 'batchGetCustomEntityTypes_names' - A list of names of the custom patterns that you want to retrieve.
newBatchGetCustomEntityTypes ::
  -- | 'names'
  Prelude.NonEmpty Prelude.Text ->
  BatchGetCustomEntityTypes
newBatchGetCustomEntityTypes pNames_ =
  BatchGetCustomEntityTypes'
    { names =
        Lens.coerced Lens.# pNames_
    }

-- | A list of names of the custom patterns that you want to retrieve.
batchGetCustomEntityTypes_names :: Lens.Lens' BatchGetCustomEntityTypes (Prelude.NonEmpty Prelude.Text)
batchGetCustomEntityTypes_names = Lens.lens (\BatchGetCustomEntityTypes' {names} -> names) (\s@BatchGetCustomEntityTypes' {} a -> s {names = a} :: BatchGetCustomEntityTypes) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetCustomEntityTypes where
  type
    AWSResponse BatchGetCustomEntityTypes =
      BatchGetCustomEntityTypesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetCustomEntityTypesResponse'
            Prelude.<$> ( x
                            Data..?> "CustomEntityTypes"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "CustomEntityTypesNotFound")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetCustomEntityTypes where
  hashWithSalt _salt BatchGetCustomEntityTypes' {..} =
    _salt `Prelude.hashWithSalt` names

instance Prelude.NFData BatchGetCustomEntityTypes where
  rnf BatchGetCustomEntityTypes' {..} =
    Prelude.rnf names

instance Data.ToHeaders BatchGetCustomEntityTypes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.BatchGetCustomEntityTypes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetCustomEntityTypes where
  toJSON BatchGetCustomEntityTypes' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Names" Data..= names)]
      )

instance Data.ToPath BatchGetCustomEntityTypes where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchGetCustomEntityTypes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetCustomEntityTypesResponse' smart constructor.
data BatchGetCustomEntityTypesResponse = BatchGetCustomEntityTypesResponse'
  { -- | A list of @CustomEntityType@ objects representing the custom patterns
    -- that have been created.
    customEntityTypes :: Prelude.Maybe [CustomEntityType],
    -- | A list of the names of custom patterns that were not found.
    customEntityTypesNotFound :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetCustomEntityTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customEntityTypes', 'batchGetCustomEntityTypesResponse_customEntityTypes' - A list of @CustomEntityType@ objects representing the custom patterns
-- that have been created.
--
-- 'customEntityTypesNotFound', 'batchGetCustomEntityTypesResponse_customEntityTypesNotFound' - A list of the names of custom patterns that were not found.
--
-- 'httpStatus', 'batchGetCustomEntityTypesResponse_httpStatus' - The response's http status code.
newBatchGetCustomEntityTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetCustomEntityTypesResponse
newBatchGetCustomEntityTypesResponse pHttpStatus_ =
  BatchGetCustomEntityTypesResponse'
    { customEntityTypes =
        Prelude.Nothing,
      customEntityTypesNotFound =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @CustomEntityType@ objects representing the custom patterns
-- that have been created.
batchGetCustomEntityTypesResponse_customEntityTypes :: Lens.Lens' BatchGetCustomEntityTypesResponse (Prelude.Maybe [CustomEntityType])
batchGetCustomEntityTypesResponse_customEntityTypes = Lens.lens (\BatchGetCustomEntityTypesResponse' {customEntityTypes} -> customEntityTypes) (\s@BatchGetCustomEntityTypesResponse' {} a -> s {customEntityTypes = a} :: BatchGetCustomEntityTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of the names of custom patterns that were not found.
batchGetCustomEntityTypesResponse_customEntityTypesNotFound :: Lens.Lens' BatchGetCustomEntityTypesResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
batchGetCustomEntityTypesResponse_customEntityTypesNotFound = Lens.lens (\BatchGetCustomEntityTypesResponse' {customEntityTypesNotFound} -> customEntityTypesNotFound) (\s@BatchGetCustomEntityTypesResponse' {} a -> s {customEntityTypesNotFound = a} :: BatchGetCustomEntityTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetCustomEntityTypesResponse_httpStatus :: Lens.Lens' BatchGetCustomEntityTypesResponse Prelude.Int
batchGetCustomEntityTypesResponse_httpStatus = Lens.lens (\BatchGetCustomEntityTypesResponse' {httpStatus} -> httpStatus) (\s@BatchGetCustomEntityTypesResponse' {} a -> s {httpStatus = a} :: BatchGetCustomEntityTypesResponse)

instance
  Prelude.NFData
    BatchGetCustomEntityTypesResponse
  where
  rnf BatchGetCustomEntityTypesResponse' {..} =
    Prelude.rnf customEntityTypes
      `Prelude.seq` Prelude.rnf customEntityTypesNotFound
      `Prelude.seq` Prelude.rnf httpStatus
