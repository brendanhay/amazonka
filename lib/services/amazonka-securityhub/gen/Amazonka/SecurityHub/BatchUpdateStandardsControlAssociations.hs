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
-- Module      : Amazonka.SecurityHub.BatchUpdateStandardsControlAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For a batch of security controls and standards, this operation updates
-- the enablement status of a control in a standard.
module Amazonka.SecurityHub.BatchUpdateStandardsControlAssociations
  ( -- * Creating a Request
    BatchUpdateStandardsControlAssociations (..),
    newBatchUpdateStandardsControlAssociations,

    -- * Request Lenses
    batchUpdateStandardsControlAssociations_standardsControlAssociationUpdates,

    -- * Destructuring the Response
    BatchUpdateStandardsControlAssociationsResponse (..),
    newBatchUpdateStandardsControlAssociationsResponse,

    -- * Response Lenses
    batchUpdateStandardsControlAssociationsResponse_unprocessedAssociationUpdates,
    batchUpdateStandardsControlAssociationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newBatchUpdateStandardsControlAssociations' smart constructor.
data BatchUpdateStandardsControlAssociations = BatchUpdateStandardsControlAssociations'
  { -- | Updates the enablement status of a security control in a specified
    -- standard.
    standardsControlAssociationUpdates :: [StandardsControlAssociationUpdate]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdateStandardsControlAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'standardsControlAssociationUpdates', 'batchUpdateStandardsControlAssociations_standardsControlAssociationUpdates' - Updates the enablement status of a security control in a specified
-- standard.
newBatchUpdateStandardsControlAssociations ::
  BatchUpdateStandardsControlAssociations
newBatchUpdateStandardsControlAssociations =
  BatchUpdateStandardsControlAssociations'
    { standardsControlAssociationUpdates =
        Prelude.mempty
    }

-- | Updates the enablement status of a security control in a specified
-- standard.
batchUpdateStandardsControlAssociations_standardsControlAssociationUpdates :: Lens.Lens' BatchUpdateStandardsControlAssociations [StandardsControlAssociationUpdate]
batchUpdateStandardsControlAssociations_standardsControlAssociationUpdates = Lens.lens (\BatchUpdateStandardsControlAssociations' {standardsControlAssociationUpdates} -> standardsControlAssociationUpdates) (\s@BatchUpdateStandardsControlAssociations' {} a -> s {standardsControlAssociationUpdates = a} :: BatchUpdateStandardsControlAssociations) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    BatchUpdateStandardsControlAssociations
  where
  type
    AWSResponse
      BatchUpdateStandardsControlAssociations =
      BatchUpdateStandardsControlAssociationsResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchUpdateStandardsControlAssociationsResponse'
            Prelude.<$> ( x
                            Data..?> "UnprocessedAssociationUpdates"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    BatchUpdateStandardsControlAssociations
  where
  hashWithSalt
    _salt
    BatchUpdateStandardsControlAssociations' {..} =
      _salt
        `Prelude.hashWithSalt` standardsControlAssociationUpdates

instance
  Prelude.NFData
    BatchUpdateStandardsControlAssociations
  where
  rnf BatchUpdateStandardsControlAssociations' {..} =
    Prelude.rnf standardsControlAssociationUpdates

instance
  Data.ToHeaders
    BatchUpdateStandardsControlAssociations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    BatchUpdateStandardsControlAssociations
  where
  toJSON BatchUpdateStandardsControlAssociations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "StandardsControlAssociationUpdates"
                  Data..= standardsControlAssociationUpdates
              )
          ]
      )

instance
  Data.ToPath
    BatchUpdateStandardsControlAssociations
  where
  toPath = Prelude.const "/associations"

instance
  Data.ToQuery
    BatchUpdateStandardsControlAssociations
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchUpdateStandardsControlAssociationsResponse' smart constructor.
data BatchUpdateStandardsControlAssociationsResponse = BatchUpdateStandardsControlAssociationsResponse'
  { -- | A security control (identified with @SecurityControlId@,
    -- @SecurityControlArn@, or a mix of both parameters) whose enablement
    -- status in a specified standard couldn\'t be updated.
    unprocessedAssociationUpdates :: Prelude.Maybe [UnprocessedStandardsControlAssociationUpdate],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdateStandardsControlAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unprocessedAssociationUpdates', 'batchUpdateStandardsControlAssociationsResponse_unprocessedAssociationUpdates' - A security control (identified with @SecurityControlId@,
-- @SecurityControlArn@, or a mix of both parameters) whose enablement
-- status in a specified standard couldn\'t be updated.
--
-- 'httpStatus', 'batchUpdateStandardsControlAssociationsResponse_httpStatus' - The response's http status code.
newBatchUpdateStandardsControlAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchUpdateStandardsControlAssociationsResponse
newBatchUpdateStandardsControlAssociationsResponse
  pHttpStatus_ =
    BatchUpdateStandardsControlAssociationsResponse'
      { unprocessedAssociationUpdates =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A security control (identified with @SecurityControlId@,
-- @SecurityControlArn@, or a mix of both parameters) whose enablement
-- status in a specified standard couldn\'t be updated.
batchUpdateStandardsControlAssociationsResponse_unprocessedAssociationUpdates :: Lens.Lens' BatchUpdateStandardsControlAssociationsResponse (Prelude.Maybe [UnprocessedStandardsControlAssociationUpdate])
batchUpdateStandardsControlAssociationsResponse_unprocessedAssociationUpdates = Lens.lens (\BatchUpdateStandardsControlAssociationsResponse' {unprocessedAssociationUpdates} -> unprocessedAssociationUpdates) (\s@BatchUpdateStandardsControlAssociationsResponse' {} a -> s {unprocessedAssociationUpdates = a} :: BatchUpdateStandardsControlAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchUpdateStandardsControlAssociationsResponse_httpStatus :: Lens.Lens' BatchUpdateStandardsControlAssociationsResponse Prelude.Int
batchUpdateStandardsControlAssociationsResponse_httpStatus = Lens.lens (\BatchUpdateStandardsControlAssociationsResponse' {httpStatus} -> httpStatus) (\s@BatchUpdateStandardsControlAssociationsResponse' {} a -> s {httpStatus = a} :: BatchUpdateStandardsControlAssociationsResponse)

instance
  Prelude.NFData
    BatchUpdateStandardsControlAssociationsResponse
  where
  rnf
    BatchUpdateStandardsControlAssociationsResponse' {..} =
      Prelude.rnf unprocessedAssociationUpdates
        `Prelude.seq` Prelude.rnf httpStatus
