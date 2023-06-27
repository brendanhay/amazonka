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
-- Module      : Amazonka.SecurityHub.BatchGetStandardsControlAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For a batch of security controls and standards, identifies whether each
-- control is currently enabled or disabled in a standard.
module Amazonka.SecurityHub.BatchGetStandardsControlAssociations
  ( -- * Creating a Request
    BatchGetStandardsControlAssociations (..),
    newBatchGetStandardsControlAssociations,

    -- * Request Lenses
    batchGetStandardsControlAssociations_standardsControlAssociationIds,

    -- * Destructuring the Response
    BatchGetStandardsControlAssociationsResponse (..),
    newBatchGetStandardsControlAssociationsResponse,

    -- * Response Lenses
    batchGetStandardsControlAssociationsResponse_unprocessedAssociations,
    batchGetStandardsControlAssociationsResponse_httpStatus,
    batchGetStandardsControlAssociationsResponse_standardsControlAssociationDetails,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newBatchGetStandardsControlAssociations' smart constructor.
data BatchGetStandardsControlAssociations = BatchGetStandardsControlAssociations'
  { -- | An array with one or more objects that includes a security control
    -- (identified with @SecurityControlId@, @SecurityControlArn@, or a mix of
    -- both parameters) and the Amazon Resource Name (ARN) of a standard. This
    -- field is used to query the enablement status of a control in a specified
    -- standard. The security control ID or ARN is the same across standards.
    standardsControlAssociationIds :: [StandardsControlAssociationId]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetStandardsControlAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'standardsControlAssociationIds', 'batchGetStandardsControlAssociations_standardsControlAssociationIds' - An array with one or more objects that includes a security control
-- (identified with @SecurityControlId@, @SecurityControlArn@, or a mix of
-- both parameters) and the Amazon Resource Name (ARN) of a standard. This
-- field is used to query the enablement status of a control in a specified
-- standard. The security control ID or ARN is the same across standards.
newBatchGetStandardsControlAssociations ::
  BatchGetStandardsControlAssociations
newBatchGetStandardsControlAssociations =
  BatchGetStandardsControlAssociations'
    { standardsControlAssociationIds =
        Prelude.mempty
    }

-- | An array with one or more objects that includes a security control
-- (identified with @SecurityControlId@, @SecurityControlArn@, or a mix of
-- both parameters) and the Amazon Resource Name (ARN) of a standard. This
-- field is used to query the enablement status of a control in a specified
-- standard. The security control ID or ARN is the same across standards.
batchGetStandardsControlAssociations_standardsControlAssociationIds :: Lens.Lens' BatchGetStandardsControlAssociations [StandardsControlAssociationId]
batchGetStandardsControlAssociations_standardsControlAssociationIds = Lens.lens (\BatchGetStandardsControlAssociations' {standardsControlAssociationIds} -> standardsControlAssociationIds) (\s@BatchGetStandardsControlAssociations' {} a -> s {standardsControlAssociationIds = a} :: BatchGetStandardsControlAssociations) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    BatchGetStandardsControlAssociations
  where
  type
    AWSResponse BatchGetStandardsControlAssociations =
      BatchGetStandardsControlAssociationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetStandardsControlAssociationsResponse'
            Prelude.<$> ( x
                            Data..?> "UnprocessedAssociations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "StandardsControlAssociationDetails"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    BatchGetStandardsControlAssociations
  where
  hashWithSalt
    _salt
    BatchGetStandardsControlAssociations' {..} =
      _salt
        `Prelude.hashWithSalt` standardsControlAssociationIds

instance
  Prelude.NFData
    BatchGetStandardsControlAssociations
  where
  rnf BatchGetStandardsControlAssociations' {..} =
    Prelude.rnf standardsControlAssociationIds

instance
  Data.ToHeaders
    BatchGetStandardsControlAssociations
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
    BatchGetStandardsControlAssociations
  where
  toJSON BatchGetStandardsControlAssociations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "StandardsControlAssociationIds"
                  Data..= standardsControlAssociationIds
              )
          ]
      )

instance
  Data.ToPath
    BatchGetStandardsControlAssociations
  where
  toPath = Prelude.const "/associations/batchGet"

instance
  Data.ToQuery
    BatchGetStandardsControlAssociations
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetStandardsControlAssociationsResponse' smart constructor.
data BatchGetStandardsControlAssociationsResponse = BatchGetStandardsControlAssociationsResponse'
  { -- | A security control (identified with @SecurityControlId@,
    -- @SecurityControlArn@, or a mix of both parameters) whose enablement
    -- status in a specified standard cannot be returned.
    unprocessedAssociations :: Prelude.Maybe [UnprocessedStandardsControlAssociation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Provides the enablement status of a security control in a specified
    -- standard and other details for the control in relation to the specified
    -- standard.
    standardsControlAssociationDetails :: [StandardsControlAssociationDetail]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetStandardsControlAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unprocessedAssociations', 'batchGetStandardsControlAssociationsResponse_unprocessedAssociations' - A security control (identified with @SecurityControlId@,
-- @SecurityControlArn@, or a mix of both parameters) whose enablement
-- status in a specified standard cannot be returned.
--
-- 'httpStatus', 'batchGetStandardsControlAssociationsResponse_httpStatus' - The response's http status code.
--
-- 'standardsControlAssociationDetails', 'batchGetStandardsControlAssociationsResponse_standardsControlAssociationDetails' - Provides the enablement status of a security control in a specified
-- standard and other details for the control in relation to the specified
-- standard.
newBatchGetStandardsControlAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetStandardsControlAssociationsResponse
newBatchGetStandardsControlAssociationsResponse
  pHttpStatus_ =
    BatchGetStandardsControlAssociationsResponse'
      { unprocessedAssociations =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        standardsControlAssociationDetails =
          Prelude.mempty
      }

-- | A security control (identified with @SecurityControlId@,
-- @SecurityControlArn@, or a mix of both parameters) whose enablement
-- status in a specified standard cannot be returned.
batchGetStandardsControlAssociationsResponse_unprocessedAssociations :: Lens.Lens' BatchGetStandardsControlAssociationsResponse (Prelude.Maybe [UnprocessedStandardsControlAssociation])
batchGetStandardsControlAssociationsResponse_unprocessedAssociations = Lens.lens (\BatchGetStandardsControlAssociationsResponse' {unprocessedAssociations} -> unprocessedAssociations) (\s@BatchGetStandardsControlAssociationsResponse' {} a -> s {unprocessedAssociations = a} :: BatchGetStandardsControlAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetStandardsControlAssociationsResponse_httpStatus :: Lens.Lens' BatchGetStandardsControlAssociationsResponse Prelude.Int
batchGetStandardsControlAssociationsResponse_httpStatus = Lens.lens (\BatchGetStandardsControlAssociationsResponse' {httpStatus} -> httpStatus) (\s@BatchGetStandardsControlAssociationsResponse' {} a -> s {httpStatus = a} :: BatchGetStandardsControlAssociationsResponse)

-- | Provides the enablement status of a security control in a specified
-- standard and other details for the control in relation to the specified
-- standard.
batchGetStandardsControlAssociationsResponse_standardsControlAssociationDetails :: Lens.Lens' BatchGetStandardsControlAssociationsResponse [StandardsControlAssociationDetail]
batchGetStandardsControlAssociationsResponse_standardsControlAssociationDetails = Lens.lens (\BatchGetStandardsControlAssociationsResponse' {standardsControlAssociationDetails} -> standardsControlAssociationDetails) (\s@BatchGetStandardsControlAssociationsResponse' {} a -> s {standardsControlAssociationDetails = a} :: BatchGetStandardsControlAssociationsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    BatchGetStandardsControlAssociationsResponse
  where
  rnf BatchGetStandardsControlAssociationsResponse' {..} =
    Prelude.rnf unprocessedAssociations
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf standardsControlAssociationDetails
