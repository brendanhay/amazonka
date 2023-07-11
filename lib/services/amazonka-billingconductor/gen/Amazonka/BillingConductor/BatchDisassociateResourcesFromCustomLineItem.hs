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
-- Module      : Amazonka.BillingConductor.BatchDisassociateResourcesFromCustomLineItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a batch of resources from a percentage custom line item.
module Amazonka.BillingConductor.BatchDisassociateResourcesFromCustomLineItem
  ( -- * Creating a Request
    BatchDisassociateResourcesFromCustomLineItem (..),
    newBatchDisassociateResourcesFromCustomLineItem,

    -- * Request Lenses
    batchDisassociateResourcesFromCustomLineItem_billingPeriodRange,
    batchDisassociateResourcesFromCustomLineItem_targetArn,
    batchDisassociateResourcesFromCustomLineItem_resourceArns,

    -- * Destructuring the Response
    BatchDisassociateResourcesFromCustomLineItemResponse (..),
    newBatchDisassociateResourcesFromCustomLineItemResponse,

    -- * Response Lenses
    batchDisassociateResourcesFromCustomLineItemResponse_failedDisassociatedResources,
    batchDisassociateResourcesFromCustomLineItemResponse_successfullyDisassociatedResources,
    batchDisassociateResourcesFromCustomLineItemResponse_httpStatus,
  )
where

import Amazonka.BillingConductor.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchDisassociateResourcesFromCustomLineItem' smart constructor.
data BatchDisassociateResourcesFromCustomLineItem = BatchDisassociateResourcesFromCustomLineItem'
  { billingPeriodRange :: Prelude.Maybe CustomLineItemBillingPeriodRange,
    -- | A percentage custom line item ARN to disassociate the resources from.
    targetArn :: Prelude.Text,
    -- | A list containing the ARNs of resources to be disassociated.
    resourceArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDisassociateResourcesFromCustomLineItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingPeriodRange', 'batchDisassociateResourcesFromCustomLineItem_billingPeriodRange' - Undocumented member.
--
-- 'targetArn', 'batchDisassociateResourcesFromCustomLineItem_targetArn' - A percentage custom line item ARN to disassociate the resources from.
--
-- 'resourceArns', 'batchDisassociateResourcesFromCustomLineItem_resourceArns' - A list containing the ARNs of resources to be disassociated.
newBatchDisassociateResourcesFromCustomLineItem ::
  -- | 'targetArn'
  Prelude.Text ->
  -- | 'resourceArns'
  Prelude.NonEmpty Prelude.Text ->
  BatchDisassociateResourcesFromCustomLineItem
newBatchDisassociateResourcesFromCustomLineItem
  pTargetArn_
  pResourceArns_ =
    BatchDisassociateResourcesFromCustomLineItem'
      { billingPeriodRange =
          Prelude.Nothing,
        targetArn = pTargetArn_,
        resourceArns =
          Lens.coerced
            Lens.# pResourceArns_
      }

-- | Undocumented member.
batchDisassociateResourcesFromCustomLineItem_billingPeriodRange :: Lens.Lens' BatchDisassociateResourcesFromCustomLineItem (Prelude.Maybe CustomLineItemBillingPeriodRange)
batchDisassociateResourcesFromCustomLineItem_billingPeriodRange = Lens.lens (\BatchDisassociateResourcesFromCustomLineItem' {billingPeriodRange} -> billingPeriodRange) (\s@BatchDisassociateResourcesFromCustomLineItem' {} a -> s {billingPeriodRange = a} :: BatchDisassociateResourcesFromCustomLineItem)

-- | A percentage custom line item ARN to disassociate the resources from.
batchDisassociateResourcesFromCustomLineItem_targetArn :: Lens.Lens' BatchDisassociateResourcesFromCustomLineItem Prelude.Text
batchDisassociateResourcesFromCustomLineItem_targetArn = Lens.lens (\BatchDisassociateResourcesFromCustomLineItem' {targetArn} -> targetArn) (\s@BatchDisassociateResourcesFromCustomLineItem' {} a -> s {targetArn = a} :: BatchDisassociateResourcesFromCustomLineItem)

-- | A list containing the ARNs of resources to be disassociated.
batchDisassociateResourcesFromCustomLineItem_resourceArns :: Lens.Lens' BatchDisassociateResourcesFromCustomLineItem (Prelude.NonEmpty Prelude.Text)
batchDisassociateResourcesFromCustomLineItem_resourceArns = Lens.lens (\BatchDisassociateResourcesFromCustomLineItem' {resourceArns} -> resourceArns) (\s@BatchDisassociateResourcesFromCustomLineItem' {} a -> s {resourceArns = a} :: BatchDisassociateResourcesFromCustomLineItem) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    BatchDisassociateResourcesFromCustomLineItem
  where
  type
    AWSResponse
      BatchDisassociateResourcesFromCustomLineItem =
      BatchDisassociateResourcesFromCustomLineItemResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDisassociateResourcesFromCustomLineItemResponse'
            Prelude.<$> ( x
                            Data..?> "FailedDisassociatedResources"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..?> "SuccessfullyDisassociatedResources"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    BatchDisassociateResourcesFromCustomLineItem
  where
  hashWithSalt
    _salt
    BatchDisassociateResourcesFromCustomLineItem' {..} =
      _salt
        `Prelude.hashWithSalt` billingPeriodRange
        `Prelude.hashWithSalt` targetArn
        `Prelude.hashWithSalt` resourceArns

instance
  Prelude.NFData
    BatchDisassociateResourcesFromCustomLineItem
  where
  rnf BatchDisassociateResourcesFromCustomLineItem' {..} =
    Prelude.rnf billingPeriodRange
      `Prelude.seq` Prelude.rnf targetArn
      `Prelude.seq` Prelude.rnf resourceArns

instance
  Data.ToHeaders
    BatchDisassociateResourcesFromCustomLineItem
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
    BatchDisassociateResourcesFromCustomLineItem
  where
  toJSON
    BatchDisassociateResourcesFromCustomLineItem' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("BillingPeriodRange" Data..=)
                Prelude.<$> billingPeriodRange,
              Prelude.Just ("TargetArn" Data..= targetArn),
              Prelude.Just ("ResourceArns" Data..= resourceArns)
            ]
        )

instance
  Data.ToPath
    BatchDisassociateResourcesFromCustomLineItem
  where
  toPath =
    Prelude.const
      "/batch-disassociate-resources-from-custom-line-item"

instance
  Data.ToQuery
    BatchDisassociateResourcesFromCustomLineItem
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDisassociateResourcesFromCustomLineItemResponse' smart constructor.
data BatchDisassociateResourcesFromCustomLineItemResponse = BatchDisassociateResourcesFromCustomLineItemResponse'
  { -- | A list of @DisassociateResourceResponseElement@ for each resource that
    -- failed disassociation from a percentage custom line item.
    failedDisassociatedResources :: Prelude.Maybe [DisassociateResourceResponseElement],
    -- | A list of @DisassociateResourceResponseElement@ for each resource
    -- that\'s been disassociated from a percentage custom line item
    -- successfully.
    successfullyDisassociatedResources :: Prelude.Maybe [DisassociateResourceResponseElement],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDisassociateResourcesFromCustomLineItemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedDisassociatedResources', 'batchDisassociateResourcesFromCustomLineItemResponse_failedDisassociatedResources' - A list of @DisassociateResourceResponseElement@ for each resource that
-- failed disassociation from a percentage custom line item.
--
-- 'successfullyDisassociatedResources', 'batchDisassociateResourcesFromCustomLineItemResponse_successfullyDisassociatedResources' - A list of @DisassociateResourceResponseElement@ for each resource
-- that\'s been disassociated from a percentage custom line item
-- successfully.
--
-- 'httpStatus', 'batchDisassociateResourcesFromCustomLineItemResponse_httpStatus' - The response's http status code.
newBatchDisassociateResourcesFromCustomLineItemResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDisassociateResourcesFromCustomLineItemResponse
newBatchDisassociateResourcesFromCustomLineItemResponse
  pHttpStatus_ =
    BatchDisassociateResourcesFromCustomLineItemResponse'
      { failedDisassociatedResources =
          Prelude.Nothing,
        successfullyDisassociatedResources =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | A list of @DisassociateResourceResponseElement@ for each resource that
-- failed disassociation from a percentage custom line item.
batchDisassociateResourcesFromCustomLineItemResponse_failedDisassociatedResources :: Lens.Lens' BatchDisassociateResourcesFromCustomLineItemResponse (Prelude.Maybe [DisassociateResourceResponseElement])
batchDisassociateResourcesFromCustomLineItemResponse_failedDisassociatedResources = Lens.lens (\BatchDisassociateResourcesFromCustomLineItemResponse' {failedDisassociatedResources} -> failedDisassociatedResources) (\s@BatchDisassociateResourcesFromCustomLineItemResponse' {} a -> s {failedDisassociatedResources = a} :: BatchDisassociateResourcesFromCustomLineItemResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of @DisassociateResourceResponseElement@ for each resource
-- that\'s been disassociated from a percentage custom line item
-- successfully.
batchDisassociateResourcesFromCustomLineItemResponse_successfullyDisassociatedResources :: Lens.Lens' BatchDisassociateResourcesFromCustomLineItemResponse (Prelude.Maybe [DisassociateResourceResponseElement])
batchDisassociateResourcesFromCustomLineItemResponse_successfullyDisassociatedResources = Lens.lens (\BatchDisassociateResourcesFromCustomLineItemResponse' {successfullyDisassociatedResources} -> successfullyDisassociatedResources) (\s@BatchDisassociateResourcesFromCustomLineItemResponse' {} a -> s {successfullyDisassociatedResources = a} :: BatchDisassociateResourcesFromCustomLineItemResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchDisassociateResourcesFromCustomLineItemResponse_httpStatus :: Lens.Lens' BatchDisassociateResourcesFromCustomLineItemResponse Prelude.Int
batchDisassociateResourcesFromCustomLineItemResponse_httpStatus = Lens.lens (\BatchDisassociateResourcesFromCustomLineItemResponse' {httpStatus} -> httpStatus) (\s@BatchDisassociateResourcesFromCustomLineItemResponse' {} a -> s {httpStatus = a} :: BatchDisassociateResourcesFromCustomLineItemResponse)

instance
  Prelude.NFData
    BatchDisassociateResourcesFromCustomLineItemResponse
  where
  rnf
    BatchDisassociateResourcesFromCustomLineItemResponse' {..} =
      Prelude.rnf failedDisassociatedResources
        `Prelude.seq` Prelude.rnf successfullyDisassociatedResources
        `Prelude.seq` Prelude.rnf httpStatus
