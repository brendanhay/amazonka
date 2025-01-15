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
-- Module      : Amazonka.BillingConductor.BatchAssociateResourcesToCustomLineItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a batch of resources to a percentage custom line item.
module Amazonka.BillingConductor.BatchAssociateResourcesToCustomLineItem
  ( -- * Creating a Request
    BatchAssociateResourcesToCustomLineItem (..),
    newBatchAssociateResourcesToCustomLineItem,

    -- * Request Lenses
    batchAssociateResourcesToCustomLineItem_billingPeriodRange,
    batchAssociateResourcesToCustomLineItem_targetArn,
    batchAssociateResourcesToCustomLineItem_resourceArns,

    -- * Destructuring the Response
    BatchAssociateResourcesToCustomLineItemResponse (..),
    newBatchAssociateResourcesToCustomLineItemResponse,

    -- * Response Lenses
    batchAssociateResourcesToCustomLineItemResponse_failedAssociatedResources,
    batchAssociateResourcesToCustomLineItemResponse_successfullyAssociatedResources,
    batchAssociateResourcesToCustomLineItemResponse_httpStatus,
  )
where

import Amazonka.BillingConductor.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchAssociateResourcesToCustomLineItem' smart constructor.
data BatchAssociateResourcesToCustomLineItem = BatchAssociateResourcesToCustomLineItem'
  { billingPeriodRange :: Prelude.Maybe CustomLineItemBillingPeriodRange,
    -- | A percentage custom line item ARN to associate the resources to.
    targetArn :: Prelude.Text,
    -- | A list containing the ARNs of the resources to be associated.
    resourceArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchAssociateResourcesToCustomLineItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingPeriodRange', 'batchAssociateResourcesToCustomLineItem_billingPeriodRange' - Undocumented member.
--
-- 'targetArn', 'batchAssociateResourcesToCustomLineItem_targetArn' - A percentage custom line item ARN to associate the resources to.
--
-- 'resourceArns', 'batchAssociateResourcesToCustomLineItem_resourceArns' - A list containing the ARNs of the resources to be associated.
newBatchAssociateResourcesToCustomLineItem ::
  -- | 'targetArn'
  Prelude.Text ->
  -- | 'resourceArns'
  Prelude.NonEmpty Prelude.Text ->
  BatchAssociateResourcesToCustomLineItem
newBatchAssociateResourcesToCustomLineItem
  pTargetArn_
  pResourceArns_ =
    BatchAssociateResourcesToCustomLineItem'
      { billingPeriodRange =
          Prelude.Nothing,
        targetArn = pTargetArn_,
        resourceArns =
          Lens.coerced
            Lens.# pResourceArns_
      }

-- | Undocumented member.
batchAssociateResourcesToCustomLineItem_billingPeriodRange :: Lens.Lens' BatchAssociateResourcesToCustomLineItem (Prelude.Maybe CustomLineItemBillingPeriodRange)
batchAssociateResourcesToCustomLineItem_billingPeriodRange = Lens.lens (\BatchAssociateResourcesToCustomLineItem' {billingPeriodRange} -> billingPeriodRange) (\s@BatchAssociateResourcesToCustomLineItem' {} a -> s {billingPeriodRange = a} :: BatchAssociateResourcesToCustomLineItem)

-- | A percentage custom line item ARN to associate the resources to.
batchAssociateResourcesToCustomLineItem_targetArn :: Lens.Lens' BatchAssociateResourcesToCustomLineItem Prelude.Text
batchAssociateResourcesToCustomLineItem_targetArn = Lens.lens (\BatchAssociateResourcesToCustomLineItem' {targetArn} -> targetArn) (\s@BatchAssociateResourcesToCustomLineItem' {} a -> s {targetArn = a} :: BatchAssociateResourcesToCustomLineItem)

-- | A list containing the ARNs of the resources to be associated.
batchAssociateResourcesToCustomLineItem_resourceArns :: Lens.Lens' BatchAssociateResourcesToCustomLineItem (Prelude.NonEmpty Prelude.Text)
batchAssociateResourcesToCustomLineItem_resourceArns = Lens.lens (\BatchAssociateResourcesToCustomLineItem' {resourceArns} -> resourceArns) (\s@BatchAssociateResourcesToCustomLineItem' {} a -> s {resourceArns = a} :: BatchAssociateResourcesToCustomLineItem) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    BatchAssociateResourcesToCustomLineItem
  where
  type
    AWSResponse
      BatchAssociateResourcesToCustomLineItem =
      BatchAssociateResourcesToCustomLineItemResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchAssociateResourcesToCustomLineItemResponse'
            Prelude.<$> ( x
                            Data..?> "FailedAssociatedResources"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..?> "SuccessfullyAssociatedResources"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    BatchAssociateResourcesToCustomLineItem
  where
  hashWithSalt
    _salt
    BatchAssociateResourcesToCustomLineItem' {..} =
      _salt
        `Prelude.hashWithSalt` billingPeriodRange
        `Prelude.hashWithSalt` targetArn
        `Prelude.hashWithSalt` resourceArns

instance
  Prelude.NFData
    BatchAssociateResourcesToCustomLineItem
  where
  rnf BatchAssociateResourcesToCustomLineItem' {..} =
    Prelude.rnf billingPeriodRange `Prelude.seq`
      Prelude.rnf targetArn `Prelude.seq`
        Prelude.rnf resourceArns

instance
  Data.ToHeaders
    BatchAssociateResourcesToCustomLineItem
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
    BatchAssociateResourcesToCustomLineItem
  where
  toJSON BatchAssociateResourcesToCustomLineItem' {..} =
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
    BatchAssociateResourcesToCustomLineItem
  where
  toPath =
    Prelude.const
      "/batch-associate-resources-to-custom-line-item"

instance
  Data.ToQuery
    BatchAssociateResourcesToCustomLineItem
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchAssociateResourcesToCustomLineItemResponse' smart constructor.
data BatchAssociateResourcesToCustomLineItemResponse = BatchAssociateResourcesToCustomLineItemResponse'
  { -- | A list of @AssociateResourceResponseElement@ for each resource that
    -- failed association to a percentage custom line item.
    failedAssociatedResources :: Prelude.Maybe [AssociateResourceResponseElement],
    -- | A list of @AssociateResourceResponseElement@ for each resource that\'s
    -- been associated to a percentage custom line item successfully.
    successfullyAssociatedResources :: Prelude.Maybe [AssociateResourceResponseElement],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchAssociateResourcesToCustomLineItemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedAssociatedResources', 'batchAssociateResourcesToCustomLineItemResponse_failedAssociatedResources' - A list of @AssociateResourceResponseElement@ for each resource that
-- failed association to a percentage custom line item.
--
-- 'successfullyAssociatedResources', 'batchAssociateResourcesToCustomLineItemResponse_successfullyAssociatedResources' - A list of @AssociateResourceResponseElement@ for each resource that\'s
-- been associated to a percentage custom line item successfully.
--
-- 'httpStatus', 'batchAssociateResourcesToCustomLineItemResponse_httpStatus' - The response's http status code.
newBatchAssociateResourcesToCustomLineItemResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchAssociateResourcesToCustomLineItemResponse
newBatchAssociateResourcesToCustomLineItemResponse
  pHttpStatus_ =
    BatchAssociateResourcesToCustomLineItemResponse'
      { failedAssociatedResources =
          Prelude.Nothing,
        successfullyAssociatedResources =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of @AssociateResourceResponseElement@ for each resource that
-- failed association to a percentage custom line item.
batchAssociateResourcesToCustomLineItemResponse_failedAssociatedResources :: Lens.Lens' BatchAssociateResourcesToCustomLineItemResponse (Prelude.Maybe [AssociateResourceResponseElement])
batchAssociateResourcesToCustomLineItemResponse_failedAssociatedResources = Lens.lens (\BatchAssociateResourcesToCustomLineItemResponse' {failedAssociatedResources} -> failedAssociatedResources) (\s@BatchAssociateResourcesToCustomLineItemResponse' {} a -> s {failedAssociatedResources = a} :: BatchAssociateResourcesToCustomLineItemResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of @AssociateResourceResponseElement@ for each resource that\'s
-- been associated to a percentage custom line item successfully.
batchAssociateResourcesToCustomLineItemResponse_successfullyAssociatedResources :: Lens.Lens' BatchAssociateResourcesToCustomLineItemResponse (Prelude.Maybe [AssociateResourceResponseElement])
batchAssociateResourcesToCustomLineItemResponse_successfullyAssociatedResources = Lens.lens (\BatchAssociateResourcesToCustomLineItemResponse' {successfullyAssociatedResources} -> successfullyAssociatedResources) (\s@BatchAssociateResourcesToCustomLineItemResponse' {} a -> s {successfullyAssociatedResources = a} :: BatchAssociateResourcesToCustomLineItemResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchAssociateResourcesToCustomLineItemResponse_httpStatus :: Lens.Lens' BatchAssociateResourcesToCustomLineItemResponse Prelude.Int
batchAssociateResourcesToCustomLineItemResponse_httpStatus = Lens.lens (\BatchAssociateResourcesToCustomLineItemResponse' {httpStatus} -> httpStatus) (\s@BatchAssociateResourcesToCustomLineItemResponse' {} a -> s {httpStatus = a} :: BatchAssociateResourcesToCustomLineItemResponse)

instance
  Prelude.NFData
    BatchAssociateResourcesToCustomLineItemResponse
  where
  rnf
    BatchAssociateResourcesToCustomLineItemResponse' {..} =
      Prelude.rnf failedAssociatedResources `Prelude.seq`
        Prelude.rnf successfullyAssociatedResources `Prelude.seq`
          Prelude.rnf httpStatus
