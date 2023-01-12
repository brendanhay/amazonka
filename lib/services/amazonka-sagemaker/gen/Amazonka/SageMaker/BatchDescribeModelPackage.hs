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
-- Module      : Amazonka.SageMaker.BatchDescribeModelPackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action batch describes a list of versioned model packages
module Amazonka.SageMaker.BatchDescribeModelPackage
  ( -- * Creating a Request
    BatchDescribeModelPackage (..),
    newBatchDescribeModelPackage,

    -- * Request Lenses
    batchDescribeModelPackage_modelPackageArnList,

    -- * Destructuring the Response
    BatchDescribeModelPackageResponse (..),
    newBatchDescribeModelPackageResponse,

    -- * Response Lenses
    batchDescribeModelPackageResponse_batchDescribeModelPackageErrorMap,
    batchDescribeModelPackageResponse_modelPackageSummaries,
    batchDescribeModelPackageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newBatchDescribeModelPackage' smart constructor.
data BatchDescribeModelPackage = BatchDescribeModelPackage'
  { -- | The list of Amazon Resource Name (ARN) of the model package groups.
    modelPackageArnList :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDescribeModelPackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelPackageArnList', 'batchDescribeModelPackage_modelPackageArnList' - The list of Amazon Resource Name (ARN) of the model package groups.
newBatchDescribeModelPackage ::
  -- | 'modelPackageArnList'
  Prelude.NonEmpty Prelude.Text ->
  BatchDescribeModelPackage
newBatchDescribeModelPackage pModelPackageArnList_ =
  BatchDescribeModelPackage'
    { modelPackageArnList =
        Lens.coerced Lens.# pModelPackageArnList_
    }

-- | The list of Amazon Resource Name (ARN) of the model package groups.
batchDescribeModelPackage_modelPackageArnList :: Lens.Lens' BatchDescribeModelPackage (Prelude.NonEmpty Prelude.Text)
batchDescribeModelPackage_modelPackageArnList = Lens.lens (\BatchDescribeModelPackage' {modelPackageArnList} -> modelPackageArnList) (\s@BatchDescribeModelPackage' {} a -> s {modelPackageArnList = a} :: BatchDescribeModelPackage) Prelude.. Lens.coerced

instance Core.AWSRequest BatchDescribeModelPackage where
  type
    AWSResponse BatchDescribeModelPackage =
      BatchDescribeModelPackageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDescribeModelPackageResponse'
            Prelude.<$> ( x Data..?> "BatchDescribeModelPackageErrorMap"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Data..?> "ModelPackageSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchDescribeModelPackage where
  hashWithSalt _salt BatchDescribeModelPackage' {..} =
    _salt `Prelude.hashWithSalt` modelPackageArnList

instance Prelude.NFData BatchDescribeModelPackage where
  rnf BatchDescribeModelPackage' {..} =
    Prelude.rnf modelPackageArnList

instance Data.ToHeaders BatchDescribeModelPackage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.BatchDescribeModelPackage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchDescribeModelPackage where
  toJSON BatchDescribeModelPackage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ModelPackageArnList" Data..= modelPackageArnList)
          ]
      )

instance Data.ToPath BatchDescribeModelPackage where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchDescribeModelPackage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDescribeModelPackageResponse' smart constructor.
data BatchDescribeModelPackageResponse = BatchDescribeModelPackageResponse'
  { -- | A map of the resource and BatchDescribeModelPackageError objects
    -- reporting the error associated with describing the model package.
    batchDescribeModelPackageErrorMap :: Prelude.Maybe (Prelude.HashMap Prelude.Text BatchDescribeModelPackageError),
    -- | The summaries for the model package versions
    modelPackageSummaries :: Prelude.Maybe (Prelude.HashMap Prelude.Text BatchDescribeModelPackageSummary),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDescribeModelPackageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchDescribeModelPackageErrorMap', 'batchDescribeModelPackageResponse_batchDescribeModelPackageErrorMap' - A map of the resource and BatchDescribeModelPackageError objects
-- reporting the error associated with describing the model package.
--
-- 'modelPackageSummaries', 'batchDescribeModelPackageResponse_modelPackageSummaries' - The summaries for the model package versions
--
-- 'httpStatus', 'batchDescribeModelPackageResponse_httpStatus' - The response's http status code.
newBatchDescribeModelPackageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDescribeModelPackageResponse
newBatchDescribeModelPackageResponse pHttpStatus_ =
  BatchDescribeModelPackageResponse'
    { batchDescribeModelPackageErrorMap =
        Prelude.Nothing,
      modelPackageSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A map of the resource and BatchDescribeModelPackageError objects
-- reporting the error associated with describing the model package.
batchDescribeModelPackageResponse_batchDescribeModelPackageErrorMap :: Lens.Lens' BatchDescribeModelPackageResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text BatchDescribeModelPackageError))
batchDescribeModelPackageResponse_batchDescribeModelPackageErrorMap = Lens.lens (\BatchDescribeModelPackageResponse' {batchDescribeModelPackageErrorMap} -> batchDescribeModelPackageErrorMap) (\s@BatchDescribeModelPackageResponse' {} a -> s {batchDescribeModelPackageErrorMap = a} :: BatchDescribeModelPackageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The summaries for the model package versions
batchDescribeModelPackageResponse_modelPackageSummaries :: Lens.Lens' BatchDescribeModelPackageResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text BatchDescribeModelPackageSummary))
batchDescribeModelPackageResponse_modelPackageSummaries = Lens.lens (\BatchDescribeModelPackageResponse' {modelPackageSummaries} -> modelPackageSummaries) (\s@BatchDescribeModelPackageResponse' {} a -> s {modelPackageSummaries = a} :: BatchDescribeModelPackageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchDescribeModelPackageResponse_httpStatus :: Lens.Lens' BatchDescribeModelPackageResponse Prelude.Int
batchDescribeModelPackageResponse_httpStatus = Lens.lens (\BatchDescribeModelPackageResponse' {httpStatus} -> httpStatus) (\s@BatchDescribeModelPackageResponse' {} a -> s {httpStatus = a} :: BatchDescribeModelPackageResponse)

instance
  Prelude.NFData
    BatchDescribeModelPackageResponse
  where
  rnf BatchDescribeModelPackageResponse' {..} =
    Prelude.rnf batchDescribeModelPackageErrorMap
      `Prelude.seq` Prelude.rnf modelPackageSummaries
      `Prelude.seq` Prelude.rnf httpStatus
