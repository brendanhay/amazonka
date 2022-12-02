{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMaker.Types.BatchDescribeModelPackageError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.BatchDescribeModelPackageError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The error code and error description associated with the resource.
--
-- /See:/ 'newBatchDescribeModelPackageError' smart constructor.
data BatchDescribeModelPackageError = BatchDescribeModelPackageError'
  { errorCode :: Prelude.Text,
    errorResponse :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDescribeModelPackageError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'batchDescribeModelPackageError_errorCode' -
--
-- 'errorResponse', 'batchDescribeModelPackageError_errorResponse' -
newBatchDescribeModelPackageError ::
  -- | 'errorCode'
  Prelude.Text ->
  -- | 'errorResponse'
  Prelude.Text ->
  BatchDescribeModelPackageError
newBatchDescribeModelPackageError
  pErrorCode_
  pErrorResponse_ =
    BatchDescribeModelPackageError'
      { errorCode =
          pErrorCode_,
        errorResponse = pErrorResponse_
      }

-- |
batchDescribeModelPackageError_errorCode :: Lens.Lens' BatchDescribeModelPackageError Prelude.Text
batchDescribeModelPackageError_errorCode = Lens.lens (\BatchDescribeModelPackageError' {errorCode} -> errorCode) (\s@BatchDescribeModelPackageError' {} a -> s {errorCode = a} :: BatchDescribeModelPackageError)

-- |
batchDescribeModelPackageError_errorResponse :: Lens.Lens' BatchDescribeModelPackageError Prelude.Text
batchDescribeModelPackageError_errorResponse = Lens.lens (\BatchDescribeModelPackageError' {errorResponse} -> errorResponse) (\s@BatchDescribeModelPackageError' {} a -> s {errorResponse = a} :: BatchDescribeModelPackageError)

instance Data.FromJSON BatchDescribeModelPackageError where
  parseJSON =
    Data.withObject
      "BatchDescribeModelPackageError"
      ( \x ->
          BatchDescribeModelPackageError'
            Prelude.<$> (x Data..: "ErrorCode")
            Prelude.<*> (x Data..: "ErrorResponse")
      )

instance
  Prelude.Hashable
    BatchDescribeModelPackageError
  where
  hashWithSalt
    _salt
    BatchDescribeModelPackageError' {..} =
      _salt `Prelude.hashWithSalt` errorCode
        `Prelude.hashWithSalt` errorResponse

instance
  Prelude.NFData
    BatchDescribeModelPackageError
  where
  rnf BatchDescribeModelPackageError' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorResponse
