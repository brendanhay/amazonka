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
-- Module      : Amazonka.DynamoDB.ImportTable
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports table data from an S3 bucket.
module Amazonka.DynamoDB.ImportTable
  ( -- * Creating a Request
    ImportTable (..),
    newImportTable,

    -- * Request Lenses
    importTable_clientToken,
    importTable_inputCompressionType,
    importTable_inputFormatOptions,
    importTable_s3BucketSource,
    importTable_inputFormat,
    importTable_tableCreationParameters,

    -- * Destructuring the Response
    ImportTableResponse (..),
    newImportTableResponse,

    -- * Response Lenses
    importTableResponse_httpStatus,
    importTableResponse_importTableDescription,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newImportTable' smart constructor.
data ImportTable = ImportTable'
  { -- | Providing a @ClientToken@ makes the call to @ImportTableInput@
    -- idempotent, meaning that multiple identical calls have the same effect
    -- as one single call.
    --
    -- A client token is valid for 8 hours after the first request that uses it
    -- is completed. After 8 hours, any request with the same client token is
    -- treated as a new request. Do not resubmit the same request with the same
    -- client token for more than 8 hours, or the result might not be
    -- idempotent.
    --
    -- If you submit a request with the same client token but a change in other
    -- parameters within the 8-hour idempotency window, DynamoDB returns an
    -- @IdempotentParameterMismatch@ exception.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Type of compression to be used on the input coming from the imported
    -- table.
    inputCompressionType :: Prelude.Maybe InputCompressionType,
    -- | Additional properties that specify how the input is formatted,
    inputFormatOptions :: Prelude.Maybe InputFormatOptions,
    -- | The S3 bucket that provides the source for the import.
    s3BucketSource :: S3BucketSource,
    -- | The format of the source data. Valid values for @ImportFormat@ are
    -- @CSV@, @DYNAMODB_JSON@ or @ION@.
    inputFormat :: InputFormat,
    -- | Parameters for the table to import the data into.
    tableCreationParameters :: TableCreationParameters
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'importTable_clientToken' - Providing a @ClientToken@ makes the call to @ImportTableInput@
-- idempotent, meaning that multiple identical calls have the same effect
-- as one single call.
--
-- A client token is valid for 8 hours after the first request that uses it
-- is completed. After 8 hours, any request with the same client token is
-- treated as a new request. Do not resubmit the same request with the same
-- client token for more than 8 hours, or the result might not be
-- idempotent.
--
-- If you submit a request with the same client token but a change in other
-- parameters within the 8-hour idempotency window, DynamoDB returns an
-- @IdempotentParameterMismatch@ exception.
--
-- 'inputCompressionType', 'importTable_inputCompressionType' - Type of compression to be used on the input coming from the imported
-- table.
--
-- 'inputFormatOptions', 'importTable_inputFormatOptions' - Additional properties that specify how the input is formatted,
--
-- 's3BucketSource', 'importTable_s3BucketSource' - The S3 bucket that provides the source for the import.
--
-- 'inputFormat', 'importTable_inputFormat' - The format of the source data. Valid values for @ImportFormat@ are
-- @CSV@, @DYNAMODB_JSON@ or @ION@.
--
-- 'tableCreationParameters', 'importTable_tableCreationParameters' - Parameters for the table to import the data into.
newImportTable ::
  -- | 's3BucketSource'
  S3BucketSource ->
  -- | 'inputFormat'
  InputFormat ->
  -- | 'tableCreationParameters'
  TableCreationParameters ->
  ImportTable
newImportTable
  pS3BucketSource_
  pInputFormat_
  pTableCreationParameters_ =
    ImportTable'
      { clientToken = Prelude.Nothing,
        inputCompressionType = Prelude.Nothing,
        inputFormatOptions = Prelude.Nothing,
        s3BucketSource = pS3BucketSource_,
        inputFormat = pInputFormat_,
        tableCreationParameters = pTableCreationParameters_
      }

-- | Providing a @ClientToken@ makes the call to @ImportTableInput@
-- idempotent, meaning that multiple identical calls have the same effect
-- as one single call.
--
-- A client token is valid for 8 hours after the first request that uses it
-- is completed. After 8 hours, any request with the same client token is
-- treated as a new request. Do not resubmit the same request with the same
-- client token for more than 8 hours, or the result might not be
-- idempotent.
--
-- If you submit a request with the same client token but a change in other
-- parameters within the 8-hour idempotency window, DynamoDB returns an
-- @IdempotentParameterMismatch@ exception.
importTable_clientToken :: Lens.Lens' ImportTable (Prelude.Maybe Prelude.Text)
importTable_clientToken = Lens.lens (\ImportTable' {clientToken} -> clientToken) (\s@ImportTable' {} a -> s {clientToken = a} :: ImportTable)

-- | Type of compression to be used on the input coming from the imported
-- table.
importTable_inputCompressionType :: Lens.Lens' ImportTable (Prelude.Maybe InputCompressionType)
importTable_inputCompressionType = Lens.lens (\ImportTable' {inputCompressionType} -> inputCompressionType) (\s@ImportTable' {} a -> s {inputCompressionType = a} :: ImportTable)

-- | Additional properties that specify how the input is formatted,
importTable_inputFormatOptions :: Lens.Lens' ImportTable (Prelude.Maybe InputFormatOptions)
importTable_inputFormatOptions = Lens.lens (\ImportTable' {inputFormatOptions} -> inputFormatOptions) (\s@ImportTable' {} a -> s {inputFormatOptions = a} :: ImportTable)

-- | The S3 bucket that provides the source for the import.
importTable_s3BucketSource :: Lens.Lens' ImportTable S3BucketSource
importTable_s3BucketSource = Lens.lens (\ImportTable' {s3BucketSource} -> s3BucketSource) (\s@ImportTable' {} a -> s {s3BucketSource = a} :: ImportTable)

-- | The format of the source data. Valid values for @ImportFormat@ are
-- @CSV@, @DYNAMODB_JSON@ or @ION@.
importTable_inputFormat :: Lens.Lens' ImportTable InputFormat
importTable_inputFormat = Lens.lens (\ImportTable' {inputFormat} -> inputFormat) (\s@ImportTable' {} a -> s {inputFormat = a} :: ImportTable)

-- | Parameters for the table to import the data into.
importTable_tableCreationParameters :: Lens.Lens' ImportTable TableCreationParameters
importTable_tableCreationParameters = Lens.lens (\ImportTable' {tableCreationParameters} -> tableCreationParameters) (\s@ImportTable' {} a -> s {tableCreationParameters = a} :: ImportTable)

instance Core.AWSRequest ImportTable where
  type AWSResponse ImportTable = ImportTableResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportTableResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ImportTableDescription")
      )

instance Prelude.Hashable ImportTable where
  hashWithSalt _salt ImportTable' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` inputCompressionType
      `Prelude.hashWithSalt` inputFormatOptions
      `Prelude.hashWithSalt` s3BucketSource
      `Prelude.hashWithSalt` inputFormat
      `Prelude.hashWithSalt` tableCreationParameters

instance Prelude.NFData ImportTable where
  rnf ImportTable' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf inputCompressionType
      `Prelude.seq` Prelude.rnf inputFormatOptions
      `Prelude.seq` Prelude.rnf s3BucketSource
      `Prelude.seq` Prelude.rnf inputFormat
      `Prelude.seq` Prelude.rnf tableCreationParameters

instance Data.ToHeaders ImportTable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDB_20120810.ImportTable" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ImportTable where
  toJSON ImportTable' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("InputCompressionType" Data..=)
              Prelude.<$> inputCompressionType,
            ("InputFormatOptions" Data..=)
              Prelude.<$> inputFormatOptions,
            Prelude.Just
              ("S3BucketSource" Data..= s3BucketSource),
            Prelude.Just ("InputFormat" Data..= inputFormat),
            Prelude.Just
              ( "TableCreationParameters"
                  Data..= tableCreationParameters
              )
          ]
      )

instance Data.ToPath ImportTable where
  toPath = Prelude.const "/"

instance Data.ToQuery ImportTable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportTableResponse' smart constructor.
data ImportTableResponse = ImportTableResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Represents the properties of the table created for the import, and
    -- parameters of the import. The import parameters include import status,
    -- how many items were processed, and how many errors were encountered.
    importTableDescription :: ImportTableDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'importTableResponse_httpStatus' - The response's http status code.
--
-- 'importTableDescription', 'importTableResponse_importTableDescription' - Represents the properties of the table created for the import, and
-- parameters of the import. The import parameters include import status,
-- how many items were processed, and how many errors were encountered.
newImportTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'importTableDescription'
  ImportTableDescription ->
  ImportTableResponse
newImportTableResponse
  pHttpStatus_
  pImportTableDescription_ =
    ImportTableResponse'
      { httpStatus = pHttpStatus_,
        importTableDescription = pImportTableDescription_
      }

-- | The response's http status code.
importTableResponse_httpStatus :: Lens.Lens' ImportTableResponse Prelude.Int
importTableResponse_httpStatus = Lens.lens (\ImportTableResponse' {httpStatus} -> httpStatus) (\s@ImportTableResponse' {} a -> s {httpStatus = a} :: ImportTableResponse)

-- | Represents the properties of the table created for the import, and
-- parameters of the import. The import parameters include import status,
-- how many items were processed, and how many errors were encountered.
importTableResponse_importTableDescription :: Lens.Lens' ImportTableResponse ImportTableDescription
importTableResponse_importTableDescription = Lens.lens (\ImportTableResponse' {importTableDescription} -> importTableDescription) (\s@ImportTableResponse' {} a -> s {importTableDescription = a} :: ImportTableResponse)

instance Prelude.NFData ImportTableResponse where
  rnf ImportTableResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf importTableDescription
