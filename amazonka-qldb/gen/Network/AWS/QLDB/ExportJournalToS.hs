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
-- Module      : Network.AWS.QLDB.ExportJournalToS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports journal contents within a date and time range from a ledger into
-- a specified Amazon Simple Storage Service (Amazon S3) bucket. The data
-- is written as files in Amazon Ion format.
--
-- If the ledger with the given @Name@ doesn\'t exist, then throws
-- @ResourceNotFoundException@.
--
-- If the ledger with the given @Name@ is in @CREATING@ status, then throws
-- @ResourcePreconditionNotMetException@.
--
-- You can initiate up to two concurrent journal export requests for each
-- ledger. Beyond this limit, journal export requests throw
-- @LimitExceededException@.
module Network.AWS.QLDB.ExportJournalToS
  ( -- * Creating a Request
    ExportJournalToS (..),
    newExportJournalToS,

    -- * Request Lenses
    exportJournalToS_name,
    exportJournalToS_inclusiveStartTime,
    exportJournalToS_exclusiveEndTime,
    exportJournalToS_s3ExportConfiguration,
    exportJournalToS_roleArn,

    -- * Destructuring the Response
    ExportJournalToSResponse (..),
    newExportJournalToSResponse,

    -- * Response Lenses
    exportJournalToSResponse_httpStatus,
    exportJournalToSResponse_exportId,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.QLDB.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newExportJournalToS' smart constructor.
data ExportJournalToS = ExportJournalToS'
  { -- | The name of the ledger.
    name :: Prelude.Text,
    -- | The inclusive start date and time for the range of journal contents that
    -- you want to export.
    --
    -- The @InclusiveStartTime@ must be in @ISO 8601@ date and time format and
    -- in Universal Coordinated Time (UTC). For example: @2019-06-13T21:36:34Z@
    --
    -- The @InclusiveStartTime@ must be before @ExclusiveEndTime@.
    --
    -- If you provide an @InclusiveStartTime@ that is before the ledger\'s
    -- @CreationDateTime@, Amazon QLDB defaults it to the ledger\'s
    -- @CreationDateTime@.
    inclusiveStartTime :: Core.POSIX,
    -- | The exclusive end date and time for the range of journal contents that
    -- you want to export.
    --
    -- The @ExclusiveEndTime@ must be in @ISO 8601@ date and time format and in
    -- Universal Coordinated Time (UTC). For example: @2019-06-13T21:36:34Z@
    --
    -- The @ExclusiveEndTime@ must be less than or equal to the current UTC
    -- date and time.
    exclusiveEndTime :: Core.POSIX,
    -- | The configuration settings of the Amazon S3 bucket destination for your
    -- export request.
    s3ExportConfiguration :: S3ExportConfiguration,
    -- | The Amazon Resource Name (ARN) of the IAM role that grants QLDB
    -- permissions for a journal export job to do the following:
    --
    -- -   Write objects into your Amazon Simple Storage Service (Amazon S3)
    --     bucket.
    --
    -- -   (Optional) Use your customer master key (CMK) in AWS Key Management
    --     Service (AWS KMS) for server-side encryption of your exported data.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportJournalToS' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'exportJournalToS_name' - The name of the ledger.
--
-- 'inclusiveStartTime', 'exportJournalToS_inclusiveStartTime' - The inclusive start date and time for the range of journal contents that
-- you want to export.
--
-- The @InclusiveStartTime@ must be in @ISO 8601@ date and time format and
-- in Universal Coordinated Time (UTC). For example: @2019-06-13T21:36:34Z@
--
-- The @InclusiveStartTime@ must be before @ExclusiveEndTime@.
--
-- If you provide an @InclusiveStartTime@ that is before the ledger\'s
-- @CreationDateTime@, Amazon QLDB defaults it to the ledger\'s
-- @CreationDateTime@.
--
-- 'exclusiveEndTime', 'exportJournalToS_exclusiveEndTime' - The exclusive end date and time for the range of journal contents that
-- you want to export.
--
-- The @ExclusiveEndTime@ must be in @ISO 8601@ date and time format and in
-- Universal Coordinated Time (UTC). For example: @2019-06-13T21:36:34Z@
--
-- The @ExclusiveEndTime@ must be less than or equal to the current UTC
-- date and time.
--
-- 's3ExportConfiguration', 'exportJournalToS_s3ExportConfiguration' - The configuration settings of the Amazon S3 bucket destination for your
-- export request.
--
-- 'roleArn', 'exportJournalToS_roleArn' - The Amazon Resource Name (ARN) of the IAM role that grants QLDB
-- permissions for a journal export job to do the following:
--
-- -   Write objects into your Amazon Simple Storage Service (Amazon S3)
--     bucket.
--
-- -   (Optional) Use your customer master key (CMK) in AWS Key Management
--     Service (AWS KMS) for server-side encryption of your exported data.
newExportJournalToS ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inclusiveStartTime'
  Prelude.UTCTime ->
  -- | 'exclusiveEndTime'
  Prelude.UTCTime ->
  -- | 's3ExportConfiguration'
  S3ExportConfiguration ->
  -- | 'roleArn'
  Prelude.Text ->
  ExportJournalToS
newExportJournalToS
  pName_
  pInclusiveStartTime_
  pExclusiveEndTime_
  pS3ExportConfiguration_
  pRoleArn_ =
    ExportJournalToS'
      { name = pName_,
        inclusiveStartTime =
          Core._Time Lens.# pInclusiveStartTime_,
        exclusiveEndTime =
          Core._Time Lens.# pExclusiveEndTime_,
        s3ExportConfiguration = pS3ExportConfiguration_,
        roleArn = pRoleArn_
      }

-- | The name of the ledger.
exportJournalToS_name :: Lens.Lens' ExportJournalToS Prelude.Text
exportJournalToS_name = Lens.lens (\ExportJournalToS' {name} -> name) (\s@ExportJournalToS' {} a -> s {name = a} :: ExportJournalToS)

-- | The inclusive start date and time for the range of journal contents that
-- you want to export.
--
-- The @InclusiveStartTime@ must be in @ISO 8601@ date and time format and
-- in Universal Coordinated Time (UTC). For example: @2019-06-13T21:36:34Z@
--
-- The @InclusiveStartTime@ must be before @ExclusiveEndTime@.
--
-- If you provide an @InclusiveStartTime@ that is before the ledger\'s
-- @CreationDateTime@, Amazon QLDB defaults it to the ledger\'s
-- @CreationDateTime@.
exportJournalToS_inclusiveStartTime :: Lens.Lens' ExportJournalToS Prelude.UTCTime
exportJournalToS_inclusiveStartTime = Lens.lens (\ExportJournalToS' {inclusiveStartTime} -> inclusiveStartTime) (\s@ExportJournalToS' {} a -> s {inclusiveStartTime = a} :: ExportJournalToS) Prelude.. Core._Time

-- | The exclusive end date and time for the range of journal contents that
-- you want to export.
--
-- The @ExclusiveEndTime@ must be in @ISO 8601@ date and time format and in
-- Universal Coordinated Time (UTC). For example: @2019-06-13T21:36:34Z@
--
-- The @ExclusiveEndTime@ must be less than or equal to the current UTC
-- date and time.
exportJournalToS_exclusiveEndTime :: Lens.Lens' ExportJournalToS Prelude.UTCTime
exportJournalToS_exclusiveEndTime = Lens.lens (\ExportJournalToS' {exclusiveEndTime} -> exclusiveEndTime) (\s@ExportJournalToS' {} a -> s {exclusiveEndTime = a} :: ExportJournalToS) Prelude.. Core._Time

-- | The configuration settings of the Amazon S3 bucket destination for your
-- export request.
exportJournalToS_s3ExportConfiguration :: Lens.Lens' ExportJournalToS S3ExportConfiguration
exportJournalToS_s3ExportConfiguration = Lens.lens (\ExportJournalToS' {s3ExportConfiguration} -> s3ExportConfiguration) (\s@ExportJournalToS' {} a -> s {s3ExportConfiguration = a} :: ExportJournalToS)

-- | The Amazon Resource Name (ARN) of the IAM role that grants QLDB
-- permissions for a journal export job to do the following:
--
-- -   Write objects into your Amazon Simple Storage Service (Amazon S3)
--     bucket.
--
-- -   (Optional) Use your customer master key (CMK) in AWS Key Management
--     Service (AWS KMS) for server-side encryption of your exported data.
exportJournalToS_roleArn :: Lens.Lens' ExportJournalToS Prelude.Text
exportJournalToS_roleArn = Lens.lens (\ExportJournalToS' {roleArn} -> roleArn) (\s@ExportJournalToS' {} a -> s {roleArn = a} :: ExportJournalToS)

instance Core.AWSRequest ExportJournalToS where
  type
    AWSResponse ExportJournalToS =
      ExportJournalToSResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ExportJournalToSResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "ExportId")
      )

instance Prelude.Hashable ExportJournalToS

instance Prelude.NFData ExportJournalToS

instance Core.ToHeaders ExportJournalToS where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ExportJournalToS where
  toJSON ExportJournalToS' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("InclusiveStartTime" Core..= inclusiveStartTime),
            Prelude.Just
              ("ExclusiveEndTime" Core..= exclusiveEndTime),
            Prelude.Just
              ( "S3ExportConfiguration"
                  Core..= s3ExportConfiguration
              ),
            Prelude.Just ("RoleArn" Core..= roleArn)
          ]
      )

instance Core.ToPath ExportJournalToS where
  toPath ExportJournalToS' {..} =
    Prelude.mconcat
      ["/ledgers/", Core.toBS name, "/journal-s3-exports"]

instance Core.ToQuery ExportJournalToS where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExportJournalToSResponse' smart constructor.
data ExportJournalToSResponse = ExportJournalToSResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique ID that QLDB assigns to each journal export job.
    --
    -- To describe your export request and check the status of the job, you can
    -- use @ExportId@ to call @DescribeJournalS3Export@.
    exportId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportJournalToSResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'exportJournalToSResponse_httpStatus' - The response's http status code.
--
-- 'exportId', 'exportJournalToSResponse_exportId' - The unique ID that QLDB assigns to each journal export job.
--
-- To describe your export request and check the status of the job, you can
-- use @ExportId@ to call @DescribeJournalS3Export@.
newExportJournalToSResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'exportId'
  Prelude.Text ->
  ExportJournalToSResponse
newExportJournalToSResponse pHttpStatus_ pExportId_ =
  ExportJournalToSResponse'
    { httpStatus =
        pHttpStatus_,
      exportId = pExportId_
    }

-- | The response's http status code.
exportJournalToSResponse_httpStatus :: Lens.Lens' ExportJournalToSResponse Prelude.Int
exportJournalToSResponse_httpStatus = Lens.lens (\ExportJournalToSResponse' {httpStatus} -> httpStatus) (\s@ExportJournalToSResponse' {} a -> s {httpStatus = a} :: ExportJournalToSResponse)

-- | The unique ID that QLDB assigns to each journal export job.
--
-- To describe your export request and check the status of the job, you can
-- use @ExportId@ to call @DescribeJournalS3Export@.
exportJournalToSResponse_exportId :: Lens.Lens' ExportJournalToSResponse Prelude.Text
exportJournalToSResponse_exportId = Lens.lens (\ExportJournalToSResponse' {exportId} -> exportId) (\s@ExportJournalToSResponse' {} a -> s {exportId = a} :: ExportJournalToSResponse)

instance Prelude.NFData ExportJournalToSResponse
