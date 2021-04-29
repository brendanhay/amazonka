{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Glacier.GetJobOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation downloads the output of the job you initiated using
-- InitiateJob. Depending on the job type you specified when you initiated
-- the job, the output will be either the content of an archive or a vault
-- inventory.
--
-- You can download all the job output or download a portion of the output
-- by specifying a byte range. In the case of an archive retrieval job,
-- depending on the byte range you specify, Amazon S3 Glacier (Glacier)
-- returns the checksum for the portion of the data. You can compute the
-- checksum on the client and verify that the values match to ensure the
-- portion you downloaded is the correct data.
--
-- A job ID will not expire for at least 24 hours after Glacier completes
-- the job. That a byte range. For both archive and inventory retrieval
-- jobs, you should verify the downloaded size against the size returned in
-- the headers from the __Get Job Output__ response.
--
-- For archive retrieval jobs, you should also verify that the size is what
-- you expected. If you download a portion of the output, the expected size
-- is based on the range of bytes you specified. For example, if you
-- specify a range of @bytes=0-1048575@, you should verify your download
-- size is 1,048,576 bytes. If you download an entire archive, the expected
-- size is the size of the archive when you uploaded it to Amazon S3
-- Glacier The expected size is also returned in the headers from the __Get
-- Job Output__ response.
--
-- In the case of an archive retrieval job, depending on the byte range you
-- specify, Glacier returns the checksum for the portion of the data. To
-- ensure the portion you downloaded is the correct data, compute the
-- checksum on the client, verify that the values match, and verify that
-- the size is what you expected.
--
-- A job ID does not expire for at least 24 hours after Glacier completes
-- the job. That is, you can download the job output within the 24 hours
-- period after Amazon Glacier completes the job.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don\'t have any
-- permissions by default. You must grant them explicit permission to
-- perform specific actions. For more information, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)>.
--
-- For conceptual information and the underlying REST API, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/vault-inventory.html Downloading a Vault Inventory>,
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/downloading-an-archive.html Downloading an Archive>,
-- and
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-job-output-get.html Get Job Output>
module Network.AWS.Glacier.GetJobOutput
  ( -- * Creating a Request
    GetJobOutput (..),
    newGetJobOutput,

    -- * Request Lenses
    getJobOutput_range,
    getJobOutput_accountId,
    getJobOutput_vaultName,
    getJobOutput_jobId,

    -- * Destructuring the Response
    GetJobOutputResponse (..),
    newGetJobOutputResponse,

    -- * Response Lenses
    getJobOutputResponse_contentType,
    getJobOutputResponse_contentRange,
    getJobOutputResponse_archiveDescription,
    getJobOutputResponse_acceptRanges,
    getJobOutputResponse_checksum,
    getJobOutputResponse_status,
    getJobOutputResponse_body,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Provides options for downloading output of an Amazon S3 Glacier job.
--
-- /See:/ 'newGetJobOutput' smart constructor.
data GetJobOutput = GetJobOutput'
  { -- | The range of bytes to retrieve from the output. For example, if you want
    -- to download the first 1,048,576 bytes, specify the range as
    -- @bytes=0-1048575@. By default, this operation downloads the entire
    -- output.
    --
    -- If the job output is large, then you can use a range to retrieve a
    -- portion of the output. This allows you to download the entire output in
    -- smaller chunks of bytes. For example, suppose you have 1 GB of job
    -- output you want to download and you decide to download 128 MB chunks of
    -- data at a time, which is a total of eight Get Job Output requests. You
    -- use the following process to download the job output:
    --
    -- 1.  Download a 128 MB chunk of output by specifying the appropriate byte
    --     range. Verify that all 128 MB of data was received.
    --
    -- 2.  Along with the data, the response includes a SHA256 tree hash of the
    --     payload. You compute the checksum of the payload on the client and
    --     compare it with the checksum you received in the response to ensure
    --     you received all the expected data.
    --
    -- 3.  Repeat steps 1 and 2 for all the eight 128 MB chunks of output data,
    --     each time specifying the appropriate byte range.
    --
    -- 4.  After downloading all the parts of the job output, you have a list
    --     of eight checksum values. Compute the tree hash of these values to
    --     find the checksum of the entire output. Using the DescribeJob API,
    --     obtain job information of the job that provided you the output. The
    --     response includes the checksum of the entire archive stored in
    --     Amazon S3 Glacier. You compare this value with the checksum you
    --     computed to ensure you have downloaded the entire archive content
    --     with no errors.
    range :: Prelude.Maybe Prelude.Text,
    -- | The @AccountId@ value is the AWS account ID of the account that owns the
    -- vault. You can either specify an AWS account ID or optionally a single
    -- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
    -- ID associated with the credentials used to sign the request. If you use
    -- an account ID, do not include any hyphens (\'-\') in the ID.
    accountId :: Prelude.Text,
    -- | The name of the vault.
    vaultName :: Prelude.Text,
    -- | The job ID whose data is downloaded.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetJobOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'range', 'getJobOutput_range' - The range of bytes to retrieve from the output. For example, if you want
-- to download the first 1,048,576 bytes, specify the range as
-- @bytes=0-1048575@. By default, this operation downloads the entire
-- output.
--
-- If the job output is large, then you can use a range to retrieve a
-- portion of the output. This allows you to download the entire output in
-- smaller chunks of bytes. For example, suppose you have 1 GB of job
-- output you want to download and you decide to download 128 MB chunks of
-- data at a time, which is a total of eight Get Job Output requests. You
-- use the following process to download the job output:
--
-- 1.  Download a 128 MB chunk of output by specifying the appropriate byte
--     range. Verify that all 128 MB of data was received.
--
-- 2.  Along with the data, the response includes a SHA256 tree hash of the
--     payload. You compute the checksum of the payload on the client and
--     compare it with the checksum you received in the response to ensure
--     you received all the expected data.
--
-- 3.  Repeat steps 1 and 2 for all the eight 128 MB chunks of output data,
--     each time specifying the appropriate byte range.
--
-- 4.  After downloading all the parts of the job output, you have a list
--     of eight checksum values. Compute the tree hash of these values to
--     find the checksum of the entire output. Using the DescribeJob API,
--     obtain job information of the job that provided you the output. The
--     response includes the checksum of the entire archive stored in
--     Amazon S3 Glacier. You compare this value with the checksum you
--     computed to ensure you have downloaded the entire archive content
--     with no errors.
--
-- 'accountId', 'getJobOutput_accountId' - The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
--
-- 'vaultName', 'getJobOutput_vaultName' - The name of the vault.
--
-- 'jobId', 'getJobOutput_jobId' - The job ID whose data is downloaded.
newGetJobOutput ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'vaultName'
  Prelude.Text ->
  -- | 'jobId'
  Prelude.Text ->
  GetJobOutput
newGetJobOutput pAccountId_ pVaultName_ pJobId_ =
  GetJobOutput'
    { range = Prelude.Nothing,
      accountId = pAccountId_,
      vaultName = pVaultName_,
      jobId = pJobId_
    }

-- | The range of bytes to retrieve from the output. For example, if you want
-- to download the first 1,048,576 bytes, specify the range as
-- @bytes=0-1048575@. By default, this operation downloads the entire
-- output.
--
-- If the job output is large, then you can use a range to retrieve a
-- portion of the output. This allows you to download the entire output in
-- smaller chunks of bytes. For example, suppose you have 1 GB of job
-- output you want to download and you decide to download 128 MB chunks of
-- data at a time, which is a total of eight Get Job Output requests. You
-- use the following process to download the job output:
--
-- 1.  Download a 128 MB chunk of output by specifying the appropriate byte
--     range. Verify that all 128 MB of data was received.
--
-- 2.  Along with the data, the response includes a SHA256 tree hash of the
--     payload. You compute the checksum of the payload on the client and
--     compare it with the checksum you received in the response to ensure
--     you received all the expected data.
--
-- 3.  Repeat steps 1 and 2 for all the eight 128 MB chunks of output data,
--     each time specifying the appropriate byte range.
--
-- 4.  After downloading all the parts of the job output, you have a list
--     of eight checksum values. Compute the tree hash of these values to
--     find the checksum of the entire output. Using the DescribeJob API,
--     obtain job information of the job that provided you the output. The
--     response includes the checksum of the entire archive stored in
--     Amazon S3 Glacier. You compare this value with the checksum you
--     computed to ensure you have downloaded the entire archive content
--     with no errors.
getJobOutput_range :: Lens.Lens' GetJobOutput (Prelude.Maybe Prelude.Text)
getJobOutput_range = Lens.lens (\GetJobOutput' {range} -> range) (\s@GetJobOutput' {} a -> s {range = a} :: GetJobOutput)

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
getJobOutput_accountId :: Lens.Lens' GetJobOutput Prelude.Text
getJobOutput_accountId = Lens.lens (\GetJobOutput' {accountId} -> accountId) (\s@GetJobOutput' {} a -> s {accountId = a} :: GetJobOutput)

-- | The name of the vault.
getJobOutput_vaultName :: Lens.Lens' GetJobOutput Prelude.Text
getJobOutput_vaultName = Lens.lens (\GetJobOutput' {vaultName} -> vaultName) (\s@GetJobOutput' {} a -> s {vaultName = a} :: GetJobOutput)

-- | The job ID whose data is downloaded.
getJobOutput_jobId :: Lens.Lens' GetJobOutput Prelude.Text
getJobOutput_jobId = Lens.lens (\GetJobOutput' {jobId} -> jobId) (\s@GetJobOutput' {} a -> s {jobId = a} :: GetJobOutput)

instance Prelude.AWSRequest GetJobOutput where
  type Rs GetJobOutput = GetJobOutputResponse
  request = Request.get defaultService
  response =
    Response.receiveBody
      ( \s h x ->
          GetJobOutputResponse'
            Prelude.<$> (h Prelude..#? "Content-Type")
            Prelude.<*> (h Prelude..#? "Content-Range")
            Prelude.<*> (h Prelude..#? "x-amz-archive-description")
            Prelude.<*> (h Prelude..#? "Accept-Ranges")
            Prelude.<*> (h Prelude..#? "x-amz-sha256-tree-hash")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.pure x)
      )

instance Prelude.Hashable GetJobOutput

instance Prelude.NFData GetJobOutput

instance Prelude.ToHeaders GetJobOutput where
  toHeaders GetJobOutput' {..} =
    Prelude.mconcat ["Range" Prelude.=# range]

instance Prelude.ToPath GetJobOutput where
  toPath GetJobOutput' {..} =
    Prelude.mconcat
      [ "/",
        Prelude.toBS accountId,
        "/vaults/",
        Prelude.toBS vaultName,
        "/jobs/",
        Prelude.toBS jobId,
        "/output"
      ]

instance Prelude.ToQuery GetJobOutput where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'newGetJobOutputResponse' smart constructor.
data GetJobOutputResponse = GetJobOutputResponse'
  { -- | The Content-Type depends on whether the job output is an archive or a
    -- vault inventory. For archive data, the Content-Type is
    -- application\/octet-stream. For vault inventory, if you requested CSV
    -- format when you initiated the job, the Content-Type is text\/csv.
    -- Otherwise, by default, vault inventory is returned as JSON, and the
    -- Content-Type is application\/json.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The range of bytes returned by Amazon S3 Glacier. If only partial output
    -- is downloaded, the response provides the range of bytes Amazon S3
    -- Glacier returned. For example, bytes 0-1048575\/8388608 returns the
    -- first 1 MB from 8 MB.
    contentRange :: Prelude.Maybe Prelude.Text,
    -- | The description of an archive.
    archiveDescription :: Prelude.Maybe Prelude.Text,
    -- | Indicates the range units accepted. For more information, see
    -- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html RFC2616>.
    acceptRanges :: Prelude.Maybe Prelude.Text,
    -- | The checksum of the data in the response. This header is returned only
    -- when retrieving the output for an archive retrieval job. Furthermore,
    -- this header appears only under the following conditions:
    --
    -- -   You get the entire range of the archive.
    --
    -- -   You request a range to return of the archive that starts and ends on
    --     a multiple of 1 MB. For example, if you have an 3.1 MB archive and
    --     you specify a range to return that starts at 1 MB and ends at 2 MB,
    --     then the x-amz-sha256-tree-hash is returned as a response header.
    --
    -- -   You request a range of the archive to return that starts on a
    --     multiple of 1 MB and goes to the end of the archive. For example, if
    --     you have a 3.1 MB archive and you specify a range that starts at 2
    --     MB and ends at 3.1 MB (the end of the archive), then the
    --     x-amz-sha256-tree-hash is returned as a response header.
    checksum :: Prelude.Maybe Prelude.Text,
    -- | The HTTP response code for a job output request. The value depends on
    -- whether a range was specified in the request.
    status :: Prelude.Int,
    -- | The job data, either archive data or inventory data.
    body :: Prelude.RsBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetJobOutputResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'getJobOutputResponse_contentType' - The Content-Type depends on whether the job output is an archive or a
-- vault inventory. For archive data, the Content-Type is
-- application\/octet-stream. For vault inventory, if you requested CSV
-- format when you initiated the job, the Content-Type is text\/csv.
-- Otherwise, by default, vault inventory is returned as JSON, and the
-- Content-Type is application\/json.
--
-- 'contentRange', 'getJobOutputResponse_contentRange' - The range of bytes returned by Amazon S3 Glacier. If only partial output
-- is downloaded, the response provides the range of bytes Amazon S3
-- Glacier returned. For example, bytes 0-1048575\/8388608 returns the
-- first 1 MB from 8 MB.
--
-- 'archiveDescription', 'getJobOutputResponse_archiveDescription' - The description of an archive.
--
-- 'acceptRanges', 'getJobOutputResponse_acceptRanges' - Indicates the range units accepted. For more information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html RFC2616>.
--
-- 'checksum', 'getJobOutputResponse_checksum' - The checksum of the data in the response. This header is returned only
-- when retrieving the output for an archive retrieval job. Furthermore,
-- this header appears only under the following conditions:
--
-- -   You get the entire range of the archive.
--
-- -   You request a range to return of the archive that starts and ends on
--     a multiple of 1 MB. For example, if you have an 3.1 MB archive and
--     you specify a range to return that starts at 1 MB and ends at 2 MB,
--     then the x-amz-sha256-tree-hash is returned as a response header.
--
-- -   You request a range of the archive to return that starts on a
--     multiple of 1 MB and goes to the end of the archive. For example, if
--     you have a 3.1 MB archive and you specify a range that starts at 2
--     MB and ends at 3.1 MB (the end of the archive), then the
--     x-amz-sha256-tree-hash is returned as a response header.
--
-- 'status', 'getJobOutputResponse_status' - The HTTP response code for a job output request. The value depends on
-- whether a range was specified in the request.
--
-- 'body', 'getJobOutputResponse_body' - The job data, either archive data or inventory data.
newGetJobOutputResponse ::
  -- | 'status'
  Prelude.Int ->
  -- | 'body'
  Prelude.RsBody ->
  GetJobOutputResponse
newGetJobOutputResponse pStatus_ pBody_ =
  GetJobOutputResponse'
    { contentType =
        Prelude.Nothing,
      contentRange = Prelude.Nothing,
      archiveDescription = Prelude.Nothing,
      acceptRanges = Prelude.Nothing,
      checksum = Prelude.Nothing,
      status = pStatus_,
      body = pBody_
    }

-- | The Content-Type depends on whether the job output is an archive or a
-- vault inventory. For archive data, the Content-Type is
-- application\/octet-stream. For vault inventory, if you requested CSV
-- format when you initiated the job, the Content-Type is text\/csv.
-- Otherwise, by default, vault inventory is returned as JSON, and the
-- Content-Type is application\/json.
getJobOutputResponse_contentType :: Lens.Lens' GetJobOutputResponse (Prelude.Maybe Prelude.Text)
getJobOutputResponse_contentType = Lens.lens (\GetJobOutputResponse' {contentType} -> contentType) (\s@GetJobOutputResponse' {} a -> s {contentType = a} :: GetJobOutputResponse)

-- | The range of bytes returned by Amazon S3 Glacier. If only partial output
-- is downloaded, the response provides the range of bytes Amazon S3
-- Glacier returned. For example, bytes 0-1048575\/8388608 returns the
-- first 1 MB from 8 MB.
getJobOutputResponse_contentRange :: Lens.Lens' GetJobOutputResponse (Prelude.Maybe Prelude.Text)
getJobOutputResponse_contentRange = Lens.lens (\GetJobOutputResponse' {contentRange} -> contentRange) (\s@GetJobOutputResponse' {} a -> s {contentRange = a} :: GetJobOutputResponse)

-- | The description of an archive.
getJobOutputResponse_archiveDescription :: Lens.Lens' GetJobOutputResponse (Prelude.Maybe Prelude.Text)
getJobOutputResponse_archiveDescription = Lens.lens (\GetJobOutputResponse' {archiveDescription} -> archiveDescription) (\s@GetJobOutputResponse' {} a -> s {archiveDescription = a} :: GetJobOutputResponse)

-- | Indicates the range units accepted. For more information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html RFC2616>.
getJobOutputResponse_acceptRanges :: Lens.Lens' GetJobOutputResponse (Prelude.Maybe Prelude.Text)
getJobOutputResponse_acceptRanges = Lens.lens (\GetJobOutputResponse' {acceptRanges} -> acceptRanges) (\s@GetJobOutputResponse' {} a -> s {acceptRanges = a} :: GetJobOutputResponse)

-- | The checksum of the data in the response. This header is returned only
-- when retrieving the output for an archive retrieval job. Furthermore,
-- this header appears only under the following conditions:
--
-- -   You get the entire range of the archive.
--
-- -   You request a range to return of the archive that starts and ends on
--     a multiple of 1 MB. For example, if you have an 3.1 MB archive and
--     you specify a range to return that starts at 1 MB and ends at 2 MB,
--     then the x-amz-sha256-tree-hash is returned as a response header.
--
-- -   You request a range of the archive to return that starts on a
--     multiple of 1 MB and goes to the end of the archive. For example, if
--     you have a 3.1 MB archive and you specify a range that starts at 2
--     MB and ends at 3.1 MB (the end of the archive), then the
--     x-amz-sha256-tree-hash is returned as a response header.
getJobOutputResponse_checksum :: Lens.Lens' GetJobOutputResponse (Prelude.Maybe Prelude.Text)
getJobOutputResponse_checksum = Lens.lens (\GetJobOutputResponse' {checksum} -> checksum) (\s@GetJobOutputResponse' {} a -> s {checksum = a} :: GetJobOutputResponse)

-- | The HTTP response code for a job output request. The value depends on
-- whether a range was specified in the request.
getJobOutputResponse_status :: Lens.Lens' GetJobOutputResponse Prelude.Int
getJobOutputResponse_status = Lens.lens (\GetJobOutputResponse' {status} -> status) (\s@GetJobOutputResponse' {} a -> s {status = a} :: GetJobOutputResponse)

-- | The job data, either archive data or inventory data.
getJobOutputResponse_body :: Lens.Lens' GetJobOutputResponse Prelude.RsBody
getJobOutputResponse_body = Lens.lens (\GetJobOutputResponse' {body} -> body) (\s@GetJobOutputResponse' {} a -> s {body = a} :: GetJobOutputResponse)
