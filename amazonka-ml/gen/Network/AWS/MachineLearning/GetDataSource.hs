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
-- Module      : Network.AWS.MachineLearning.GetDataSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a @DataSource@ that includes metadata and data file information,
-- as well as the current status of the @DataSource@.
--
-- @GetDataSource@ provides results in normal or verbose format. The
-- verbose format adds the schema description and the list of files pointed
-- to by the DataSource to the normal format.
module Network.AWS.MachineLearning.GetDataSource
  ( -- * Creating a Request
    GetDataSource (..),
    newGetDataSource,

    -- * Request Lenses
    getDataSource_verbose,
    getDataSource_dataSourceId,

    -- * Destructuring the Response
    GetDataSourceResponse (..),
    newGetDataSourceResponse,

    -- * Response Lenses
    getDataSourceResponse_status,
    getDataSourceResponse_startedAt,
    getDataSourceResponse_dataRearrangement,
    getDataSourceResponse_roleARN,
    getDataSourceResponse_redshiftMetadata,
    getDataSourceResponse_message,
    getDataSourceResponse_dataSourceId,
    getDataSourceResponse_computeStatistics,
    getDataSourceResponse_dataLocationS3,
    getDataSourceResponse_createdAt,
    getDataSourceResponse_numberOfFiles,
    getDataSourceResponse_finishedAt,
    getDataSourceResponse_createdByIamUser,
    getDataSourceResponse_name,
    getDataSourceResponse_dataSourceSchema,
    getDataSourceResponse_dataSizeInBytes,
    getDataSourceResponse_computeTime,
    getDataSourceResponse_rDSMetadata,
    getDataSourceResponse_lastUpdatedAt,
    getDataSourceResponse_logUri,
    getDataSourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDataSource' smart constructor.
data GetDataSource = GetDataSource'
  { -- | Specifies whether the @GetDataSource@ operation should return
    -- @DataSourceSchema@.
    --
    -- If true, @DataSourceSchema@ is returned.
    --
    -- If false, @DataSourceSchema@ is not returned.
    verbose :: Prelude.Maybe Prelude.Bool,
    -- | The ID assigned to the @DataSource@ at creation.
    dataSourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'verbose', 'getDataSource_verbose' - Specifies whether the @GetDataSource@ operation should return
-- @DataSourceSchema@.
--
-- If true, @DataSourceSchema@ is returned.
--
-- If false, @DataSourceSchema@ is not returned.
--
-- 'dataSourceId', 'getDataSource_dataSourceId' - The ID assigned to the @DataSource@ at creation.
newGetDataSource ::
  -- | 'dataSourceId'
  Prelude.Text ->
  GetDataSource
newGetDataSource pDataSourceId_ =
  GetDataSource'
    { verbose = Prelude.Nothing,
      dataSourceId = pDataSourceId_
    }

-- | Specifies whether the @GetDataSource@ operation should return
-- @DataSourceSchema@.
--
-- If true, @DataSourceSchema@ is returned.
--
-- If false, @DataSourceSchema@ is not returned.
getDataSource_verbose :: Lens.Lens' GetDataSource (Prelude.Maybe Prelude.Bool)
getDataSource_verbose = Lens.lens (\GetDataSource' {verbose} -> verbose) (\s@GetDataSource' {} a -> s {verbose = a} :: GetDataSource)

-- | The ID assigned to the @DataSource@ at creation.
getDataSource_dataSourceId :: Lens.Lens' GetDataSource Prelude.Text
getDataSource_dataSourceId = Lens.lens (\GetDataSource' {dataSourceId} -> dataSourceId) (\s@GetDataSource' {} a -> s {dataSourceId = a} :: GetDataSource)

instance Core.AWSRequest GetDataSource where
  type
    AWSResponse GetDataSource =
      GetDataSourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataSourceResponse'
            Prelude.<$> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "StartedAt")
            Prelude.<*> (x Core..?> "DataRearrangement")
            Prelude.<*> (x Core..?> "RoleARN")
            Prelude.<*> (x Core..?> "RedshiftMetadata")
            Prelude.<*> (x Core..?> "Message")
            Prelude.<*> (x Core..?> "DataSourceId")
            Prelude.<*> (x Core..?> "ComputeStatistics")
            Prelude.<*> (x Core..?> "DataLocationS3")
            Prelude.<*> (x Core..?> "CreatedAt")
            Prelude.<*> (x Core..?> "NumberOfFiles")
            Prelude.<*> (x Core..?> "FinishedAt")
            Prelude.<*> (x Core..?> "CreatedByIamUser")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "DataSourceSchema")
            Prelude.<*> (x Core..?> "DataSizeInBytes")
            Prelude.<*> (x Core..?> "ComputeTime")
            Prelude.<*> (x Core..?> "RDSMetadata")
            Prelude.<*> (x Core..?> "LastUpdatedAt")
            Prelude.<*> (x Core..?> "LogUri")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDataSource

instance Prelude.NFData GetDataSource

instance Core.ToHeaders GetDataSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonML_20141212.GetDataSource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetDataSource where
  toJSON GetDataSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Verbose" Core..=) Prelude.<$> verbose,
            Prelude.Just ("DataSourceId" Core..= dataSourceId)
          ]
      )

instance Core.ToPath GetDataSource where
  toPath = Prelude.const "/"

instance Core.ToQuery GetDataSource where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @GetDataSource@ operation and describes a
-- @DataSource@.
--
-- /See:/ 'newGetDataSourceResponse' smart constructor.
data GetDataSourceResponse = GetDataSourceResponse'
  { -- | The current status of the @DataSource@. This element can have one of the
    -- following values:
    --
    -- -   @PENDING@ - Amazon ML submitted a request to create a @DataSource@.
    -- -   @INPROGRESS@ - The creation process is underway.
    -- -   @FAILED@ - The request to create a @DataSource@ did not run to
    --     completion. It is not usable.
    -- -   @COMPLETED@ - The creation process completed successfully.
    -- -   @DELETED@ - The @DataSource@ is marked as deleted. It is not usable.
    status :: Prelude.Maybe EntityStatus,
    -- | The epoch time when Amazon Machine Learning marked the @DataSource@ as
    -- @INPROGRESS@. @StartedAt@ isn\'t available if the @DataSource@ is in the
    -- @PENDING@ state.
    startedAt :: Prelude.Maybe Core.POSIX,
    -- | A JSON string that represents the splitting and rearrangement
    -- requirement used when this @DataSource@ was created.
    dataRearrangement :: Prelude.Maybe Prelude.Text,
    roleARN :: Prelude.Maybe Prelude.Text,
    redshiftMetadata :: Prelude.Maybe RedshiftMetadata,
    -- | The user-supplied description of the most recent details about creating
    -- the @DataSource@.
    message :: Prelude.Maybe Prelude.Text,
    -- | The ID assigned to the @DataSource@ at creation. This value should be
    -- identical to the value of the @DataSourceId@ in the request.
    dataSourceId :: Prelude.Maybe Prelude.Text,
    -- | The parameter is @true@ if statistics need to be generated from the
    -- observation data.
    computeStatistics :: Prelude.Maybe Prelude.Bool,
    -- | The location of the data file or directory in Amazon Simple Storage
    -- Service (Amazon S3).
    dataLocationS3 :: Prelude.Maybe Prelude.Text,
    -- | The time that the @DataSource@ was created. The time is expressed in
    -- epoch time.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The number of data files referenced by the @DataSource@.
    numberOfFiles :: Prelude.Maybe Prelude.Integer,
    -- | The epoch time when Amazon Machine Learning marked the @DataSource@ as
    -- @COMPLETED@ or @FAILED@. @FinishedAt@ is only available when the
    -- @DataSource@ is in the @COMPLETED@ or @FAILED@ state.
    finishedAt :: Prelude.Maybe Core.POSIX,
    -- | The AWS user account from which the @DataSource@ was created. The
    -- account type can be either an AWS root account or an AWS Identity and
    -- Access Management (IAM) user account.
    createdByIamUser :: Prelude.Maybe Prelude.Text,
    -- | A user-supplied name or description of the @DataSource@.
    name :: Prelude.Maybe Prelude.Text,
    -- | The schema used by all of the data files of this @DataSource@.
    --
    -- Note
    --
    -- This parameter is provided as part of the verbose format.
    dataSourceSchema :: Prelude.Maybe Prelude.Text,
    -- | The total size of observations in the data files.
    dataSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The approximate CPU time in milliseconds that Amazon Machine Learning
    -- spent processing the @DataSource@, normalized and scaled on computation
    -- resources. @ComputeTime@ is only available if the @DataSource@ is in the
    -- @COMPLETED@ state and the @ComputeStatistics@ is set to true.
    computeTime :: Prelude.Maybe Prelude.Integer,
    rDSMetadata :: Prelude.Maybe RDSMetadata,
    -- | The time of the most recent edit to the @DataSource@. The time is
    -- expressed in epoch time.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | A link to the file containing logs of @CreateDataSourceFrom*@
    -- operations.
    logUri :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'getDataSourceResponse_status' - The current status of the @DataSource@. This element can have one of the
-- following values:
--
-- -   @PENDING@ - Amazon ML submitted a request to create a @DataSource@.
-- -   @INPROGRESS@ - The creation process is underway.
-- -   @FAILED@ - The request to create a @DataSource@ did not run to
--     completion. It is not usable.
-- -   @COMPLETED@ - The creation process completed successfully.
-- -   @DELETED@ - The @DataSource@ is marked as deleted. It is not usable.
--
-- 'startedAt', 'getDataSourceResponse_startedAt' - The epoch time when Amazon Machine Learning marked the @DataSource@ as
-- @INPROGRESS@. @StartedAt@ isn\'t available if the @DataSource@ is in the
-- @PENDING@ state.
--
-- 'dataRearrangement', 'getDataSourceResponse_dataRearrangement' - A JSON string that represents the splitting and rearrangement
-- requirement used when this @DataSource@ was created.
--
-- 'roleARN', 'getDataSourceResponse_roleARN' - Undocumented member.
--
-- 'redshiftMetadata', 'getDataSourceResponse_redshiftMetadata' - Undocumented member.
--
-- 'message', 'getDataSourceResponse_message' - The user-supplied description of the most recent details about creating
-- the @DataSource@.
--
-- 'dataSourceId', 'getDataSourceResponse_dataSourceId' - The ID assigned to the @DataSource@ at creation. This value should be
-- identical to the value of the @DataSourceId@ in the request.
--
-- 'computeStatistics', 'getDataSourceResponse_computeStatistics' - The parameter is @true@ if statistics need to be generated from the
-- observation data.
--
-- 'dataLocationS3', 'getDataSourceResponse_dataLocationS3' - The location of the data file or directory in Amazon Simple Storage
-- Service (Amazon S3).
--
-- 'createdAt', 'getDataSourceResponse_createdAt' - The time that the @DataSource@ was created. The time is expressed in
-- epoch time.
--
-- 'numberOfFiles', 'getDataSourceResponse_numberOfFiles' - The number of data files referenced by the @DataSource@.
--
-- 'finishedAt', 'getDataSourceResponse_finishedAt' - The epoch time when Amazon Machine Learning marked the @DataSource@ as
-- @COMPLETED@ or @FAILED@. @FinishedAt@ is only available when the
-- @DataSource@ is in the @COMPLETED@ or @FAILED@ state.
--
-- 'createdByIamUser', 'getDataSourceResponse_createdByIamUser' - The AWS user account from which the @DataSource@ was created. The
-- account type can be either an AWS root account or an AWS Identity and
-- Access Management (IAM) user account.
--
-- 'name', 'getDataSourceResponse_name' - A user-supplied name or description of the @DataSource@.
--
-- 'dataSourceSchema', 'getDataSourceResponse_dataSourceSchema' - The schema used by all of the data files of this @DataSource@.
--
-- Note
--
-- This parameter is provided as part of the verbose format.
--
-- 'dataSizeInBytes', 'getDataSourceResponse_dataSizeInBytes' - The total size of observations in the data files.
--
-- 'computeTime', 'getDataSourceResponse_computeTime' - The approximate CPU time in milliseconds that Amazon Machine Learning
-- spent processing the @DataSource@, normalized and scaled on computation
-- resources. @ComputeTime@ is only available if the @DataSource@ is in the
-- @COMPLETED@ state and the @ComputeStatistics@ is set to true.
--
-- 'rDSMetadata', 'getDataSourceResponse_rDSMetadata' - Undocumented member.
--
-- 'lastUpdatedAt', 'getDataSourceResponse_lastUpdatedAt' - The time of the most recent edit to the @DataSource@. The time is
-- expressed in epoch time.
--
-- 'logUri', 'getDataSourceResponse_logUri' - A link to the file containing logs of @CreateDataSourceFrom*@
-- operations.
--
-- 'httpStatus', 'getDataSourceResponse_httpStatus' - The response's http status code.
newGetDataSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDataSourceResponse
newGetDataSourceResponse pHttpStatus_ =
  GetDataSourceResponse'
    { status = Prelude.Nothing,
      startedAt = Prelude.Nothing,
      dataRearrangement = Prelude.Nothing,
      roleARN = Prelude.Nothing,
      redshiftMetadata = Prelude.Nothing,
      message = Prelude.Nothing,
      dataSourceId = Prelude.Nothing,
      computeStatistics = Prelude.Nothing,
      dataLocationS3 = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      numberOfFiles = Prelude.Nothing,
      finishedAt = Prelude.Nothing,
      createdByIamUser = Prelude.Nothing,
      name = Prelude.Nothing,
      dataSourceSchema = Prelude.Nothing,
      dataSizeInBytes = Prelude.Nothing,
      computeTime = Prelude.Nothing,
      rDSMetadata = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      logUri = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current status of the @DataSource@. This element can have one of the
-- following values:
--
-- -   @PENDING@ - Amazon ML submitted a request to create a @DataSource@.
-- -   @INPROGRESS@ - The creation process is underway.
-- -   @FAILED@ - The request to create a @DataSource@ did not run to
--     completion. It is not usable.
-- -   @COMPLETED@ - The creation process completed successfully.
-- -   @DELETED@ - The @DataSource@ is marked as deleted. It is not usable.
getDataSourceResponse_status :: Lens.Lens' GetDataSourceResponse (Prelude.Maybe EntityStatus)
getDataSourceResponse_status = Lens.lens (\GetDataSourceResponse' {status} -> status) (\s@GetDataSourceResponse' {} a -> s {status = a} :: GetDataSourceResponse)

-- | The epoch time when Amazon Machine Learning marked the @DataSource@ as
-- @INPROGRESS@. @StartedAt@ isn\'t available if the @DataSource@ is in the
-- @PENDING@ state.
getDataSourceResponse_startedAt :: Lens.Lens' GetDataSourceResponse (Prelude.Maybe Prelude.UTCTime)
getDataSourceResponse_startedAt = Lens.lens (\GetDataSourceResponse' {startedAt} -> startedAt) (\s@GetDataSourceResponse' {} a -> s {startedAt = a} :: GetDataSourceResponse) Prelude.. Lens.mapping Core._Time

-- | A JSON string that represents the splitting and rearrangement
-- requirement used when this @DataSource@ was created.
getDataSourceResponse_dataRearrangement :: Lens.Lens' GetDataSourceResponse (Prelude.Maybe Prelude.Text)
getDataSourceResponse_dataRearrangement = Lens.lens (\GetDataSourceResponse' {dataRearrangement} -> dataRearrangement) (\s@GetDataSourceResponse' {} a -> s {dataRearrangement = a} :: GetDataSourceResponse)

-- | Undocumented member.
getDataSourceResponse_roleARN :: Lens.Lens' GetDataSourceResponse (Prelude.Maybe Prelude.Text)
getDataSourceResponse_roleARN = Lens.lens (\GetDataSourceResponse' {roleARN} -> roleARN) (\s@GetDataSourceResponse' {} a -> s {roleARN = a} :: GetDataSourceResponse)

-- | Undocumented member.
getDataSourceResponse_redshiftMetadata :: Lens.Lens' GetDataSourceResponse (Prelude.Maybe RedshiftMetadata)
getDataSourceResponse_redshiftMetadata = Lens.lens (\GetDataSourceResponse' {redshiftMetadata} -> redshiftMetadata) (\s@GetDataSourceResponse' {} a -> s {redshiftMetadata = a} :: GetDataSourceResponse)

-- | The user-supplied description of the most recent details about creating
-- the @DataSource@.
getDataSourceResponse_message :: Lens.Lens' GetDataSourceResponse (Prelude.Maybe Prelude.Text)
getDataSourceResponse_message = Lens.lens (\GetDataSourceResponse' {message} -> message) (\s@GetDataSourceResponse' {} a -> s {message = a} :: GetDataSourceResponse)

-- | The ID assigned to the @DataSource@ at creation. This value should be
-- identical to the value of the @DataSourceId@ in the request.
getDataSourceResponse_dataSourceId :: Lens.Lens' GetDataSourceResponse (Prelude.Maybe Prelude.Text)
getDataSourceResponse_dataSourceId = Lens.lens (\GetDataSourceResponse' {dataSourceId} -> dataSourceId) (\s@GetDataSourceResponse' {} a -> s {dataSourceId = a} :: GetDataSourceResponse)

-- | The parameter is @true@ if statistics need to be generated from the
-- observation data.
getDataSourceResponse_computeStatistics :: Lens.Lens' GetDataSourceResponse (Prelude.Maybe Prelude.Bool)
getDataSourceResponse_computeStatistics = Lens.lens (\GetDataSourceResponse' {computeStatistics} -> computeStatistics) (\s@GetDataSourceResponse' {} a -> s {computeStatistics = a} :: GetDataSourceResponse)

-- | The location of the data file or directory in Amazon Simple Storage
-- Service (Amazon S3).
getDataSourceResponse_dataLocationS3 :: Lens.Lens' GetDataSourceResponse (Prelude.Maybe Prelude.Text)
getDataSourceResponse_dataLocationS3 = Lens.lens (\GetDataSourceResponse' {dataLocationS3} -> dataLocationS3) (\s@GetDataSourceResponse' {} a -> s {dataLocationS3 = a} :: GetDataSourceResponse)

-- | The time that the @DataSource@ was created. The time is expressed in
-- epoch time.
getDataSourceResponse_createdAt :: Lens.Lens' GetDataSourceResponse (Prelude.Maybe Prelude.UTCTime)
getDataSourceResponse_createdAt = Lens.lens (\GetDataSourceResponse' {createdAt} -> createdAt) (\s@GetDataSourceResponse' {} a -> s {createdAt = a} :: GetDataSourceResponse) Prelude.. Lens.mapping Core._Time

-- | The number of data files referenced by the @DataSource@.
getDataSourceResponse_numberOfFiles :: Lens.Lens' GetDataSourceResponse (Prelude.Maybe Prelude.Integer)
getDataSourceResponse_numberOfFiles = Lens.lens (\GetDataSourceResponse' {numberOfFiles} -> numberOfFiles) (\s@GetDataSourceResponse' {} a -> s {numberOfFiles = a} :: GetDataSourceResponse)

-- | The epoch time when Amazon Machine Learning marked the @DataSource@ as
-- @COMPLETED@ or @FAILED@. @FinishedAt@ is only available when the
-- @DataSource@ is in the @COMPLETED@ or @FAILED@ state.
getDataSourceResponse_finishedAt :: Lens.Lens' GetDataSourceResponse (Prelude.Maybe Prelude.UTCTime)
getDataSourceResponse_finishedAt = Lens.lens (\GetDataSourceResponse' {finishedAt} -> finishedAt) (\s@GetDataSourceResponse' {} a -> s {finishedAt = a} :: GetDataSourceResponse) Prelude.. Lens.mapping Core._Time

-- | The AWS user account from which the @DataSource@ was created. The
-- account type can be either an AWS root account or an AWS Identity and
-- Access Management (IAM) user account.
getDataSourceResponse_createdByIamUser :: Lens.Lens' GetDataSourceResponse (Prelude.Maybe Prelude.Text)
getDataSourceResponse_createdByIamUser = Lens.lens (\GetDataSourceResponse' {createdByIamUser} -> createdByIamUser) (\s@GetDataSourceResponse' {} a -> s {createdByIamUser = a} :: GetDataSourceResponse)

-- | A user-supplied name or description of the @DataSource@.
getDataSourceResponse_name :: Lens.Lens' GetDataSourceResponse (Prelude.Maybe Prelude.Text)
getDataSourceResponse_name = Lens.lens (\GetDataSourceResponse' {name} -> name) (\s@GetDataSourceResponse' {} a -> s {name = a} :: GetDataSourceResponse)

-- | The schema used by all of the data files of this @DataSource@.
--
-- Note
--
-- This parameter is provided as part of the verbose format.
getDataSourceResponse_dataSourceSchema :: Lens.Lens' GetDataSourceResponse (Prelude.Maybe Prelude.Text)
getDataSourceResponse_dataSourceSchema = Lens.lens (\GetDataSourceResponse' {dataSourceSchema} -> dataSourceSchema) (\s@GetDataSourceResponse' {} a -> s {dataSourceSchema = a} :: GetDataSourceResponse)

-- | The total size of observations in the data files.
getDataSourceResponse_dataSizeInBytes :: Lens.Lens' GetDataSourceResponse (Prelude.Maybe Prelude.Integer)
getDataSourceResponse_dataSizeInBytes = Lens.lens (\GetDataSourceResponse' {dataSizeInBytes} -> dataSizeInBytes) (\s@GetDataSourceResponse' {} a -> s {dataSizeInBytes = a} :: GetDataSourceResponse)

-- | The approximate CPU time in milliseconds that Amazon Machine Learning
-- spent processing the @DataSource@, normalized and scaled on computation
-- resources. @ComputeTime@ is only available if the @DataSource@ is in the
-- @COMPLETED@ state and the @ComputeStatistics@ is set to true.
getDataSourceResponse_computeTime :: Lens.Lens' GetDataSourceResponse (Prelude.Maybe Prelude.Integer)
getDataSourceResponse_computeTime = Lens.lens (\GetDataSourceResponse' {computeTime} -> computeTime) (\s@GetDataSourceResponse' {} a -> s {computeTime = a} :: GetDataSourceResponse)

-- | Undocumented member.
getDataSourceResponse_rDSMetadata :: Lens.Lens' GetDataSourceResponse (Prelude.Maybe RDSMetadata)
getDataSourceResponse_rDSMetadata = Lens.lens (\GetDataSourceResponse' {rDSMetadata} -> rDSMetadata) (\s@GetDataSourceResponse' {} a -> s {rDSMetadata = a} :: GetDataSourceResponse)

-- | The time of the most recent edit to the @DataSource@. The time is
-- expressed in epoch time.
getDataSourceResponse_lastUpdatedAt :: Lens.Lens' GetDataSourceResponse (Prelude.Maybe Prelude.UTCTime)
getDataSourceResponse_lastUpdatedAt = Lens.lens (\GetDataSourceResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@GetDataSourceResponse' {} a -> s {lastUpdatedAt = a} :: GetDataSourceResponse) Prelude.. Lens.mapping Core._Time

-- | A link to the file containing logs of @CreateDataSourceFrom*@
-- operations.
getDataSourceResponse_logUri :: Lens.Lens' GetDataSourceResponse (Prelude.Maybe Prelude.Text)
getDataSourceResponse_logUri = Lens.lens (\GetDataSourceResponse' {logUri} -> logUri) (\s@GetDataSourceResponse' {} a -> s {logUri = a} :: GetDataSourceResponse)

-- | The response's http status code.
getDataSourceResponse_httpStatus :: Lens.Lens' GetDataSourceResponse Prelude.Int
getDataSourceResponse_httpStatus = Lens.lens (\GetDataSourceResponse' {httpStatus} -> httpStatus) (\s@GetDataSourceResponse' {} a -> s {httpStatus = a} :: GetDataSourceResponse)

instance Prelude.NFData GetDataSourceResponse
