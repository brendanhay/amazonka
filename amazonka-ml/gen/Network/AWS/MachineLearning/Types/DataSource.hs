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
-- Module      : Network.AWS.MachineLearning.Types.DataSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.DataSource where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types.EntityStatus
import Network.AWS.MachineLearning.Types.RDSMetadata
import Network.AWS.MachineLearning.Types.RedshiftMetadata

-- | Represents the output of the @GetDataSource@ operation.
--
-- The content consists of the detailed metadata and data file information
-- and the current status of the @DataSource@.
--
-- /See:/ 'newDataSource' smart constructor.
data DataSource = DataSource'
  { -- | The current status of the @DataSource@. This element can have one of the
    -- following values:
    --
    -- -   PENDING - Amazon Machine Learning (Amazon ML) submitted a request to
    --     create a @DataSource@.
    -- -   INPROGRESS - The creation process is underway.
    -- -   FAILED - The request to create a @DataSource@ did not run to
    --     completion. It is not usable.
    -- -   COMPLETED - The creation process completed successfully.
    -- -   DELETED - The @DataSource@ is marked as deleted. It is not usable.
    status :: Core.Maybe EntityStatus,
    startedAt :: Core.Maybe Core.POSIX,
    -- | A JSON string that represents the splitting and rearrangement
    -- requirement used when this @DataSource@ was created.
    dataRearrangement :: Core.Maybe Core.Text,
    roleARN :: Core.Maybe Core.Text,
    redshiftMetadata :: Core.Maybe RedshiftMetadata,
    -- | A description of the most recent details about creating the
    -- @DataSource@.
    message :: Core.Maybe Core.Text,
    -- | The ID that is assigned to the @DataSource@ during creation.
    dataSourceId :: Core.Maybe Core.Text,
    -- | The parameter is @true@ if statistics need to be generated from the
    -- observation data.
    computeStatistics :: Core.Maybe Core.Bool,
    -- | The location and name of the data in Amazon Simple Storage Service
    -- (Amazon S3) that is used by a @DataSource@.
    dataLocationS3 :: Core.Maybe Core.Text,
    -- | The time that the @DataSource@ was created. The time is expressed in
    -- epoch time.
    createdAt :: Core.Maybe Core.POSIX,
    -- | The number of data files referenced by the @DataSource@.
    numberOfFiles :: Core.Maybe Core.Integer,
    finishedAt :: Core.Maybe Core.POSIX,
    -- | The AWS user account from which the @DataSource@ was created. The
    -- account type can be either an AWS root account or an AWS Identity and
    -- Access Management (IAM) user account.
    createdByIamUser :: Core.Maybe Core.Text,
    -- | A user-supplied name or description of the @DataSource@.
    name :: Core.Maybe Core.Text,
    -- | The total number of observations contained in the data files that the
    -- @DataSource@ references.
    dataSizeInBytes :: Core.Maybe Core.Integer,
    computeTime :: Core.Maybe Core.Integer,
    rDSMetadata :: Core.Maybe RDSMetadata,
    -- | The time of the most recent edit to the @BatchPrediction@. The time is
    -- expressed in epoch time.
    lastUpdatedAt :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'dataSource_status' - The current status of the @DataSource@. This element can have one of the
-- following values:
--
-- -   PENDING - Amazon Machine Learning (Amazon ML) submitted a request to
--     create a @DataSource@.
-- -   INPROGRESS - The creation process is underway.
-- -   FAILED - The request to create a @DataSource@ did not run to
--     completion. It is not usable.
-- -   COMPLETED - The creation process completed successfully.
-- -   DELETED - The @DataSource@ is marked as deleted. It is not usable.
--
-- 'startedAt', 'dataSource_startedAt' - Undocumented member.
--
-- 'dataRearrangement', 'dataSource_dataRearrangement' - A JSON string that represents the splitting and rearrangement
-- requirement used when this @DataSource@ was created.
--
-- 'roleARN', 'dataSource_roleARN' - Undocumented member.
--
-- 'redshiftMetadata', 'dataSource_redshiftMetadata' - Undocumented member.
--
-- 'message', 'dataSource_message' - A description of the most recent details about creating the
-- @DataSource@.
--
-- 'dataSourceId', 'dataSource_dataSourceId' - The ID that is assigned to the @DataSource@ during creation.
--
-- 'computeStatistics', 'dataSource_computeStatistics' - The parameter is @true@ if statistics need to be generated from the
-- observation data.
--
-- 'dataLocationS3', 'dataSource_dataLocationS3' - The location and name of the data in Amazon Simple Storage Service
-- (Amazon S3) that is used by a @DataSource@.
--
-- 'createdAt', 'dataSource_createdAt' - The time that the @DataSource@ was created. The time is expressed in
-- epoch time.
--
-- 'numberOfFiles', 'dataSource_numberOfFiles' - The number of data files referenced by the @DataSource@.
--
-- 'finishedAt', 'dataSource_finishedAt' - Undocumented member.
--
-- 'createdByIamUser', 'dataSource_createdByIamUser' - The AWS user account from which the @DataSource@ was created. The
-- account type can be either an AWS root account or an AWS Identity and
-- Access Management (IAM) user account.
--
-- 'name', 'dataSource_name' - A user-supplied name or description of the @DataSource@.
--
-- 'dataSizeInBytes', 'dataSource_dataSizeInBytes' - The total number of observations contained in the data files that the
-- @DataSource@ references.
--
-- 'computeTime', 'dataSource_computeTime' - Undocumented member.
--
-- 'rDSMetadata', 'dataSource_rDSMetadata' - Undocumented member.
--
-- 'lastUpdatedAt', 'dataSource_lastUpdatedAt' - The time of the most recent edit to the @BatchPrediction@. The time is
-- expressed in epoch time.
newDataSource ::
  DataSource
newDataSource =
  DataSource'
    { status = Core.Nothing,
      startedAt = Core.Nothing,
      dataRearrangement = Core.Nothing,
      roleARN = Core.Nothing,
      redshiftMetadata = Core.Nothing,
      message = Core.Nothing,
      dataSourceId = Core.Nothing,
      computeStatistics = Core.Nothing,
      dataLocationS3 = Core.Nothing,
      createdAt = Core.Nothing,
      numberOfFiles = Core.Nothing,
      finishedAt = Core.Nothing,
      createdByIamUser = Core.Nothing,
      name = Core.Nothing,
      dataSizeInBytes = Core.Nothing,
      computeTime = Core.Nothing,
      rDSMetadata = Core.Nothing,
      lastUpdatedAt = Core.Nothing
    }

-- | The current status of the @DataSource@. This element can have one of the
-- following values:
--
-- -   PENDING - Amazon Machine Learning (Amazon ML) submitted a request to
--     create a @DataSource@.
-- -   INPROGRESS - The creation process is underway.
-- -   FAILED - The request to create a @DataSource@ did not run to
--     completion. It is not usable.
-- -   COMPLETED - The creation process completed successfully.
-- -   DELETED - The @DataSource@ is marked as deleted. It is not usable.
dataSource_status :: Lens.Lens' DataSource (Core.Maybe EntityStatus)
dataSource_status = Lens.lens (\DataSource' {status} -> status) (\s@DataSource' {} a -> s {status = a} :: DataSource)

-- | Undocumented member.
dataSource_startedAt :: Lens.Lens' DataSource (Core.Maybe Core.UTCTime)
dataSource_startedAt = Lens.lens (\DataSource' {startedAt} -> startedAt) (\s@DataSource' {} a -> s {startedAt = a} :: DataSource) Core.. Lens.mapping Core._Time

-- | A JSON string that represents the splitting and rearrangement
-- requirement used when this @DataSource@ was created.
dataSource_dataRearrangement :: Lens.Lens' DataSource (Core.Maybe Core.Text)
dataSource_dataRearrangement = Lens.lens (\DataSource' {dataRearrangement} -> dataRearrangement) (\s@DataSource' {} a -> s {dataRearrangement = a} :: DataSource)

-- | Undocumented member.
dataSource_roleARN :: Lens.Lens' DataSource (Core.Maybe Core.Text)
dataSource_roleARN = Lens.lens (\DataSource' {roleARN} -> roleARN) (\s@DataSource' {} a -> s {roleARN = a} :: DataSource)

-- | Undocumented member.
dataSource_redshiftMetadata :: Lens.Lens' DataSource (Core.Maybe RedshiftMetadata)
dataSource_redshiftMetadata = Lens.lens (\DataSource' {redshiftMetadata} -> redshiftMetadata) (\s@DataSource' {} a -> s {redshiftMetadata = a} :: DataSource)

-- | A description of the most recent details about creating the
-- @DataSource@.
dataSource_message :: Lens.Lens' DataSource (Core.Maybe Core.Text)
dataSource_message = Lens.lens (\DataSource' {message} -> message) (\s@DataSource' {} a -> s {message = a} :: DataSource)

-- | The ID that is assigned to the @DataSource@ during creation.
dataSource_dataSourceId :: Lens.Lens' DataSource (Core.Maybe Core.Text)
dataSource_dataSourceId = Lens.lens (\DataSource' {dataSourceId} -> dataSourceId) (\s@DataSource' {} a -> s {dataSourceId = a} :: DataSource)

-- | The parameter is @true@ if statistics need to be generated from the
-- observation data.
dataSource_computeStatistics :: Lens.Lens' DataSource (Core.Maybe Core.Bool)
dataSource_computeStatistics = Lens.lens (\DataSource' {computeStatistics} -> computeStatistics) (\s@DataSource' {} a -> s {computeStatistics = a} :: DataSource)

-- | The location and name of the data in Amazon Simple Storage Service
-- (Amazon S3) that is used by a @DataSource@.
dataSource_dataLocationS3 :: Lens.Lens' DataSource (Core.Maybe Core.Text)
dataSource_dataLocationS3 = Lens.lens (\DataSource' {dataLocationS3} -> dataLocationS3) (\s@DataSource' {} a -> s {dataLocationS3 = a} :: DataSource)

-- | The time that the @DataSource@ was created. The time is expressed in
-- epoch time.
dataSource_createdAt :: Lens.Lens' DataSource (Core.Maybe Core.UTCTime)
dataSource_createdAt = Lens.lens (\DataSource' {createdAt} -> createdAt) (\s@DataSource' {} a -> s {createdAt = a} :: DataSource) Core.. Lens.mapping Core._Time

-- | The number of data files referenced by the @DataSource@.
dataSource_numberOfFiles :: Lens.Lens' DataSource (Core.Maybe Core.Integer)
dataSource_numberOfFiles = Lens.lens (\DataSource' {numberOfFiles} -> numberOfFiles) (\s@DataSource' {} a -> s {numberOfFiles = a} :: DataSource)

-- | Undocumented member.
dataSource_finishedAt :: Lens.Lens' DataSource (Core.Maybe Core.UTCTime)
dataSource_finishedAt = Lens.lens (\DataSource' {finishedAt} -> finishedAt) (\s@DataSource' {} a -> s {finishedAt = a} :: DataSource) Core.. Lens.mapping Core._Time

-- | The AWS user account from which the @DataSource@ was created. The
-- account type can be either an AWS root account or an AWS Identity and
-- Access Management (IAM) user account.
dataSource_createdByIamUser :: Lens.Lens' DataSource (Core.Maybe Core.Text)
dataSource_createdByIamUser = Lens.lens (\DataSource' {createdByIamUser} -> createdByIamUser) (\s@DataSource' {} a -> s {createdByIamUser = a} :: DataSource)

-- | A user-supplied name or description of the @DataSource@.
dataSource_name :: Lens.Lens' DataSource (Core.Maybe Core.Text)
dataSource_name = Lens.lens (\DataSource' {name} -> name) (\s@DataSource' {} a -> s {name = a} :: DataSource)

-- | The total number of observations contained in the data files that the
-- @DataSource@ references.
dataSource_dataSizeInBytes :: Lens.Lens' DataSource (Core.Maybe Core.Integer)
dataSource_dataSizeInBytes = Lens.lens (\DataSource' {dataSizeInBytes} -> dataSizeInBytes) (\s@DataSource' {} a -> s {dataSizeInBytes = a} :: DataSource)

-- | Undocumented member.
dataSource_computeTime :: Lens.Lens' DataSource (Core.Maybe Core.Integer)
dataSource_computeTime = Lens.lens (\DataSource' {computeTime} -> computeTime) (\s@DataSource' {} a -> s {computeTime = a} :: DataSource)

-- | Undocumented member.
dataSource_rDSMetadata :: Lens.Lens' DataSource (Core.Maybe RDSMetadata)
dataSource_rDSMetadata = Lens.lens (\DataSource' {rDSMetadata} -> rDSMetadata) (\s@DataSource' {} a -> s {rDSMetadata = a} :: DataSource)

-- | The time of the most recent edit to the @BatchPrediction@. The time is
-- expressed in epoch time.
dataSource_lastUpdatedAt :: Lens.Lens' DataSource (Core.Maybe Core.UTCTime)
dataSource_lastUpdatedAt = Lens.lens (\DataSource' {lastUpdatedAt} -> lastUpdatedAt) (\s@DataSource' {} a -> s {lastUpdatedAt = a} :: DataSource) Core.. Lens.mapping Core._Time

instance Core.FromJSON DataSource where
  parseJSON =
    Core.withObject
      "DataSource"
      ( \x ->
          DataSource'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "StartedAt")
            Core.<*> (x Core..:? "DataRearrangement")
            Core.<*> (x Core..:? "RoleARN")
            Core.<*> (x Core..:? "RedshiftMetadata")
            Core.<*> (x Core..:? "Message")
            Core.<*> (x Core..:? "DataSourceId")
            Core.<*> (x Core..:? "ComputeStatistics")
            Core.<*> (x Core..:? "DataLocationS3")
            Core.<*> (x Core..:? "CreatedAt")
            Core.<*> (x Core..:? "NumberOfFiles")
            Core.<*> (x Core..:? "FinishedAt")
            Core.<*> (x Core..:? "CreatedByIamUser")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "DataSizeInBytes")
            Core.<*> (x Core..:? "ComputeTime")
            Core.<*> (x Core..:? "RDSMetadata")
            Core.<*> (x Core..:? "LastUpdatedAt")
      )

instance Core.Hashable DataSource

instance Core.NFData DataSource
