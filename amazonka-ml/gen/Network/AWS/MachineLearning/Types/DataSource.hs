{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types.EntityStatus
import Network.AWS.MachineLearning.Types.RDSMetadata
import Network.AWS.MachineLearning.Types.RedshiftMetadata
import qualified Network.AWS.Prelude as Prelude

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
    status :: Prelude.Maybe EntityStatus,
    startedAt :: Prelude.Maybe Prelude.POSIX,
    -- | A JSON string that represents the splitting and rearrangement
    -- requirement used when this @DataSource@ was created.
    dataRearrangement :: Prelude.Maybe Prelude.Text,
    roleARN :: Prelude.Maybe Prelude.Text,
    redshiftMetadata :: Prelude.Maybe RedshiftMetadata,
    -- | A description of the most recent details about creating the
    -- @DataSource@.
    message :: Prelude.Maybe Prelude.Text,
    -- | The ID that is assigned to the @DataSource@ during creation.
    dataSourceId :: Prelude.Maybe Prelude.Text,
    -- | The parameter is @true@ if statistics need to be generated from the
    -- observation data.
    computeStatistics :: Prelude.Maybe Prelude.Bool,
    -- | The location and name of the data in Amazon Simple Storage Service
    -- (Amazon S3) that is used by a @DataSource@.
    dataLocationS3 :: Prelude.Maybe Prelude.Text,
    -- | The time that the @DataSource@ was created. The time is expressed in
    -- epoch time.
    createdAt :: Prelude.Maybe Prelude.POSIX,
    -- | The number of data files referenced by the @DataSource@.
    numberOfFiles :: Prelude.Maybe Prelude.Integer,
    finishedAt :: Prelude.Maybe Prelude.POSIX,
    -- | The AWS user account from which the @DataSource@ was created. The
    -- account type can be either an AWS root account or an AWS Identity and
    -- Access Management (IAM) user account.
    createdByIamUser :: Prelude.Maybe Prelude.Text,
    -- | A user-supplied name or description of the @DataSource@.
    name :: Prelude.Maybe Prelude.Text,
    -- | The total number of observations contained in the data files that the
    -- @DataSource@ references.
    dataSizeInBytes :: Prelude.Maybe Prelude.Integer,
    computeTime :: Prelude.Maybe Prelude.Integer,
    rDSMetadata :: Prelude.Maybe RDSMetadata,
    -- | The time of the most recent edit to the @BatchPrediction@. The time is
    -- expressed in epoch time.
    lastUpdatedAt :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
      dataSizeInBytes = Prelude.Nothing,
      computeTime = Prelude.Nothing,
      rDSMetadata = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing
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
dataSource_status :: Lens.Lens' DataSource (Prelude.Maybe EntityStatus)
dataSource_status = Lens.lens (\DataSource' {status} -> status) (\s@DataSource' {} a -> s {status = a} :: DataSource)

-- | Undocumented member.
dataSource_startedAt :: Lens.Lens' DataSource (Prelude.Maybe Prelude.UTCTime)
dataSource_startedAt = Lens.lens (\DataSource' {startedAt} -> startedAt) (\s@DataSource' {} a -> s {startedAt = a} :: DataSource) Prelude.. Lens.mapping Prelude._Time

-- | A JSON string that represents the splitting and rearrangement
-- requirement used when this @DataSource@ was created.
dataSource_dataRearrangement :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_dataRearrangement = Lens.lens (\DataSource' {dataRearrangement} -> dataRearrangement) (\s@DataSource' {} a -> s {dataRearrangement = a} :: DataSource)

-- | Undocumented member.
dataSource_roleARN :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_roleARN = Lens.lens (\DataSource' {roleARN} -> roleARN) (\s@DataSource' {} a -> s {roleARN = a} :: DataSource)

-- | Undocumented member.
dataSource_redshiftMetadata :: Lens.Lens' DataSource (Prelude.Maybe RedshiftMetadata)
dataSource_redshiftMetadata = Lens.lens (\DataSource' {redshiftMetadata} -> redshiftMetadata) (\s@DataSource' {} a -> s {redshiftMetadata = a} :: DataSource)

-- | A description of the most recent details about creating the
-- @DataSource@.
dataSource_message :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_message = Lens.lens (\DataSource' {message} -> message) (\s@DataSource' {} a -> s {message = a} :: DataSource)

-- | The ID that is assigned to the @DataSource@ during creation.
dataSource_dataSourceId :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_dataSourceId = Lens.lens (\DataSource' {dataSourceId} -> dataSourceId) (\s@DataSource' {} a -> s {dataSourceId = a} :: DataSource)

-- | The parameter is @true@ if statistics need to be generated from the
-- observation data.
dataSource_computeStatistics :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Bool)
dataSource_computeStatistics = Lens.lens (\DataSource' {computeStatistics} -> computeStatistics) (\s@DataSource' {} a -> s {computeStatistics = a} :: DataSource)

-- | The location and name of the data in Amazon Simple Storage Service
-- (Amazon S3) that is used by a @DataSource@.
dataSource_dataLocationS3 :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_dataLocationS3 = Lens.lens (\DataSource' {dataLocationS3} -> dataLocationS3) (\s@DataSource' {} a -> s {dataLocationS3 = a} :: DataSource)

-- | The time that the @DataSource@ was created. The time is expressed in
-- epoch time.
dataSource_createdAt :: Lens.Lens' DataSource (Prelude.Maybe Prelude.UTCTime)
dataSource_createdAt = Lens.lens (\DataSource' {createdAt} -> createdAt) (\s@DataSource' {} a -> s {createdAt = a} :: DataSource) Prelude.. Lens.mapping Prelude._Time

-- | The number of data files referenced by the @DataSource@.
dataSource_numberOfFiles :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Integer)
dataSource_numberOfFiles = Lens.lens (\DataSource' {numberOfFiles} -> numberOfFiles) (\s@DataSource' {} a -> s {numberOfFiles = a} :: DataSource)

-- | Undocumented member.
dataSource_finishedAt :: Lens.Lens' DataSource (Prelude.Maybe Prelude.UTCTime)
dataSource_finishedAt = Lens.lens (\DataSource' {finishedAt} -> finishedAt) (\s@DataSource' {} a -> s {finishedAt = a} :: DataSource) Prelude.. Lens.mapping Prelude._Time

-- | The AWS user account from which the @DataSource@ was created. The
-- account type can be either an AWS root account or an AWS Identity and
-- Access Management (IAM) user account.
dataSource_createdByIamUser :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_createdByIamUser = Lens.lens (\DataSource' {createdByIamUser} -> createdByIamUser) (\s@DataSource' {} a -> s {createdByIamUser = a} :: DataSource)

-- | A user-supplied name or description of the @DataSource@.
dataSource_name :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_name = Lens.lens (\DataSource' {name} -> name) (\s@DataSource' {} a -> s {name = a} :: DataSource)

-- | The total number of observations contained in the data files that the
-- @DataSource@ references.
dataSource_dataSizeInBytes :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Integer)
dataSource_dataSizeInBytes = Lens.lens (\DataSource' {dataSizeInBytes} -> dataSizeInBytes) (\s@DataSource' {} a -> s {dataSizeInBytes = a} :: DataSource)

-- | Undocumented member.
dataSource_computeTime :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Integer)
dataSource_computeTime = Lens.lens (\DataSource' {computeTime} -> computeTime) (\s@DataSource' {} a -> s {computeTime = a} :: DataSource)

-- | Undocumented member.
dataSource_rDSMetadata :: Lens.Lens' DataSource (Prelude.Maybe RDSMetadata)
dataSource_rDSMetadata = Lens.lens (\DataSource' {rDSMetadata} -> rDSMetadata) (\s@DataSource' {} a -> s {rDSMetadata = a} :: DataSource)

-- | The time of the most recent edit to the @BatchPrediction@. The time is
-- expressed in epoch time.
dataSource_lastUpdatedAt :: Lens.Lens' DataSource (Prelude.Maybe Prelude.UTCTime)
dataSource_lastUpdatedAt = Lens.lens (\DataSource' {lastUpdatedAt} -> lastUpdatedAt) (\s@DataSource' {} a -> s {lastUpdatedAt = a} :: DataSource) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON DataSource where
  parseJSON =
    Prelude.withObject
      "DataSource"
      ( \x ->
          DataSource'
            Prelude.<$> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "StartedAt")
            Prelude.<*> (x Prelude..:? "DataRearrangement")
            Prelude.<*> (x Prelude..:? "RoleARN")
            Prelude.<*> (x Prelude..:? "RedshiftMetadata")
            Prelude.<*> (x Prelude..:? "Message")
            Prelude.<*> (x Prelude..:? "DataSourceId")
            Prelude.<*> (x Prelude..:? "ComputeStatistics")
            Prelude.<*> (x Prelude..:? "DataLocationS3")
            Prelude.<*> (x Prelude..:? "CreatedAt")
            Prelude.<*> (x Prelude..:? "NumberOfFiles")
            Prelude.<*> (x Prelude..:? "FinishedAt")
            Prelude.<*> (x Prelude..:? "CreatedByIamUser")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "DataSizeInBytes")
            Prelude.<*> (x Prelude..:? "ComputeTime")
            Prelude.<*> (x Prelude..:? "RDSMetadata")
            Prelude.<*> (x Prelude..:? "LastUpdatedAt")
      )

instance Prelude.Hashable DataSource

instance Prelude.NFData DataSource
