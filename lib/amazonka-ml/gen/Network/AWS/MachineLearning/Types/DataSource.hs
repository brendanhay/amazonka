{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.DataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.DataSource where

import Network.AWS.Lens
import Network.AWS.MachineLearning.Types.EntityStatus
import Network.AWS.MachineLearning.Types.RDSMetadata
import Network.AWS.MachineLearning.Types.RedshiftMetadata
import Network.AWS.Prelude

-- | Represents the output of the @GetDataSource@ operation.
--
--
-- The content consists of the detailed metadata and data file information and the current status of the @DataSource@ .
--
--
-- /See:/ 'dataSource' smart constructor.
data DataSource = DataSource'
  { _dsStatus :: !(Maybe EntityStatus),
    _dsNumberOfFiles :: !(Maybe Integer),
    _dsLastUpdatedAt :: !(Maybe POSIX),
    _dsCreatedAt :: !(Maybe POSIX),
    _dsComputeTime :: !(Maybe Integer),
    _dsDataSourceId :: !(Maybe Text),
    _dsRDSMetadata :: !(Maybe RDSMetadata),
    _dsDataSizeInBytes :: !(Maybe Integer),
    _dsStartedAt :: !(Maybe POSIX),
    _dsFinishedAt :: !(Maybe POSIX),
    _dsCreatedByIAMUser :: !(Maybe Text),
    _dsName :: !(Maybe Text),
    _dsDataLocationS3 :: !(Maybe Text),
    _dsComputeStatistics :: !(Maybe Bool),
    _dsMessage :: !(Maybe Text),
    _dsRedshiftMetadata :: !(Maybe RedshiftMetadata),
    _dsDataRearrangement :: !(Maybe Text),
    _dsRoleARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsStatus' - The current status of the @DataSource@ . This element can have one of the following values:      * PENDING - Amazon Machine Learning (Amazon ML) submitted a request to create a @DataSource@ .    * INPROGRESS - The creation process is underway.    * FAILED - The request to create a @DataSource@ did not run to completion. It is not usable.    * COMPLETED - The creation process completed successfully.    * DELETED - The @DataSource@ is marked as deleted. It is not usable.
--
-- * 'dsNumberOfFiles' - The number of data files referenced by the @DataSource@ .
--
-- * 'dsLastUpdatedAt' - The time of the most recent edit to the @BatchPrediction@ . The time is expressed in epoch time.
--
-- * 'dsCreatedAt' - The time that the @DataSource@ was created. The time is expressed in epoch time.
--
-- * 'dsComputeTime' - Undocumented member.
--
-- * 'dsDataSourceId' - The ID that is assigned to the @DataSource@ during creation.
--
-- * 'dsRDSMetadata' - Undocumented member.
--
-- * 'dsDataSizeInBytes' - The total number of observations contained in the data files that the @DataSource@ references.
--
-- * 'dsStartedAt' - Undocumented member.
--
-- * 'dsFinishedAt' - Undocumented member.
--
-- * 'dsCreatedByIAMUser' - The AWS user account from which the @DataSource@ was created. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
--
-- * 'dsName' - A user-supplied name or description of the @DataSource@ .
--
-- * 'dsDataLocationS3' - The location and name of the data in Amazon Simple Storage Service (Amazon S3) that is used by a @DataSource@ .
--
-- * 'dsComputeStatistics' - The parameter is @true@ if statistics need to be generated from the observation data.
--
-- * 'dsMessage' - A description of the most recent details about creating the @DataSource@ .
--
-- * 'dsRedshiftMetadata' - Undocumented member.
--
-- * 'dsDataRearrangement' - A JSON string that represents the splitting and rearrangement requirement used when this @DataSource@ was created.
--
-- * 'dsRoleARN' - Undocumented member.
dataSource ::
  DataSource
dataSource =
  DataSource'
    { _dsStatus = Nothing,
      _dsNumberOfFiles = Nothing,
      _dsLastUpdatedAt = Nothing,
      _dsCreatedAt = Nothing,
      _dsComputeTime = Nothing,
      _dsDataSourceId = Nothing,
      _dsRDSMetadata = Nothing,
      _dsDataSizeInBytes = Nothing,
      _dsStartedAt = Nothing,
      _dsFinishedAt = Nothing,
      _dsCreatedByIAMUser = Nothing,
      _dsName = Nothing,
      _dsDataLocationS3 = Nothing,
      _dsComputeStatistics = Nothing,
      _dsMessage = Nothing,
      _dsRedshiftMetadata = Nothing,
      _dsDataRearrangement = Nothing,
      _dsRoleARN = Nothing
    }

-- | The current status of the @DataSource@ . This element can have one of the following values:      * PENDING - Amazon Machine Learning (Amazon ML) submitted a request to create a @DataSource@ .    * INPROGRESS - The creation process is underway.    * FAILED - The request to create a @DataSource@ did not run to completion. It is not usable.    * COMPLETED - The creation process completed successfully.    * DELETED - The @DataSource@ is marked as deleted. It is not usable.
dsStatus :: Lens' DataSource (Maybe EntityStatus)
dsStatus = lens _dsStatus (\s a -> s {_dsStatus = a})

-- | The number of data files referenced by the @DataSource@ .
dsNumberOfFiles :: Lens' DataSource (Maybe Integer)
dsNumberOfFiles = lens _dsNumberOfFiles (\s a -> s {_dsNumberOfFiles = a})

-- | The time of the most recent edit to the @BatchPrediction@ . The time is expressed in epoch time.
dsLastUpdatedAt :: Lens' DataSource (Maybe UTCTime)
dsLastUpdatedAt = lens _dsLastUpdatedAt (\s a -> s {_dsLastUpdatedAt = a}) . mapping _Time

-- | The time that the @DataSource@ was created. The time is expressed in epoch time.
dsCreatedAt :: Lens' DataSource (Maybe UTCTime)
dsCreatedAt = lens _dsCreatedAt (\s a -> s {_dsCreatedAt = a}) . mapping _Time

-- | Undocumented member.
dsComputeTime :: Lens' DataSource (Maybe Integer)
dsComputeTime = lens _dsComputeTime (\s a -> s {_dsComputeTime = a})

-- | The ID that is assigned to the @DataSource@ during creation.
dsDataSourceId :: Lens' DataSource (Maybe Text)
dsDataSourceId = lens _dsDataSourceId (\s a -> s {_dsDataSourceId = a})

-- | Undocumented member.
dsRDSMetadata :: Lens' DataSource (Maybe RDSMetadata)
dsRDSMetadata = lens _dsRDSMetadata (\s a -> s {_dsRDSMetadata = a})

-- | The total number of observations contained in the data files that the @DataSource@ references.
dsDataSizeInBytes :: Lens' DataSource (Maybe Integer)
dsDataSizeInBytes = lens _dsDataSizeInBytes (\s a -> s {_dsDataSizeInBytes = a})

-- | Undocumented member.
dsStartedAt :: Lens' DataSource (Maybe UTCTime)
dsStartedAt = lens _dsStartedAt (\s a -> s {_dsStartedAt = a}) . mapping _Time

-- | Undocumented member.
dsFinishedAt :: Lens' DataSource (Maybe UTCTime)
dsFinishedAt = lens _dsFinishedAt (\s a -> s {_dsFinishedAt = a}) . mapping _Time

-- | The AWS user account from which the @DataSource@ was created. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
dsCreatedByIAMUser :: Lens' DataSource (Maybe Text)
dsCreatedByIAMUser = lens _dsCreatedByIAMUser (\s a -> s {_dsCreatedByIAMUser = a})

-- | A user-supplied name or description of the @DataSource@ .
dsName :: Lens' DataSource (Maybe Text)
dsName = lens _dsName (\s a -> s {_dsName = a})

-- | The location and name of the data in Amazon Simple Storage Service (Amazon S3) that is used by a @DataSource@ .
dsDataLocationS3 :: Lens' DataSource (Maybe Text)
dsDataLocationS3 = lens _dsDataLocationS3 (\s a -> s {_dsDataLocationS3 = a})

-- | The parameter is @true@ if statistics need to be generated from the observation data.
dsComputeStatistics :: Lens' DataSource (Maybe Bool)
dsComputeStatistics = lens _dsComputeStatistics (\s a -> s {_dsComputeStatistics = a})

-- | A description of the most recent details about creating the @DataSource@ .
dsMessage :: Lens' DataSource (Maybe Text)
dsMessage = lens _dsMessage (\s a -> s {_dsMessage = a})

-- | Undocumented member.
dsRedshiftMetadata :: Lens' DataSource (Maybe RedshiftMetadata)
dsRedshiftMetadata = lens _dsRedshiftMetadata (\s a -> s {_dsRedshiftMetadata = a})

-- | A JSON string that represents the splitting and rearrangement requirement used when this @DataSource@ was created.
dsDataRearrangement :: Lens' DataSource (Maybe Text)
dsDataRearrangement = lens _dsDataRearrangement (\s a -> s {_dsDataRearrangement = a})

-- | Undocumented member.
dsRoleARN :: Lens' DataSource (Maybe Text)
dsRoleARN = lens _dsRoleARN (\s a -> s {_dsRoleARN = a})

instance FromJSON DataSource where
  parseJSON =
    withObject
      "DataSource"
      ( \x ->
          DataSource'
            <$> (x .:? "Status")
            <*> (x .:? "NumberOfFiles")
            <*> (x .:? "LastUpdatedAt")
            <*> (x .:? "CreatedAt")
            <*> (x .:? "ComputeTime")
            <*> (x .:? "DataSourceId")
            <*> (x .:? "RDSMetadata")
            <*> (x .:? "DataSizeInBytes")
            <*> (x .:? "StartedAt")
            <*> (x .:? "FinishedAt")
            <*> (x .:? "CreatedByIamUser")
            <*> (x .:? "Name")
            <*> (x .:? "DataLocationS3")
            <*> (x .:? "ComputeStatistics")
            <*> (x .:? "Message")
            <*> (x .:? "RedshiftMetadata")
            <*> (x .:? "DataRearrangement")
            <*> (x .:? "RoleARN")
      )

instance Hashable DataSource

instance NFData DataSource
