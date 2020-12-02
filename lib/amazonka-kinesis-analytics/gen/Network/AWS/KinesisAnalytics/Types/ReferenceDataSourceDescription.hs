{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceDescription where

import Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceDescription
import Network.AWS.KinesisAnalytics.Types.SourceSchema
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the reference data source configured for an application.
--
--
--
-- /See:/ 'referenceDataSourceDescription' smart constructor.
data ReferenceDataSourceDescription = ReferenceDataSourceDescription'
  { _rdsdReferenceSchema ::
      !(Maybe SourceSchema),
    _rdsdReferenceId :: !Text,
    _rdsdTableName :: !Text,
    _rdsdS3ReferenceDataSourceDescription ::
      !S3ReferenceDataSourceDescription
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReferenceDataSourceDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdsdReferenceSchema' - Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.
--
-- * 'rdsdReferenceId' - ID of the reference data source. This is the ID that Amazon Kinesis Analytics assigns when you add the reference data source to your application using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_AddApplicationReferenceDataSource.html AddApplicationReferenceDataSource> operation.
--
-- * 'rdsdTableName' - The in-application table name created by the specific reference data source configuration.
--
-- * 'rdsdS3ReferenceDataSourceDescription' - Provides the S3 bucket name, the object key name that contains the reference data. It also provides the Amazon Resource Name (ARN) of the IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object and populate the in-application reference table.
referenceDataSourceDescription ::
  -- | 'rdsdReferenceId'
  Text ->
  -- | 'rdsdTableName'
  Text ->
  -- | 'rdsdS3ReferenceDataSourceDescription'
  S3ReferenceDataSourceDescription ->
  ReferenceDataSourceDescription
referenceDataSourceDescription
  pReferenceId_
  pTableName_
  pS3ReferenceDataSourceDescription_ =
    ReferenceDataSourceDescription'
      { _rdsdReferenceSchema = Nothing,
        _rdsdReferenceId = pReferenceId_,
        _rdsdTableName = pTableName_,
        _rdsdS3ReferenceDataSourceDescription =
          pS3ReferenceDataSourceDescription_
      }

-- | Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.
rdsdReferenceSchema :: Lens' ReferenceDataSourceDescription (Maybe SourceSchema)
rdsdReferenceSchema = lens _rdsdReferenceSchema (\s a -> s {_rdsdReferenceSchema = a})

-- | ID of the reference data source. This is the ID that Amazon Kinesis Analytics assigns when you add the reference data source to your application using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_AddApplicationReferenceDataSource.html AddApplicationReferenceDataSource> operation.
rdsdReferenceId :: Lens' ReferenceDataSourceDescription Text
rdsdReferenceId = lens _rdsdReferenceId (\s a -> s {_rdsdReferenceId = a})

-- | The in-application table name created by the specific reference data source configuration.
rdsdTableName :: Lens' ReferenceDataSourceDescription Text
rdsdTableName = lens _rdsdTableName (\s a -> s {_rdsdTableName = a})

-- | Provides the S3 bucket name, the object key name that contains the reference data. It also provides the Amazon Resource Name (ARN) of the IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object and populate the in-application reference table.
rdsdS3ReferenceDataSourceDescription :: Lens' ReferenceDataSourceDescription S3ReferenceDataSourceDescription
rdsdS3ReferenceDataSourceDescription = lens _rdsdS3ReferenceDataSourceDescription (\s a -> s {_rdsdS3ReferenceDataSourceDescription = a})

instance FromJSON ReferenceDataSourceDescription where
  parseJSON =
    withObject
      "ReferenceDataSourceDescription"
      ( \x ->
          ReferenceDataSourceDescription'
            <$> (x .:? "ReferenceSchema")
            <*> (x .: "ReferenceId")
            <*> (x .: "TableName")
            <*> (x .: "S3ReferenceDataSourceDescription")
      )

instance Hashable ReferenceDataSourceDescription

instance NFData ReferenceDataSourceDescription
