{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.ReferenceDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.ReferenceDataSource where

import Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSource
import Network.AWS.KinesisAnalytics.Types.SourceSchema
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the reference data source by providing the source information (S3 bucket name and object key name), the resulting in-application table name that is created, and the necessary schema to map the data elements in the Amazon S3 object to the in-application table.
--
--
--
-- /See:/ 'referenceDataSource' smart constructor.
data ReferenceDataSource = ReferenceDataSource'
  { _rdsS3ReferenceDataSource ::
      !(Maybe S3ReferenceDataSource),
    _rdsTableName :: !Text,
    _rdsReferenceSchema :: !SourceSchema
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReferenceDataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdsS3ReferenceDataSource' - Identifies the S3 bucket and object that contains the reference data. Also identifies the IAM role Amazon Kinesis Analytics can assume to read this object on your behalf. An Amazon Kinesis Analytics application loads reference data only once. If the data changes, you call the @UpdateApplication@ operation to trigger reloading of data into your application.
--
-- * 'rdsTableName' - Name of the in-application table to create.
--
-- * 'rdsReferenceSchema' - Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.
referenceDataSource ::
  -- | 'rdsTableName'
  Text ->
  -- | 'rdsReferenceSchema'
  SourceSchema ->
  ReferenceDataSource
referenceDataSource pTableName_ pReferenceSchema_ =
  ReferenceDataSource'
    { _rdsS3ReferenceDataSource = Nothing,
      _rdsTableName = pTableName_,
      _rdsReferenceSchema = pReferenceSchema_
    }

-- | Identifies the S3 bucket and object that contains the reference data. Also identifies the IAM role Amazon Kinesis Analytics can assume to read this object on your behalf. An Amazon Kinesis Analytics application loads reference data only once. If the data changes, you call the @UpdateApplication@ operation to trigger reloading of data into your application.
rdsS3ReferenceDataSource :: Lens' ReferenceDataSource (Maybe S3ReferenceDataSource)
rdsS3ReferenceDataSource = lens _rdsS3ReferenceDataSource (\s a -> s {_rdsS3ReferenceDataSource = a})

-- | Name of the in-application table to create.
rdsTableName :: Lens' ReferenceDataSource Text
rdsTableName = lens _rdsTableName (\s a -> s {_rdsTableName = a})

-- | Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.
rdsReferenceSchema :: Lens' ReferenceDataSource SourceSchema
rdsReferenceSchema = lens _rdsReferenceSchema (\s a -> s {_rdsReferenceSchema = a})

instance Hashable ReferenceDataSource

instance NFData ReferenceDataSource

instance ToJSON ReferenceDataSource where
  toJSON ReferenceDataSource' {..} =
    object
      ( catMaybes
          [ ("S3ReferenceDataSource" .=) <$> _rdsS3ReferenceDataSource,
            Just ("TableName" .= _rdsTableName),
            Just ("ReferenceSchema" .= _rdsReferenceSchema)
          ]
      )
