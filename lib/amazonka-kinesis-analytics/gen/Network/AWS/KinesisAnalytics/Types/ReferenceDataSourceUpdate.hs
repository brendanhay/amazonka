{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceUpdate where

import Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceUpdate
import Network.AWS.KinesisAnalytics.Types.SourceSchema
import Network.AWS.Lens
import Network.AWS.Prelude

-- | When you update a reference data source configuration for an application, this object provides all the updated values (such as the source bucket name and object key name), the in-application table name that is created, and updated mapping information that maps the data in the Amazon S3 object to the in-application reference table that is created.
--
--
--
-- /See:/ 'referenceDataSourceUpdate' smart constructor.
data ReferenceDataSourceUpdate = ReferenceDataSourceUpdate'
  { _rdsuTableNameUpdate ::
      !(Maybe Text),
    _rdsuS3ReferenceDataSourceUpdate ::
      !(Maybe S3ReferenceDataSourceUpdate),
    _rdsuReferenceSchemaUpdate ::
      !(Maybe SourceSchema),
    _rdsuReferenceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReferenceDataSourceUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdsuTableNameUpdate' - In-application table name that is created by this update.
--
-- * 'rdsuS3ReferenceDataSourceUpdate' - Describes the S3 bucket name, object key name, and IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object on your behalf and populate the in-application reference table.
--
-- * 'rdsuReferenceSchemaUpdate' - Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.
--
-- * 'rdsuReferenceId' - ID of the reference data source being updated. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get this value.
referenceDataSourceUpdate ::
  -- | 'rdsuReferenceId'
  Text ->
  ReferenceDataSourceUpdate
referenceDataSourceUpdate pReferenceId_ =
  ReferenceDataSourceUpdate'
    { _rdsuTableNameUpdate = Nothing,
      _rdsuS3ReferenceDataSourceUpdate = Nothing,
      _rdsuReferenceSchemaUpdate = Nothing,
      _rdsuReferenceId = pReferenceId_
    }

-- | In-application table name that is created by this update.
rdsuTableNameUpdate :: Lens' ReferenceDataSourceUpdate (Maybe Text)
rdsuTableNameUpdate = lens _rdsuTableNameUpdate (\s a -> s {_rdsuTableNameUpdate = a})

-- | Describes the S3 bucket name, object key name, and IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object on your behalf and populate the in-application reference table.
rdsuS3ReferenceDataSourceUpdate :: Lens' ReferenceDataSourceUpdate (Maybe S3ReferenceDataSourceUpdate)
rdsuS3ReferenceDataSourceUpdate = lens _rdsuS3ReferenceDataSourceUpdate (\s a -> s {_rdsuS3ReferenceDataSourceUpdate = a})

-- | Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.
rdsuReferenceSchemaUpdate :: Lens' ReferenceDataSourceUpdate (Maybe SourceSchema)
rdsuReferenceSchemaUpdate = lens _rdsuReferenceSchemaUpdate (\s a -> s {_rdsuReferenceSchemaUpdate = a})

-- | ID of the reference data source being updated. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get this value.
rdsuReferenceId :: Lens' ReferenceDataSourceUpdate Text
rdsuReferenceId = lens _rdsuReferenceId (\s a -> s {_rdsuReferenceId = a})

instance Hashable ReferenceDataSourceUpdate

instance NFData ReferenceDataSourceUpdate

instance ToJSON ReferenceDataSourceUpdate where
  toJSON ReferenceDataSourceUpdate' {..} =
    object
      ( catMaybes
          [ ("TableNameUpdate" .=) <$> _rdsuTableNameUpdate,
            ("S3ReferenceDataSourceUpdate" .=)
              <$> _rdsuS3ReferenceDataSourceUpdate,
            ("ReferenceSchemaUpdate" .=) <$> _rdsuReferenceSchemaUpdate,
            Just ("ReferenceId" .= _rdsuReferenceId)
          ]
      )
