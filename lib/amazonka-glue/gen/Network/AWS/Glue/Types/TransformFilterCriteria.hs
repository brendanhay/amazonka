{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TransformFilterCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TransformFilterCriteria where

import Network.AWS.Glue.Types.SchemaColumn
import Network.AWS.Glue.Types.TransformStatusType
import Network.AWS.Glue.Types.TransformType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The criteria used to filter the machine learning transforms.
--
--
--
-- /See:/ 'transformFilterCriteria' smart constructor.
data TransformFilterCriteria = TransformFilterCriteria'
  { _tfcCreatedAfter ::
      !(Maybe POSIX),
    _tfcStatus :: !(Maybe TransformStatusType),
    _tfcLastModifiedAfter :: !(Maybe POSIX),
    _tfcLastModifiedBefore :: !(Maybe POSIX),
    _tfcGlueVersion :: !(Maybe Text),
    _tfcSchema :: !(Maybe [SchemaColumn]),
    _tfcTransformType :: !(Maybe TransformType),
    _tfcName :: !(Maybe Text),
    _tfcCreatedBefore :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransformFilterCriteria' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tfcCreatedAfter' - The time and date after which the transforms were created.
--
-- * 'tfcStatus' - Filters the list of machine learning transforms by the last known status of the transforms (to indicate whether a transform can be used or not). One of "NOT_READY", "READY", or "DELETING".
--
-- * 'tfcLastModifiedAfter' - Filter on transforms last modified after this date.
--
-- * 'tfcLastModifiedBefore' - Filter on transforms last modified before this date.
--
-- * 'tfcGlueVersion' - This value determines which version of AWS Glue this machine learning transform is compatible with. Glue 1.0 is recommended for most customers. If the value is not set, the Glue compatibility defaults to Glue 0.9. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions> in the developer guide.
--
-- * 'tfcSchema' - Filters on datasets with a specific schema. The @Map<Column, Type>@ object is an array of key-value pairs representing the schema this transform accepts, where @Column@ is the name of a column, and @Type@ is the type of the data such as an integer or string. Has an upper bound of 100 columns.
--
-- * 'tfcTransformType' - The type of machine learning transform that is used to filter the machine learning transforms.
--
-- * 'tfcName' - A unique transform name that is used to filter the machine learning transforms.
--
-- * 'tfcCreatedBefore' - The time and date before which the transforms were created.
transformFilterCriteria ::
  TransformFilterCriteria
transformFilterCriteria =
  TransformFilterCriteria'
    { _tfcCreatedAfter = Nothing,
      _tfcStatus = Nothing,
      _tfcLastModifiedAfter = Nothing,
      _tfcLastModifiedBefore = Nothing,
      _tfcGlueVersion = Nothing,
      _tfcSchema = Nothing,
      _tfcTransformType = Nothing,
      _tfcName = Nothing,
      _tfcCreatedBefore = Nothing
    }

-- | The time and date after which the transforms were created.
tfcCreatedAfter :: Lens' TransformFilterCriteria (Maybe UTCTime)
tfcCreatedAfter = lens _tfcCreatedAfter (\s a -> s {_tfcCreatedAfter = a}) . mapping _Time

-- | Filters the list of machine learning transforms by the last known status of the transforms (to indicate whether a transform can be used or not). One of "NOT_READY", "READY", or "DELETING".
tfcStatus :: Lens' TransformFilterCriteria (Maybe TransformStatusType)
tfcStatus = lens _tfcStatus (\s a -> s {_tfcStatus = a})

-- | Filter on transforms last modified after this date.
tfcLastModifiedAfter :: Lens' TransformFilterCriteria (Maybe UTCTime)
tfcLastModifiedAfter = lens _tfcLastModifiedAfter (\s a -> s {_tfcLastModifiedAfter = a}) . mapping _Time

-- | Filter on transforms last modified before this date.
tfcLastModifiedBefore :: Lens' TransformFilterCriteria (Maybe UTCTime)
tfcLastModifiedBefore = lens _tfcLastModifiedBefore (\s a -> s {_tfcLastModifiedBefore = a}) . mapping _Time

-- | This value determines which version of AWS Glue this machine learning transform is compatible with. Glue 1.0 is recommended for most customers. If the value is not set, the Glue compatibility defaults to Glue 0.9. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions> in the developer guide.
tfcGlueVersion :: Lens' TransformFilterCriteria (Maybe Text)
tfcGlueVersion = lens _tfcGlueVersion (\s a -> s {_tfcGlueVersion = a})

-- | Filters on datasets with a specific schema. The @Map<Column, Type>@ object is an array of key-value pairs representing the schema this transform accepts, where @Column@ is the name of a column, and @Type@ is the type of the data such as an integer or string. Has an upper bound of 100 columns.
tfcSchema :: Lens' TransformFilterCriteria [SchemaColumn]
tfcSchema = lens _tfcSchema (\s a -> s {_tfcSchema = a}) . _Default . _Coerce

-- | The type of machine learning transform that is used to filter the machine learning transforms.
tfcTransformType :: Lens' TransformFilterCriteria (Maybe TransformType)
tfcTransformType = lens _tfcTransformType (\s a -> s {_tfcTransformType = a})

-- | A unique transform name that is used to filter the machine learning transforms.
tfcName :: Lens' TransformFilterCriteria (Maybe Text)
tfcName = lens _tfcName (\s a -> s {_tfcName = a})

-- | The time and date before which the transforms were created.
tfcCreatedBefore :: Lens' TransformFilterCriteria (Maybe UTCTime)
tfcCreatedBefore = lens _tfcCreatedBefore (\s a -> s {_tfcCreatedBefore = a}) . mapping _Time

instance Hashable TransformFilterCriteria

instance NFData TransformFilterCriteria

instance ToJSON TransformFilterCriteria where
  toJSON TransformFilterCriteria' {..} =
    object
      ( catMaybes
          [ ("CreatedAfter" .=) <$> _tfcCreatedAfter,
            ("Status" .=) <$> _tfcStatus,
            ("LastModifiedAfter" .=) <$> _tfcLastModifiedAfter,
            ("LastModifiedBefore" .=) <$> _tfcLastModifiedBefore,
            ("GlueVersion" .=) <$> _tfcGlueVersion,
            ("Schema" .=) <$> _tfcSchema,
            ("TransformType" .=) <$> _tfcTransformType,
            ("Name" .=) <$> _tfcName,
            ("CreatedBefore" .=) <$> _tfcCreatedBefore
          ]
      )
