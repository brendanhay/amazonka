{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.Serializer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.Serializer where

import Network.AWS.Firehose.Types.OrcSerDe
import Network.AWS.Firehose.Types.ParquetSerDe
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The serializer that you want Kinesis Data Firehose to use to convert data to the target format before writing it to Amazon S3. Kinesis Data Firehose supports two types of serializers: the <https://hive.apache.org/javadocs/r1.2.2/api/org/apache/hadoop/hive/ql/io/orc/OrcSerde.html ORC SerDe> and the <https://hive.apache.org/javadocs/r1.2.2/api/org/apache/hadoop/hive/ql/io/parquet/serde/ParquetHiveSerDe.html Parquet SerDe> .
--
--
--
-- /See:/ 'serializer' smart constructor.
data Serializer = Serializer'
  { _sOrcSerDe :: !(Maybe OrcSerDe),
    _sParquetSerDe :: !(Maybe ParquetSerDe)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Serializer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sOrcSerDe' - A serializer to use for converting data to the ORC format before storing it in Amazon S3. For more information, see <https://orc.apache.org/docs/ Apache ORC> .
--
-- * 'sParquetSerDe' - A serializer to use for converting data to the Parquet format before storing it in Amazon S3. For more information, see <https://parquet.apache.org/documentation/latest/ Apache Parquet> .
serializer ::
  Serializer
serializer =
  Serializer' {_sOrcSerDe = Nothing, _sParquetSerDe = Nothing}

-- | A serializer to use for converting data to the ORC format before storing it in Amazon S3. For more information, see <https://orc.apache.org/docs/ Apache ORC> .
sOrcSerDe :: Lens' Serializer (Maybe OrcSerDe)
sOrcSerDe = lens _sOrcSerDe (\s a -> s {_sOrcSerDe = a})

-- | A serializer to use for converting data to the Parquet format before storing it in Amazon S3. For more information, see <https://parquet.apache.org/documentation/latest/ Apache Parquet> .
sParquetSerDe :: Lens' Serializer (Maybe ParquetSerDe)
sParquetSerDe = lens _sParquetSerDe (\s a -> s {_sParquetSerDe = a})

instance FromJSON Serializer where
  parseJSON =
    withObject
      "Serializer"
      ( \x ->
          Serializer' <$> (x .:? "OrcSerDe") <*> (x .:? "ParquetSerDe")
      )

instance Hashable Serializer

instance NFData Serializer

instance ToJSON Serializer where
  toJSON Serializer' {..} =
    object
      ( catMaybes
          [ ("OrcSerDe" .=) <$> _sOrcSerDe,
            ("ParquetSerDe" .=) <$> _sParquetSerDe
          ]
      )
