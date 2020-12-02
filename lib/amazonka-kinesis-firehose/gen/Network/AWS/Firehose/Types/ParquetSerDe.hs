{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ParquetSerDe
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ParquetSerDe where

import Network.AWS.Firehose.Types.ParquetCompression
import Network.AWS.Firehose.Types.ParquetWriterVersion
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A serializer to use for converting data to the Parquet format before storing it in Amazon S3. For more information, see <https://parquet.apache.org/documentation/latest/ Apache Parquet> .
--
--
--
-- /See:/ 'parquetSerDe' smart constructor.
data ParquetSerDe = ParquetSerDe'
  { _psdWriterVersion ::
      !(Maybe ParquetWriterVersion),
    _psdCompression :: !(Maybe ParquetCompression),
    _psdMaxPaddingBytes :: !(Maybe Nat),
    _psdEnableDictionaryCompression :: !(Maybe Bool),
    _psdPageSizeBytes :: !(Maybe Nat),
    _psdBlockSizeBytes :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ParquetSerDe' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psdWriterVersion' - Indicates the version of row format to output. The possible values are @V1@ and @V2@ . The default is @V1@ .
--
-- * 'psdCompression' - The compression code to use over data blocks. The possible values are @UNCOMPRESSED@ , @SNAPPY@ , and @GZIP@ , with the default being @SNAPPY@ . Use @SNAPPY@ for higher decompression speed. Use @GZIP@ if the compression ratio is more important than speed.
--
-- * 'psdMaxPaddingBytes' - The maximum amount of padding to apply. This is useful if you intend to copy the data from Amazon S3 to HDFS before querying. The default is 0.
--
-- * 'psdEnableDictionaryCompression' - Indicates whether to enable dictionary compression.
--
-- * 'psdPageSizeBytes' - The Parquet page size. Column chunks are divided into pages. A page is conceptually an indivisible unit (in terms of compression and encoding). The minimum value is 64 KiB and the default is 1 MiB.
--
-- * 'psdBlockSizeBytes' - The Hadoop Distributed File System (HDFS) block size. This is useful if you intend to copy the data from Amazon S3 to HDFS before querying. The default is 256 MiB and the minimum is 64 MiB. Kinesis Data Firehose uses this value for padding calculations.
parquetSerDe ::
  ParquetSerDe
parquetSerDe =
  ParquetSerDe'
    { _psdWriterVersion = Nothing,
      _psdCompression = Nothing,
      _psdMaxPaddingBytes = Nothing,
      _psdEnableDictionaryCompression = Nothing,
      _psdPageSizeBytes = Nothing,
      _psdBlockSizeBytes = Nothing
    }

-- | Indicates the version of row format to output. The possible values are @V1@ and @V2@ . The default is @V1@ .
psdWriterVersion :: Lens' ParquetSerDe (Maybe ParquetWriterVersion)
psdWriterVersion = lens _psdWriterVersion (\s a -> s {_psdWriterVersion = a})

-- | The compression code to use over data blocks. The possible values are @UNCOMPRESSED@ , @SNAPPY@ , and @GZIP@ , with the default being @SNAPPY@ . Use @SNAPPY@ for higher decompression speed. Use @GZIP@ if the compression ratio is more important than speed.
psdCompression :: Lens' ParquetSerDe (Maybe ParquetCompression)
psdCompression = lens _psdCompression (\s a -> s {_psdCompression = a})

-- | The maximum amount of padding to apply. This is useful if you intend to copy the data from Amazon S3 to HDFS before querying. The default is 0.
psdMaxPaddingBytes :: Lens' ParquetSerDe (Maybe Natural)
psdMaxPaddingBytes = lens _psdMaxPaddingBytes (\s a -> s {_psdMaxPaddingBytes = a}) . mapping _Nat

-- | Indicates whether to enable dictionary compression.
psdEnableDictionaryCompression :: Lens' ParquetSerDe (Maybe Bool)
psdEnableDictionaryCompression = lens _psdEnableDictionaryCompression (\s a -> s {_psdEnableDictionaryCompression = a})

-- | The Parquet page size. Column chunks are divided into pages. A page is conceptually an indivisible unit (in terms of compression and encoding). The minimum value is 64 KiB and the default is 1 MiB.
psdPageSizeBytes :: Lens' ParquetSerDe (Maybe Natural)
psdPageSizeBytes = lens _psdPageSizeBytes (\s a -> s {_psdPageSizeBytes = a}) . mapping _Nat

-- | The Hadoop Distributed File System (HDFS) block size. This is useful if you intend to copy the data from Amazon S3 to HDFS before querying. The default is 256 MiB and the minimum is 64 MiB. Kinesis Data Firehose uses this value for padding calculations.
psdBlockSizeBytes :: Lens' ParquetSerDe (Maybe Natural)
psdBlockSizeBytes = lens _psdBlockSizeBytes (\s a -> s {_psdBlockSizeBytes = a}) . mapping _Nat

instance FromJSON ParquetSerDe where
  parseJSON =
    withObject
      "ParquetSerDe"
      ( \x ->
          ParquetSerDe'
            <$> (x .:? "WriterVersion")
            <*> (x .:? "Compression")
            <*> (x .:? "MaxPaddingBytes")
            <*> (x .:? "EnableDictionaryCompression")
            <*> (x .:? "PageSizeBytes")
            <*> (x .:? "BlockSizeBytes")
      )

instance Hashable ParquetSerDe

instance NFData ParquetSerDe

instance ToJSON ParquetSerDe where
  toJSON ParquetSerDe' {..} =
    object
      ( catMaybes
          [ ("WriterVersion" .=) <$> _psdWriterVersion,
            ("Compression" .=) <$> _psdCompression,
            ("MaxPaddingBytes" .=) <$> _psdMaxPaddingBytes,
            ("EnableDictionaryCompression" .=)
              <$> _psdEnableDictionaryCompression,
            ("PageSizeBytes" .=) <$> _psdPageSizeBytes,
            ("BlockSizeBytes" .=) <$> _psdBlockSizeBytes
          ]
      )
