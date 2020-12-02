{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.InputSerialization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.InputSerialization where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.CSVInput
import Network.AWS.S3.Types.CompressionType
import Network.AWS.S3.Types.JSONInput
import Network.AWS.S3.Types.ParquetInput

-- | Describes the serialization format of the object.
--
--
--
-- /See:/ 'inputSerialization' smart constructor.
data InputSerialization = InputSerialization'
  { _isJSON ::
      !(Maybe JSONInput),
    _isCSV :: !(Maybe CSVInput),
    _isParquet :: !(Maybe ParquetInput),
    _isCompressionType :: !(Maybe CompressionType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputSerialization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isJSON' - Specifies JSON as object's input serialization format.
--
-- * 'isCSV' - Describes the serialization of a CSV-encoded object.
--
-- * 'isParquet' - Specifies Parquet as object's input serialization format.
--
-- * 'isCompressionType' - Specifies object's compression format. Valid values: NONE, GZIP, BZIP2. Default Value: NONE.
inputSerialization ::
  InputSerialization
inputSerialization =
  InputSerialization'
    { _isJSON = Nothing,
      _isCSV = Nothing,
      _isParquet = Nothing,
      _isCompressionType = Nothing
    }

-- | Specifies JSON as object's input serialization format.
isJSON :: Lens' InputSerialization (Maybe JSONInput)
isJSON = lens _isJSON (\s a -> s {_isJSON = a})

-- | Describes the serialization of a CSV-encoded object.
isCSV :: Lens' InputSerialization (Maybe CSVInput)
isCSV = lens _isCSV (\s a -> s {_isCSV = a})

-- | Specifies Parquet as object's input serialization format.
isParquet :: Lens' InputSerialization (Maybe ParquetInput)
isParquet = lens _isParquet (\s a -> s {_isParquet = a})

-- | Specifies object's compression format. Valid values: NONE, GZIP, BZIP2. Default Value: NONE.
isCompressionType :: Lens' InputSerialization (Maybe CompressionType)
isCompressionType = lens _isCompressionType (\s a -> s {_isCompressionType = a})

instance Hashable InputSerialization

instance NFData InputSerialization

instance ToXML InputSerialization where
  toXML InputSerialization' {..} =
    mconcat
      [ "JSON" @= _isJSON,
        "CSV" @= _isCSV,
        "Parquet" @= _isParquet,
        "CompressionType" @= _isCompressionType
      ]
