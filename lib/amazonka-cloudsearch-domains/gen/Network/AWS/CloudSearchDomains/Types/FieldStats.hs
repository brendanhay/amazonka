{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.FieldStats
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.FieldStats where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The statistics for a field calculated in the request.
--
--
--
-- /See:/ 'fieldStats' smart constructor.
data FieldStats = FieldStats'
  { _fsMax :: !(Maybe Text),
    _fsMean :: !(Maybe Text),
    _fsCount :: !(Maybe Integer),
    _fsMissing :: !(Maybe Integer),
    _fsStddev :: !(Maybe Double),
    _fsMin :: !(Maybe Text),
    _fsSumOfSquares :: !(Maybe Double),
    _fsSum :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FieldStats' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fsMax' - The maximum value found in the specified field in the result set. If the field is numeric (@int@ , @int-array@ , @double@ , or @double-array@ ), @max@ is the string representation of a double-precision 64-bit floating point value. If the field is @date@ or @date-array@ , @max@ is the string representation of a date with the format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339> : yyyy-mm-ddTHH:mm:ss.SSSZ.
--
-- * 'fsMean' - The average of the values found in the specified field in the result set. If the field is numeric (@int@ , @int-array@ , @double@ , or @double-array@ ), @mean@ is the string representation of a double-precision 64-bit floating point value. If the field is @date@ or @date-array@ , @mean@ is the string representation of a date with the format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339> : yyyy-mm-ddTHH:mm:ss.SSSZ.
--
-- * 'fsCount' - The number of documents that contain a value in the specified field in the result set.
--
-- * 'fsMissing' - The number of documents that do not contain a value in the specified field in the result set.
--
-- * 'fsStddev' - The standard deviation of the values in the specified field in the result set.
--
-- * 'fsMin' - The minimum value found in the specified field in the result set. If the field is numeric (@int@ , @int-array@ , @double@ , or @double-array@ ), @min@ is the string representation of a double-precision 64-bit floating point value. If the field is @date@ or @date-array@ , @min@ is the string representation of a date with the format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339> : yyyy-mm-ddTHH:mm:ss.SSSZ.
--
-- * 'fsSumOfSquares' - The sum of all field values in the result set squared.
--
-- * 'fsSum' - The sum of the field values across the documents in the result set. @null@ for date fields.
fieldStats ::
  FieldStats
fieldStats =
  FieldStats'
    { _fsMax = Nothing,
      _fsMean = Nothing,
      _fsCount = Nothing,
      _fsMissing = Nothing,
      _fsStddev = Nothing,
      _fsMin = Nothing,
      _fsSumOfSquares = Nothing,
      _fsSum = Nothing
    }

-- | The maximum value found in the specified field in the result set. If the field is numeric (@int@ , @int-array@ , @double@ , or @double-array@ ), @max@ is the string representation of a double-precision 64-bit floating point value. If the field is @date@ or @date-array@ , @max@ is the string representation of a date with the format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339> : yyyy-mm-ddTHH:mm:ss.SSSZ.
fsMax :: Lens' FieldStats (Maybe Text)
fsMax = lens _fsMax (\s a -> s {_fsMax = a})

-- | The average of the values found in the specified field in the result set. If the field is numeric (@int@ , @int-array@ , @double@ , or @double-array@ ), @mean@ is the string representation of a double-precision 64-bit floating point value. If the field is @date@ or @date-array@ , @mean@ is the string representation of a date with the format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339> : yyyy-mm-ddTHH:mm:ss.SSSZ.
fsMean :: Lens' FieldStats (Maybe Text)
fsMean = lens _fsMean (\s a -> s {_fsMean = a})

-- | The number of documents that contain a value in the specified field in the result set.
fsCount :: Lens' FieldStats (Maybe Integer)
fsCount = lens _fsCount (\s a -> s {_fsCount = a})

-- | The number of documents that do not contain a value in the specified field in the result set.
fsMissing :: Lens' FieldStats (Maybe Integer)
fsMissing = lens _fsMissing (\s a -> s {_fsMissing = a})

-- | The standard deviation of the values in the specified field in the result set.
fsStddev :: Lens' FieldStats (Maybe Double)
fsStddev = lens _fsStddev (\s a -> s {_fsStddev = a})

-- | The minimum value found in the specified field in the result set. If the field is numeric (@int@ , @int-array@ , @double@ , or @double-array@ ), @min@ is the string representation of a double-precision 64-bit floating point value. If the field is @date@ or @date-array@ , @min@ is the string representation of a date with the format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339> : yyyy-mm-ddTHH:mm:ss.SSSZ.
fsMin :: Lens' FieldStats (Maybe Text)
fsMin = lens _fsMin (\s a -> s {_fsMin = a})

-- | The sum of all field values in the result set squared.
fsSumOfSquares :: Lens' FieldStats (Maybe Double)
fsSumOfSquares = lens _fsSumOfSquares (\s a -> s {_fsSumOfSquares = a})

-- | The sum of the field values across the documents in the result set. @null@ for date fields.
fsSum :: Lens' FieldStats (Maybe Double)
fsSum = lens _fsSum (\s a -> s {_fsSum = a})

instance FromJSON FieldStats where
  parseJSON =
    withObject
      "FieldStats"
      ( \x ->
          FieldStats'
            <$> (x .:? "max")
            <*> (x .:? "mean")
            <*> (x .:? "count")
            <*> (x .:? "missing")
            <*> (x .:? "stddev")
            <*> (x .:? "min")
            <*> (x .:? "sumOfSquares")
            <*> (x .:? "sum")
      )

instance Hashable FieldStats

instance NFData FieldStats
