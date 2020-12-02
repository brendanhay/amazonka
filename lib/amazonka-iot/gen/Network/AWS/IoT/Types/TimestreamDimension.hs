{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TimestreamDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TimestreamDimension where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Metadata attributes of the time series that are written in each measure record.
--
--
--
-- /See:/ 'timestreamDimension' smart constructor.
data TimestreamDimension = TimestreamDimension'
  { _tdName :: !Text,
    _tdValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TimestreamDimension' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdName' - The metadata dimension name. This is the name of the column in the Amazon Timestream database table record. Dimensions cannot be named: @measure_name@ , @measure_value@ , or @time@ . These names are reserved. Dimension names cannot start with @ts_@ or @measure_value@ and they cannot contain the colon (@:@ ) character.
--
-- * 'tdValue' - The value to write in this column of the database record.
timestreamDimension ::
  -- | 'tdName'
  Text ->
  -- | 'tdValue'
  Text ->
  TimestreamDimension
timestreamDimension pName_ pValue_ =
  TimestreamDimension' {_tdName = pName_, _tdValue = pValue_}

-- | The metadata dimension name. This is the name of the column in the Amazon Timestream database table record. Dimensions cannot be named: @measure_name@ , @measure_value@ , or @time@ . These names are reserved. Dimension names cannot start with @ts_@ or @measure_value@ and they cannot contain the colon (@:@ ) character.
tdName :: Lens' TimestreamDimension Text
tdName = lens _tdName (\s a -> s {_tdName = a})

-- | The value to write in this column of the database record.
tdValue :: Lens' TimestreamDimension Text
tdValue = lens _tdValue (\s a -> s {_tdValue = a})

instance FromJSON TimestreamDimension where
  parseJSON =
    withObject
      "TimestreamDimension"
      (\x -> TimestreamDimension' <$> (x .: "name") <*> (x .: "value"))

instance Hashable TimestreamDimension

instance NFData TimestreamDimension

instance ToJSON TimestreamDimension where
  toJSON TimestreamDimension' {..} =
    object
      (catMaybes [Just ("name" .= _tdName), Just ("value" .= _tdValue)])
