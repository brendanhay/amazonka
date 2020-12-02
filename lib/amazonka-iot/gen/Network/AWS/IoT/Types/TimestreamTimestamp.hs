{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TimestreamTimestamp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TimestreamTimestamp where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes how to interpret an application-defined timestamp value from an MQTT message payload and the precision of that value.
--
--
--
-- /See:/ 'timestreamTimestamp' smart constructor.
data TimestreamTimestamp = TimestreamTimestamp'
  { _ttValue :: !Text,
    _ttUnit :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TimestreamTimestamp' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ttValue' - An expression that returns a long epoch time value.
--
-- * 'ttUnit' - The precision of the timestamp value that results from the expression described in @value@ . Valid values: @SECONDS@ | @MILLISECONDS@ | @MICROSECONDS@ | @NANOSECONDS@ . The default is @MILLISECONDS@ .
timestreamTimestamp ::
  -- | 'ttValue'
  Text ->
  -- | 'ttUnit'
  Text ->
  TimestreamTimestamp
timestreamTimestamp pValue_ pUnit_ =
  TimestreamTimestamp' {_ttValue = pValue_, _ttUnit = pUnit_}

-- | An expression that returns a long epoch time value.
ttValue :: Lens' TimestreamTimestamp Text
ttValue = lens _ttValue (\s a -> s {_ttValue = a})

-- | The precision of the timestamp value that results from the expression described in @value@ . Valid values: @SECONDS@ | @MILLISECONDS@ | @MICROSECONDS@ | @NANOSECONDS@ . The default is @MILLISECONDS@ .
ttUnit :: Lens' TimestreamTimestamp Text
ttUnit = lens _ttUnit (\s a -> s {_ttUnit = a})

instance FromJSON TimestreamTimestamp where
  parseJSON =
    withObject
      "TimestreamTimestamp"
      (\x -> TimestreamTimestamp' <$> (x .: "value") <*> (x .: "unit"))

instance Hashable TimestreamTimestamp

instance NFData TimestreamTimestamp

instance ToJSON TimestreamTimestamp where
  toJSON TimestreamTimestamp' {..} =
    object
      (catMaybes [Just ("value" .= _ttValue), Just ("unit" .= _ttUnit)])
