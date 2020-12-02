{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.Datapoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.Datapoint where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a single value in the forecast data used for predictive scaling.
--
--
--
-- /See:/ 'datapoint' smart constructor.
data Datapoint = Datapoint'
  { _dValue :: !(Maybe Double),
    _dTimestamp :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Datapoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dValue' - The value of the data point.
--
-- * 'dTimestamp' - The time stamp for the data point in UTC format.
datapoint ::
  Datapoint
datapoint = Datapoint' {_dValue = Nothing, _dTimestamp = Nothing}

-- | The value of the data point.
dValue :: Lens' Datapoint (Maybe Double)
dValue = lens _dValue (\s a -> s {_dValue = a})

-- | The time stamp for the data point in UTC format.
dTimestamp :: Lens' Datapoint (Maybe UTCTime)
dTimestamp = lens _dTimestamp (\s a -> s {_dTimestamp = a}) . mapping _Time

instance FromJSON Datapoint where
  parseJSON =
    withObject
      "Datapoint"
      (\x -> Datapoint' <$> (x .:? "Value") <*> (x .:? "Timestamp"))

instance Hashable Datapoint

instance NFData Datapoint
