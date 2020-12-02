{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.MetricValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.MetricValue where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The value to be compared with the @metric@ .
--
--
--
-- /See:/ 'metricValue' smart constructor.
data MetricValue = MetricValue'
  { _mvCidrs :: !(Maybe [Text]),
    _mvCount :: !(Maybe Nat),
    _mvPorts :: !(Maybe [Nat])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MetricValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mvCidrs' - If the @comparisonOperator@ calls for a set of CIDRs, use this to specify that set to be compared with the @metric@ .
--
-- * 'mvCount' - If the @comparisonOperator@ calls for a numeric value, use this to specify that numeric value to be compared with the @metric@ .
--
-- * 'mvPorts' - If the @comparisonOperator@ calls for a set of ports, use this to specify that set to be compared with the @metric@ .
metricValue ::
  MetricValue
metricValue =
  MetricValue'
    { _mvCidrs = Nothing,
      _mvCount = Nothing,
      _mvPorts = Nothing
    }

-- | If the @comparisonOperator@ calls for a set of CIDRs, use this to specify that set to be compared with the @metric@ .
mvCidrs :: Lens' MetricValue [Text]
mvCidrs = lens _mvCidrs (\s a -> s {_mvCidrs = a}) . _Default . _Coerce

-- | If the @comparisonOperator@ calls for a numeric value, use this to specify that numeric value to be compared with the @metric@ .
mvCount :: Lens' MetricValue (Maybe Natural)
mvCount = lens _mvCount (\s a -> s {_mvCount = a}) . mapping _Nat

-- | If the @comparisonOperator@ calls for a set of ports, use this to specify that set to be compared with the @metric@ .
mvPorts :: Lens' MetricValue [Natural]
mvPorts = lens _mvPorts (\s a -> s {_mvPorts = a}) . _Default . _Coerce

instance FromJSON MetricValue where
  parseJSON =
    withObject
      "MetricValue"
      ( \x ->
          MetricValue'
            <$> (x .:? "cidrs" .!= mempty)
            <*> (x .:? "count")
            <*> (x .:? "ports" .!= mempty)
      )

instance Hashable MetricValue

instance NFData MetricValue

instance ToJSON MetricValue where
  toJSON MetricValue' {..} =
    object
      ( catMaybes
          [ ("cidrs" .=) <$> _mvCidrs,
            ("count" .=) <$> _mvCount,
            ("ports" .=) <$> _mvPorts
          ]
      )
