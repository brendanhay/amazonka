{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.MetricDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.MetricDimension where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the dimension of a metric.
--
--
--
-- /See:/ 'metricDimension' smart constructor.
data MetricDimension = MetricDimension'
  { _mdName :: !Text,
    _mdValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MetricDimension' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdName' - The name of the dimension.
--
-- * 'mdValue' - The value of the dimension.
metricDimension ::
  -- | 'mdName'
  Text ->
  -- | 'mdValue'
  Text ->
  MetricDimension
metricDimension pName_ pValue_ =
  MetricDimension' {_mdName = pName_, _mdValue = pValue_}

-- | The name of the dimension.
mdName :: Lens' MetricDimension Text
mdName = lens _mdName (\s a -> s {_mdName = a})

-- | The value of the dimension.
mdValue :: Lens' MetricDimension Text
mdValue = lens _mdValue (\s a -> s {_mdValue = a})

instance FromXML MetricDimension where
  parseXML x = MetricDimension' <$> (x .@ "Name") <*> (x .@ "Value")

instance Hashable MetricDimension

instance NFData MetricDimension

instance ToQuery MetricDimension where
  toQuery MetricDimension' {..} =
    mconcat ["Name" =: _mdName, "Value" =: _mdValue]
