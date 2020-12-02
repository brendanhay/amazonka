{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.Threshold
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.Threshold where

import Network.AWS.Connect.Types.Comparison
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the threshold for service level metrics.
--
--
--
-- /See:/ 'threshold' smart constructor.
data Threshold = Threshold'
  { _tThresholdValue :: !(Maybe Double),
    _tComparison :: !(Maybe Comparison)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Threshold' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tThresholdValue' - The threshold value to compare.
--
-- * 'tComparison' - The type of comparison. Only "less than" (LT) comparisons are supported.
threshold ::
  Threshold
threshold =
  Threshold' {_tThresholdValue = Nothing, _tComparison = Nothing}

-- | The threshold value to compare.
tThresholdValue :: Lens' Threshold (Maybe Double)
tThresholdValue = lens _tThresholdValue (\s a -> s {_tThresholdValue = a})

-- | The type of comparison. Only "less than" (LT) comparisons are supported.
tComparison :: Lens' Threshold (Maybe Comparison)
tComparison = lens _tComparison (\s a -> s {_tComparison = a})

instance FromJSON Threshold where
  parseJSON =
    withObject
      "Threshold"
      ( \x ->
          Threshold' <$> (x .:? "ThresholdValue") <*> (x .:? "Comparison")
      )

instance Hashable Threshold

instance NFData Threshold

instance ToJSON Threshold where
  toJSON Threshold' {..} =
    object
      ( catMaybes
          [ ("ThresholdValue" .=) <$> _tThresholdValue,
            ("Comparison" .=) <$> _tComparison
          ]
      )
