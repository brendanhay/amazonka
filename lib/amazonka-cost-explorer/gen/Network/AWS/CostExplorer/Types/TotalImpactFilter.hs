{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.TotalImpactFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.TotalImpactFilter where

import Network.AWS.CostExplorer.Types.NumericOperator
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Filters cost anomalies based on the total impact.
--
--
--
-- /See:/ 'totalImpactFilter' smart constructor.
data TotalImpactFilter = TotalImpactFilter'
  { _tifEndValue ::
      !(Maybe Double),
    _tifNumericOperator :: !NumericOperator,
    _tifStartValue :: !Double
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TotalImpactFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tifEndValue' - The upper bound dollar value used in the filter.
--
-- * 'tifNumericOperator' - The comparing value used in the filter.
--
-- * 'tifStartValue' - The lower bound dollar value used in the filter.
totalImpactFilter ::
  -- | 'tifNumericOperator'
  NumericOperator ->
  -- | 'tifStartValue'
  Double ->
  TotalImpactFilter
totalImpactFilter pNumericOperator_ pStartValue_ =
  TotalImpactFilter'
    { _tifEndValue = Nothing,
      _tifNumericOperator = pNumericOperator_,
      _tifStartValue = pStartValue_
    }

-- | The upper bound dollar value used in the filter.
tifEndValue :: Lens' TotalImpactFilter (Maybe Double)
tifEndValue = lens _tifEndValue (\s a -> s {_tifEndValue = a})

-- | The comparing value used in the filter.
tifNumericOperator :: Lens' TotalImpactFilter NumericOperator
tifNumericOperator = lens _tifNumericOperator (\s a -> s {_tifNumericOperator = a})

-- | The lower bound dollar value used in the filter.
tifStartValue :: Lens' TotalImpactFilter Double
tifStartValue = lens _tifStartValue (\s a -> s {_tifStartValue = a})

instance Hashable TotalImpactFilter

instance NFData TotalImpactFilter

instance ToJSON TotalImpactFilter where
  toJSON TotalImpactFilter' {..} =
    object
      ( catMaybes
          [ ("EndValue" .=) <$> _tifEndValue,
            Just ("NumericOperator" .= _tifNumericOperator),
            Just ("StartValue" .= _tifStartValue)
          ]
      )
