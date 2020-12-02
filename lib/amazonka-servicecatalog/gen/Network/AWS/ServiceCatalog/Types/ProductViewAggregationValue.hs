{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProductViewAggregationValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProductViewAggregationValue where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A single product view aggregation value/count pair, containing metadata about each product to which the calling user has access.
--
--
--
-- /See:/ 'productViewAggregationValue' smart constructor.
data ProductViewAggregationValue = ProductViewAggregationValue'
  { _pvavValue ::
      !(Maybe Text),
    _pvavApproximateCount ::
      !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProductViewAggregationValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pvavValue' - The value of the product view aggregation.
--
-- * 'pvavApproximateCount' - An approximate count of the products that match the value.
productViewAggregationValue ::
  ProductViewAggregationValue
productViewAggregationValue =
  ProductViewAggregationValue'
    { _pvavValue = Nothing,
      _pvavApproximateCount = Nothing
    }

-- | The value of the product view aggregation.
pvavValue :: Lens' ProductViewAggregationValue (Maybe Text)
pvavValue = lens _pvavValue (\s a -> s {_pvavValue = a})

-- | An approximate count of the products that match the value.
pvavApproximateCount :: Lens' ProductViewAggregationValue (Maybe Int)
pvavApproximateCount = lens _pvavApproximateCount (\s a -> s {_pvavApproximateCount = a})

instance FromJSON ProductViewAggregationValue where
  parseJSON =
    withObject
      "ProductViewAggregationValue"
      ( \x ->
          ProductViewAggregationValue'
            <$> (x .:? "Value") <*> (x .:? "ApproximateCount")
      )

instance Hashable ProductViewAggregationValue

instance NFData ProductViewAggregationValue
