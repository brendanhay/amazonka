{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.USD
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.USD where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents an amount of money in United States dollars/
--
--
--
-- /See:/ 'uSD' smart constructor.
data USD = USD'
  { _usdCents :: !(Maybe Nat),
    _usdDollars :: !(Maybe Nat),
    _usdTenthFractionsOfACent :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'USD' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usdCents' - The fractional portion, in cents, of the amount.
--
-- * 'usdDollars' - The whole number of dollars in the amount.
--
-- * 'usdTenthFractionsOfACent' - Fractions of a cent, in tenths.
uSD ::
  USD
uSD =
  USD'
    { _usdCents = Nothing,
      _usdDollars = Nothing,
      _usdTenthFractionsOfACent = Nothing
    }

-- | The fractional portion, in cents, of the amount.
usdCents :: Lens' USD (Maybe Natural)
usdCents = lens _usdCents (\s a -> s {_usdCents = a}) . mapping _Nat

-- | The whole number of dollars in the amount.
usdDollars :: Lens' USD (Maybe Natural)
usdDollars = lens _usdDollars (\s a -> s {_usdDollars = a}) . mapping _Nat

-- | Fractions of a cent, in tenths.
usdTenthFractionsOfACent :: Lens' USD (Maybe Natural)
usdTenthFractionsOfACent = lens _usdTenthFractionsOfACent (\s a -> s {_usdTenthFractionsOfACent = a}) . mapping _Nat

instance FromJSON USD where
  parseJSON =
    withObject
      "USD"
      ( \x ->
          USD'
            <$> (x .:? "Cents")
            <*> (x .:? "Dollars")
            <*> (x .:? "TenthFractionsOfACent")
      )

instance Hashable USD

instance NFData USD

instance ToJSON USD where
  toJSON USD' {..} =
    object
      ( catMaybes
          [ ("Cents" .=) <$> _usdCents,
            ("Dollars" .=) <$> _usdDollars,
            ("TenthFractionsOfACent" .=) <$> _usdTenthFractionsOfACent
          ]
      )
