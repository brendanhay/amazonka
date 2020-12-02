{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.MonetaryAmount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.MonetaryAmount where

import Network.AWS.DeviceFarm.Types.CurrencyCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A number that represents the monetary amount for an offering or transaction.
--
--
--
-- /See:/ 'monetaryAmount' smart constructor.
data MonetaryAmount = MonetaryAmount'
  { _maAmount :: !(Maybe Double),
    _maCurrencyCode :: !(Maybe CurrencyCode)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MonetaryAmount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'maAmount' - The numerical amount of an offering or transaction.
--
-- * 'maCurrencyCode' - The currency code of a monetary amount. For example, @USD@ means U.S. dollars.
monetaryAmount ::
  MonetaryAmount
monetaryAmount =
  MonetaryAmount' {_maAmount = Nothing, _maCurrencyCode = Nothing}

-- | The numerical amount of an offering or transaction.
maAmount :: Lens' MonetaryAmount (Maybe Double)
maAmount = lens _maAmount (\s a -> s {_maAmount = a})

-- | The currency code of a monetary amount. For example, @USD@ means U.S. dollars.
maCurrencyCode :: Lens' MonetaryAmount (Maybe CurrencyCode)
maCurrencyCode = lens _maCurrencyCode (\s a -> s {_maCurrencyCode = a})

instance FromJSON MonetaryAmount where
  parseJSON =
    withObject
      "MonetaryAmount"
      ( \x ->
          MonetaryAmount' <$> (x .:? "amount") <*> (x .:? "currencyCode")
      )

instance Hashable MonetaryAmount

instance NFData MonetaryAmount
