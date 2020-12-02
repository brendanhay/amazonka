{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Total
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Total where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the total usage with the corresponding currency unit for that value.
--
--
--
-- /See:/ 'total' smart constructor.
data Total = Total'
  { _tAmount :: !(Maybe Text),
    _tUnit :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Total' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tAmount' - The total usage.
--
-- * 'tUnit' - The currency unit that the amount is given in.
total ::
  Total
total = Total' {_tAmount = Nothing, _tUnit = Nothing}

-- | The total usage.
tAmount :: Lens' Total (Maybe Text)
tAmount = lens _tAmount (\s a -> s {_tAmount = a})

-- | The currency unit that the amount is given in.
tUnit :: Lens' Total (Maybe Text)
tUnit = lens _tUnit (\s a -> s {_tUnit = a})

instance FromJSON Total where
  parseJSON =
    withObject
      "Total"
      (\x -> Total' <$> (x .:? "amount") <*> (x .:? "unit"))

instance Hashable Total

instance NFData Total
