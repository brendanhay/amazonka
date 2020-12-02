{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.MonthlyTransfer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.MonthlyTransfer where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the monthly data transfer in and out of your virtual private server (or /instance/ ).
--
--
--
-- /See:/ 'monthlyTransfer' smart constructor.
newtype MonthlyTransfer = MonthlyTransfer'
  { _mtGbPerMonthAllocated ::
      Maybe Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MonthlyTransfer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtGbPerMonthAllocated' - The amount allocated per month (in GB).
monthlyTransfer ::
  MonthlyTransfer
monthlyTransfer =
  MonthlyTransfer' {_mtGbPerMonthAllocated = Nothing}

-- | The amount allocated per month (in GB).
mtGbPerMonthAllocated :: Lens' MonthlyTransfer (Maybe Int)
mtGbPerMonthAllocated = lens _mtGbPerMonthAllocated (\s a -> s {_mtGbPerMonthAllocated = a})

instance FromJSON MonthlyTransfer where
  parseJSON =
    withObject
      "MonthlyTransfer"
      (\x -> MonthlyTransfer' <$> (x .:? "gbPerMonthAllocated"))

instance Hashable MonthlyTransfer

instance NFData MonthlyTransfer
