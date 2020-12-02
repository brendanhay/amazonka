{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Attribute details on a specific Savings Plan.
--
--
--
-- /See:/ 'savingsPlansDetails' smart constructor.
data SavingsPlansDetails = SavingsPlansDetails'
  { _spdInstanceFamily ::
      !(Maybe Text),
    _spdOfferingId :: !(Maybe Text),
    _spdRegion :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SavingsPlansDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spdInstanceFamily' - A group of instance types that Savings Plans applies to.
--
-- * 'spdOfferingId' - The unique ID used to distinguish Savings Plans from one another.
--
-- * 'spdRegion' - A collection of AWS resources in a geographic area. Each AWS Region is isolated and independent of the other Regions.
savingsPlansDetails ::
  SavingsPlansDetails
savingsPlansDetails =
  SavingsPlansDetails'
    { _spdInstanceFamily = Nothing,
      _spdOfferingId = Nothing,
      _spdRegion = Nothing
    }

-- | A group of instance types that Savings Plans applies to.
spdInstanceFamily :: Lens' SavingsPlansDetails (Maybe Text)
spdInstanceFamily = lens _spdInstanceFamily (\s a -> s {_spdInstanceFamily = a})

-- | The unique ID used to distinguish Savings Plans from one another.
spdOfferingId :: Lens' SavingsPlansDetails (Maybe Text)
spdOfferingId = lens _spdOfferingId (\s a -> s {_spdOfferingId = a})

-- | A collection of AWS resources in a geographic area. Each AWS Region is isolated and independent of the other Regions.
spdRegion :: Lens' SavingsPlansDetails (Maybe Text)
spdRegion = lens _spdRegion (\s a -> s {_spdRegion = a})

instance FromJSON SavingsPlansDetails where
  parseJSON =
    withObject
      "SavingsPlansDetails"
      ( \x ->
          SavingsPlansDetails'
            <$> (x .:? "InstanceFamily")
            <*> (x .:? "OfferingId")
            <*> (x .:? "Region")
      )

instance Hashable SavingsPlansDetails

instance NFData SavingsPlansDetails
