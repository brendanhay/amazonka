{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CoverageCost
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CoverageCost where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | How much it costs to run an instance.
--
--
--
-- /See:/ 'coverageCost' smart constructor.
newtype CoverageCost = CoverageCost' {_ccOnDemandCost :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CoverageCost' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccOnDemandCost' - How much an On-Demand Instance costs.
coverageCost ::
  CoverageCost
coverageCost = CoverageCost' {_ccOnDemandCost = Nothing}

-- | How much an On-Demand Instance costs.
ccOnDemandCost :: Lens' CoverageCost (Maybe Text)
ccOnDemandCost = lens _ccOnDemandCost (\s a -> s {_ccOnDemandCost = a})

instance FromJSON CoverageCost where
  parseJSON =
    withObject
      "CoverageCost"
      (\x -> CoverageCost' <$> (x .:? "OnDemandCost"))

instance Hashable CoverageCost

instance NFData CoverageCost
