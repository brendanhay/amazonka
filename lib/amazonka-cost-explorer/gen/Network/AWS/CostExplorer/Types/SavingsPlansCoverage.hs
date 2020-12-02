{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansCoverage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansCoverage where

import Network.AWS.CostExplorer.Types.DateInterval
import Network.AWS.CostExplorer.Types.SavingsPlansCoverageData
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The amount of Savings Plans eligible usage that is covered by Savings Plans. All calculations consider the On-Demand equivalent of your Savings Plans usage.
--
--
--
-- /See:/ 'savingsPlansCoverage' smart constructor.
data SavingsPlansCoverage = SavingsPlansCoverage'
  { _spcTimePeriod ::
      !(Maybe DateInterval),
    _spcCoverage :: !(Maybe SavingsPlansCoverageData),
    _spcAttributes :: !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SavingsPlansCoverage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spcTimePeriod' - Undocumented member.
--
-- * 'spcCoverage' - The amount of Savings Plans eligible usage that the Savings Plans covered.
--
-- * 'spcAttributes' - The attribute that applies to a specific @Dimension@ .
savingsPlansCoverage ::
  SavingsPlansCoverage
savingsPlansCoverage =
  SavingsPlansCoverage'
    { _spcTimePeriod = Nothing,
      _spcCoverage = Nothing,
      _spcAttributes = Nothing
    }

-- | Undocumented member.
spcTimePeriod :: Lens' SavingsPlansCoverage (Maybe DateInterval)
spcTimePeriod = lens _spcTimePeriod (\s a -> s {_spcTimePeriod = a})

-- | The amount of Savings Plans eligible usage that the Savings Plans covered.
spcCoverage :: Lens' SavingsPlansCoverage (Maybe SavingsPlansCoverageData)
spcCoverage = lens _spcCoverage (\s a -> s {_spcCoverage = a})

-- | The attribute that applies to a specific @Dimension@ .
spcAttributes :: Lens' SavingsPlansCoverage (HashMap Text (Text))
spcAttributes = lens _spcAttributes (\s a -> s {_spcAttributes = a}) . _Default . _Map

instance FromJSON SavingsPlansCoverage where
  parseJSON =
    withObject
      "SavingsPlansCoverage"
      ( \x ->
          SavingsPlansCoverage'
            <$> (x .:? "TimePeriod")
            <*> (x .:? "Coverage")
            <*> (x .:? "Attributes" .!= mempty)
      )

instance Hashable SavingsPlansCoverage

instance NFData SavingsPlansCoverage
