{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansUtilization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansUtilization where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The measurement of how well you are using your existing Savings Plans.
--
--
--
-- /See:/ 'savingsPlansUtilization' smart constructor.
data SavingsPlansUtilization = SavingsPlansUtilization'
  { _spuUnusedCommitment ::
      !(Maybe Text),
    _spuUtilizationPercentage :: !(Maybe Text),
    _spuTotalCommitment :: !(Maybe Text),
    _spuUsedCommitment :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SavingsPlansUtilization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spuUnusedCommitment' - The amount of your Savings Plans commitment that was not consumed from Savings Plans eligible usage in a specific period.
--
-- * 'spuUtilizationPercentage' - The amount of @UsedCommitment@ divided by the @TotalCommitment@ for your Savings Plans.
--
-- * 'spuTotalCommitment' - The total amount of Savings Plans commitment that's been purchased in an account (or set of accounts).
--
-- * 'spuUsedCommitment' - The amount of your Savings Plans commitment that was consumed from Savings Plans eligible usage in a specific period.
savingsPlansUtilization ::
  SavingsPlansUtilization
savingsPlansUtilization =
  SavingsPlansUtilization'
    { _spuUnusedCommitment = Nothing,
      _spuUtilizationPercentage = Nothing,
      _spuTotalCommitment = Nothing,
      _spuUsedCommitment = Nothing
    }

-- | The amount of your Savings Plans commitment that was not consumed from Savings Plans eligible usage in a specific period.
spuUnusedCommitment :: Lens' SavingsPlansUtilization (Maybe Text)
spuUnusedCommitment = lens _spuUnusedCommitment (\s a -> s {_spuUnusedCommitment = a})

-- | The amount of @UsedCommitment@ divided by the @TotalCommitment@ for your Savings Plans.
spuUtilizationPercentage :: Lens' SavingsPlansUtilization (Maybe Text)
spuUtilizationPercentage = lens _spuUtilizationPercentage (\s a -> s {_spuUtilizationPercentage = a})

-- | The total amount of Savings Plans commitment that's been purchased in an account (or set of accounts).
spuTotalCommitment :: Lens' SavingsPlansUtilization (Maybe Text)
spuTotalCommitment = lens _spuTotalCommitment (\s a -> s {_spuTotalCommitment = a})

-- | The amount of your Savings Plans commitment that was consumed from Savings Plans eligible usage in a specific period.
spuUsedCommitment :: Lens' SavingsPlansUtilization (Maybe Text)
spuUsedCommitment = lens _spuUsedCommitment (\s a -> s {_spuUsedCommitment = a})

instance FromJSON SavingsPlansUtilization where
  parseJSON =
    withObject
      "SavingsPlansUtilization"
      ( \x ->
          SavingsPlansUtilization'
            <$> (x .:? "UnusedCommitment")
            <*> (x .:? "UtilizationPercentage")
            <*> (x .:? "TotalCommitment")
            <*> (x .:? "UsedCommitment")
      )

instance Hashable SavingsPlansUtilization

instance NFData SavingsPlansUtilization
