{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansAmortizedCommitment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansAmortizedCommitment where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The amortized amount of Savings Plans purchased in a specific account during a specific time interval.
--
--
--
-- /See:/ 'savingsPlansAmortizedCommitment' smart constructor.
data SavingsPlansAmortizedCommitment = SavingsPlansAmortizedCommitment'
  { _spacAmortizedUpfrontCommitment ::
      !(Maybe Text),
    _spacTotalAmortizedCommitment ::
      !(Maybe Text),
    _spacAmortizedRecurringCommitment ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SavingsPlansAmortizedCommitment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spacAmortizedUpfrontCommitment' - The amortized amount of your Savings Plans commitment that was purchased with an @Upfront@ or @PartialUpfront@ Savings Plans.
--
-- * 'spacTotalAmortizedCommitment' - The total amortized amount of your Savings Plans commitment, regardless of your Savings Plans purchase method.
--
-- * 'spacAmortizedRecurringCommitment' - The amortized amount of your Savings Plans commitment that was purchased with either a @Partial@ or a @NoUpfront@ .
savingsPlansAmortizedCommitment ::
  SavingsPlansAmortizedCommitment
savingsPlansAmortizedCommitment =
  SavingsPlansAmortizedCommitment'
    { _spacAmortizedUpfrontCommitment =
        Nothing,
      _spacTotalAmortizedCommitment = Nothing,
      _spacAmortizedRecurringCommitment = Nothing
    }

-- | The amortized amount of your Savings Plans commitment that was purchased with an @Upfront@ or @PartialUpfront@ Savings Plans.
spacAmortizedUpfrontCommitment :: Lens' SavingsPlansAmortizedCommitment (Maybe Text)
spacAmortizedUpfrontCommitment = lens _spacAmortizedUpfrontCommitment (\s a -> s {_spacAmortizedUpfrontCommitment = a})

-- | The total amortized amount of your Savings Plans commitment, regardless of your Savings Plans purchase method.
spacTotalAmortizedCommitment :: Lens' SavingsPlansAmortizedCommitment (Maybe Text)
spacTotalAmortizedCommitment = lens _spacTotalAmortizedCommitment (\s a -> s {_spacTotalAmortizedCommitment = a})

-- | The amortized amount of your Savings Plans commitment that was purchased with either a @Partial@ or a @NoUpfront@ .
spacAmortizedRecurringCommitment :: Lens' SavingsPlansAmortizedCommitment (Maybe Text)
spacAmortizedRecurringCommitment = lens _spacAmortizedRecurringCommitment (\s a -> s {_spacAmortizedRecurringCommitment = a})

instance FromJSON SavingsPlansAmortizedCommitment where
  parseJSON =
    withObject
      "SavingsPlansAmortizedCommitment"
      ( \x ->
          SavingsPlansAmortizedCommitment'
            <$> (x .:? "AmortizedUpfrontCommitment")
            <*> (x .:? "TotalAmortizedCommitment")
            <*> (x .:? "AmortizedRecurringCommitment")
      )

instance Hashable SavingsPlansAmortizedCommitment

instance NFData SavingsPlansAmortizedCommitment
