{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.TimePeriod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.TimePeriod where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The period of time that is covered by a budget. The period has a start date and an end date. The start date must come before the end date. There are no restrictions on the end date.
--
--
--
-- /See:/ 'timePeriod' smart constructor.
data TimePeriod = TimePeriod'
  { _tpStart :: !(Maybe POSIX),
    _tpEnd :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TimePeriod' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tpStart' - The start date for a budget. If you created your budget and didn't specify a start date, AWS defaults to the start of your chosen time period (DAILY, MONTHLY, QUARTERLY, or ANNUALLY). For example, if you created your budget on January 24, 2018, chose @DAILY@ , and didn't set a start date, AWS set your start date to @01/24/18 00:00 UTC@ . If you chose @MONTHLY@ , AWS set your start date to @01/01/18 00:00 UTC@ . The defaults are the same for the AWS Billing and Cost Management console and the API. You can change your start date with the @UpdateBudget@ operation.
--
-- * 'tpEnd' - The end date for a budget. If you didn't specify an end date, AWS set your end date to @06/15/87 00:00 UTC@ . The defaults are the same for the AWS Billing and Cost Management console and the API. After the end date, AWS deletes the budget and all associated notifications and subscribers. You can change your end date with the @UpdateBudget@ operation.
timePeriod ::
  TimePeriod
timePeriod = TimePeriod' {_tpStart = Nothing, _tpEnd = Nothing}

-- | The start date for a budget. If you created your budget and didn't specify a start date, AWS defaults to the start of your chosen time period (DAILY, MONTHLY, QUARTERLY, or ANNUALLY). For example, if you created your budget on January 24, 2018, chose @DAILY@ , and didn't set a start date, AWS set your start date to @01/24/18 00:00 UTC@ . If you chose @MONTHLY@ , AWS set your start date to @01/01/18 00:00 UTC@ . The defaults are the same for the AWS Billing and Cost Management console and the API. You can change your start date with the @UpdateBudget@ operation.
tpStart :: Lens' TimePeriod (Maybe UTCTime)
tpStart = lens _tpStart (\s a -> s {_tpStart = a}) . mapping _Time

-- | The end date for a budget. If you didn't specify an end date, AWS set your end date to @06/15/87 00:00 UTC@ . The defaults are the same for the AWS Billing and Cost Management console and the API. After the end date, AWS deletes the budget and all associated notifications and subscribers. You can change your end date with the @UpdateBudget@ operation.
tpEnd :: Lens' TimePeriod (Maybe UTCTime)
tpEnd = lens _tpEnd (\s a -> s {_tpEnd = a}) . mapping _Time

instance FromJSON TimePeriod where
  parseJSON =
    withObject
      "TimePeriod"
      (\x -> TimePeriod' <$> (x .:? "Start") <*> (x .:? "End"))

instance Hashable TimePeriod

instance NFData TimePeriod

instance ToJSON TimePeriod where
  toJSON TimePeriod' {..} =
    object
      (catMaybes [("Start" .=) <$> _tpStart, ("End" .=) <$> _tpEnd])
