{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.RandomSplitEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.RandomSplitEntry where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the settings for a path in a random split activity in a journey.
--
--
--
-- /See:/ 'randomSplitEntry' smart constructor.
data RandomSplitEntry = RandomSplitEntry'
  { _rseNextActivity ::
      !(Maybe Text),
    _rsePercentage :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RandomSplitEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rseNextActivity' - The unique identifier for the next activity to perform, after completing the activity for the path.
--
-- * 'rsePercentage' - The percentage of participants to send down the activity path. To determine which participants are sent down each path, Amazon Pinpoint applies a probability-based algorithm to the percentages that you specify for the paths. Therefore, the actual percentage of participants who are sent down a path may not be equal to the percentage that you specify.
randomSplitEntry ::
  RandomSplitEntry
randomSplitEntry =
  RandomSplitEntry'
    { _rseNextActivity = Nothing,
      _rsePercentage = Nothing
    }

-- | The unique identifier for the next activity to perform, after completing the activity for the path.
rseNextActivity :: Lens' RandomSplitEntry (Maybe Text)
rseNextActivity = lens _rseNextActivity (\s a -> s {_rseNextActivity = a})

-- | The percentage of participants to send down the activity path. To determine which participants are sent down each path, Amazon Pinpoint applies a probability-based algorithm to the percentages that you specify for the paths. Therefore, the actual percentage of participants who are sent down a path may not be equal to the percentage that you specify.
rsePercentage :: Lens' RandomSplitEntry (Maybe Int)
rsePercentage = lens _rsePercentage (\s a -> s {_rsePercentage = a})

instance FromJSON RandomSplitEntry where
  parseJSON =
    withObject
      "RandomSplitEntry"
      ( \x ->
          RandomSplitEntry'
            <$> (x .:? "NextActivity") <*> (x .:? "Percentage")
      )

instance Hashable RandomSplitEntry

instance NFData RandomSplitEntry

instance ToJSON RandomSplitEntry where
  toJSON RandomSplitEntry' {..} =
    object
      ( catMaybes
          [ ("NextActivity" .=) <$> _rseNextActivity,
            ("Percentage" .=) <$> _rsePercentage
          ]
      )
