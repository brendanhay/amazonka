{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.HoldoutActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.HoldoutActivity where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the settings for a holdout activity in a journey. This type of activity stops a journey for a specified percentage of participants.
--
--
--
-- /See:/ 'holdoutActivity' smart constructor.
data HoldoutActivity = HoldoutActivity'
  { _haNextActivity ::
      !(Maybe Text),
    _haPercentage :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HoldoutActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'haNextActivity' - The unique identifier for the next activity to perform, after performing the holdout activity.
--
-- * 'haPercentage' - The percentage of participants who shouldn't continue the journey. To determine which participants are held out, Amazon Pinpoint applies a probability-based algorithm to the percentage that you specify. Therefore, the actual percentage of participants who are held out may not be equal to the percentage that you specify.
holdoutActivity ::
  -- | 'haPercentage'
  Int ->
  HoldoutActivity
holdoutActivity pPercentage_ =
  HoldoutActivity'
    { _haNextActivity = Nothing,
      _haPercentage = pPercentage_
    }

-- | The unique identifier for the next activity to perform, after performing the holdout activity.
haNextActivity :: Lens' HoldoutActivity (Maybe Text)
haNextActivity = lens _haNextActivity (\s a -> s {_haNextActivity = a})

-- | The percentage of participants who shouldn't continue the journey. To determine which participants are held out, Amazon Pinpoint applies a probability-based algorithm to the percentage that you specify. Therefore, the actual percentage of participants who are held out may not be equal to the percentage that you specify.
haPercentage :: Lens' HoldoutActivity Int
haPercentage = lens _haPercentage (\s a -> s {_haPercentage = a})

instance FromJSON HoldoutActivity where
  parseJSON =
    withObject
      "HoldoutActivity"
      ( \x ->
          HoldoutActivity'
            <$> (x .:? "NextActivity") <*> (x .: "Percentage")
      )

instance Hashable HoldoutActivity

instance NFData HoldoutActivity

instance ToJSON HoldoutActivity where
  toJSON HoldoutActivity' {..} =
    object
      ( catMaybes
          [ ("NextActivity" .=) <$> _haNextActivity,
            Just ("Percentage" .= _haPercentage)
          ]
      )
