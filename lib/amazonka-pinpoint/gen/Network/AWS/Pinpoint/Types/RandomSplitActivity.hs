{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.RandomSplitActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.RandomSplitActivity where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.RandomSplitEntry
import Network.AWS.Prelude

-- | Specifies the settings for a random split activity in a journey. This type of activity randomly sends specified percentages of participants down one of as many as five paths in a journey, based on conditions that you specify.
--
--
--
-- /See:/ 'randomSplitActivity' smart constructor.
newtype RandomSplitActivity = RandomSplitActivity'
  { _rsaBranches ::
      Maybe [RandomSplitEntry]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RandomSplitActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsaBranches' - The paths for the activity, including the percentage of participants to enter each path and the activity to perform for each path.
randomSplitActivity ::
  RandomSplitActivity
randomSplitActivity = RandomSplitActivity' {_rsaBranches = Nothing}

-- | The paths for the activity, including the percentage of participants to enter each path and the activity to perform for each path.
rsaBranches :: Lens' RandomSplitActivity [RandomSplitEntry]
rsaBranches = lens _rsaBranches (\s a -> s {_rsaBranches = a}) . _Default . _Coerce

instance FromJSON RandomSplitActivity where
  parseJSON =
    withObject
      "RandomSplitActivity"
      (\x -> RandomSplitActivity' <$> (x .:? "Branches" .!= mempty))

instance Hashable RandomSplitActivity

instance NFData RandomSplitActivity

instance ToJSON RandomSplitActivity where
  toJSON RandomSplitActivity' {..} =
    object (catMaybes [("Branches" .=) <$> _rsaBranches])
