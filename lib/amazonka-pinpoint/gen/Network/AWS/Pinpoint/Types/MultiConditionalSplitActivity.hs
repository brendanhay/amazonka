{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.MultiConditionalSplitActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.MultiConditionalSplitActivity where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.MultiConditionalBranch
import Network.AWS.Pinpoint.Types.WaitTime
import Network.AWS.Prelude

-- | Specifies the settings for a multivariate split activity in a journey. This type of activity sends participants down one of as many as five paths (including a default /Else/ path) in a journey, based on conditions that you specify.
--
--
--
-- /See:/ 'multiConditionalSplitActivity' smart constructor.
data MultiConditionalSplitActivity = MultiConditionalSplitActivity'
  { _mcsaBranches ::
      !( Maybe
           [MultiConditionalBranch]
       ),
    _mcsaEvaluationWaitTime ::
      !(Maybe WaitTime),
    _mcsaDefaultActivity ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MultiConditionalSplitActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcsaBranches' - The paths for the activity, including the conditions for entering each path and the activity to perform for each path.
--
-- * 'mcsaEvaluationWaitTime' - The amount of time to wait or the date and time when Amazon Pinpoint determines whether the conditions are met.
--
-- * 'mcsaDefaultActivity' - The unique identifier for the activity to perform for participants who don't meet any of the conditions specified for other paths in the activity.
multiConditionalSplitActivity ::
  MultiConditionalSplitActivity
multiConditionalSplitActivity =
  MultiConditionalSplitActivity'
    { _mcsaBranches = Nothing,
      _mcsaEvaluationWaitTime = Nothing,
      _mcsaDefaultActivity = Nothing
    }

-- | The paths for the activity, including the conditions for entering each path and the activity to perform for each path.
mcsaBranches :: Lens' MultiConditionalSplitActivity [MultiConditionalBranch]
mcsaBranches = lens _mcsaBranches (\s a -> s {_mcsaBranches = a}) . _Default . _Coerce

-- | The amount of time to wait or the date and time when Amazon Pinpoint determines whether the conditions are met.
mcsaEvaluationWaitTime :: Lens' MultiConditionalSplitActivity (Maybe WaitTime)
mcsaEvaluationWaitTime = lens _mcsaEvaluationWaitTime (\s a -> s {_mcsaEvaluationWaitTime = a})

-- | The unique identifier for the activity to perform for participants who don't meet any of the conditions specified for other paths in the activity.
mcsaDefaultActivity :: Lens' MultiConditionalSplitActivity (Maybe Text)
mcsaDefaultActivity = lens _mcsaDefaultActivity (\s a -> s {_mcsaDefaultActivity = a})

instance FromJSON MultiConditionalSplitActivity where
  parseJSON =
    withObject
      "MultiConditionalSplitActivity"
      ( \x ->
          MultiConditionalSplitActivity'
            <$> (x .:? "Branches" .!= mempty)
            <*> (x .:? "EvaluationWaitTime")
            <*> (x .:? "DefaultActivity")
      )

instance Hashable MultiConditionalSplitActivity

instance NFData MultiConditionalSplitActivity

instance ToJSON MultiConditionalSplitActivity where
  toJSON MultiConditionalSplitActivity' {..} =
    object
      ( catMaybes
          [ ("Branches" .=) <$> _mcsaBranches,
            ("EvaluationWaitTime" .=) <$> _mcsaEvaluationWaitTime,
            ("DefaultActivity" .=) <$> _mcsaDefaultActivity
          ]
      )
