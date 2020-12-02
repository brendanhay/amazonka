{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ConditionalSplitActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ConditionalSplitActivity where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.Condition
import Network.AWS.Pinpoint.Types.WaitTime
import Network.AWS.Prelude

-- | Specifies the settings for a yes/no split activity in a journey. This type of activity sends participants down one of two paths in a journey, based on conditions that you specify.
--
--
--
-- /See:/ 'conditionalSplitActivity' smart constructor.
data ConditionalSplitActivity = ConditionalSplitActivity'
  { _csaEvaluationWaitTime ::
      !(Maybe WaitTime),
    _csaTrueActivity :: !(Maybe Text),
    _csaFalseActivity :: !(Maybe Text),
    _csaCondition :: !(Maybe Condition)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConditionalSplitActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csaEvaluationWaitTime' - The amount of time to wait before determining whether the conditions are met, or the date and time when Amazon Pinpoint determines whether the conditions are met.
--
-- * 'csaTrueActivity' - The unique identifier for the activity to perform if the conditions are met.
--
-- * 'csaFalseActivity' - The unique identifier for the activity to perform if the conditions aren't met.
--
-- * 'csaCondition' - The conditions that define the paths for the activity, and the relationship between the conditions.
conditionalSplitActivity ::
  ConditionalSplitActivity
conditionalSplitActivity =
  ConditionalSplitActivity'
    { _csaEvaluationWaitTime = Nothing,
      _csaTrueActivity = Nothing,
      _csaFalseActivity = Nothing,
      _csaCondition = Nothing
    }

-- | The amount of time to wait before determining whether the conditions are met, or the date and time when Amazon Pinpoint determines whether the conditions are met.
csaEvaluationWaitTime :: Lens' ConditionalSplitActivity (Maybe WaitTime)
csaEvaluationWaitTime = lens _csaEvaluationWaitTime (\s a -> s {_csaEvaluationWaitTime = a})

-- | The unique identifier for the activity to perform if the conditions are met.
csaTrueActivity :: Lens' ConditionalSplitActivity (Maybe Text)
csaTrueActivity = lens _csaTrueActivity (\s a -> s {_csaTrueActivity = a})

-- | The unique identifier for the activity to perform if the conditions aren't met.
csaFalseActivity :: Lens' ConditionalSplitActivity (Maybe Text)
csaFalseActivity = lens _csaFalseActivity (\s a -> s {_csaFalseActivity = a})

-- | The conditions that define the paths for the activity, and the relationship between the conditions.
csaCondition :: Lens' ConditionalSplitActivity (Maybe Condition)
csaCondition = lens _csaCondition (\s a -> s {_csaCondition = a})

instance FromJSON ConditionalSplitActivity where
  parseJSON =
    withObject
      "ConditionalSplitActivity"
      ( \x ->
          ConditionalSplitActivity'
            <$> (x .:? "EvaluationWaitTime")
            <*> (x .:? "TrueActivity")
            <*> (x .:? "FalseActivity")
            <*> (x .:? "Condition")
      )

instance Hashable ConditionalSplitActivity

instance NFData ConditionalSplitActivity

instance ToJSON ConditionalSplitActivity where
  toJSON ConditionalSplitActivity' {..} =
    object
      ( catMaybes
          [ ("EvaluationWaitTime" .=) <$> _csaEvaluationWaitTime,
            ("TrueActivity" .=) <$> _csaTrueActivity,
            ("FalseActivity" .=) <$> _csaFalseActivity,
            ("Condition" .=) <$> _csaCondition
          ]
      )
