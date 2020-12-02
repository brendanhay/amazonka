{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.MultiConditionalBranch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.MultiConditionalBranch where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.SimpleCondition
import Network.AWS.Prelude

-- | Specifies a condition to evaluate for an activity path in a journey.
--
--
--
-- /See:/ 'multiConditionalBranch' smart constructor.
data MultiConditionalBranch = MultiConditionalBranch'
  { _mcbNextActivity ::
      !(Maybe Text),
    _mcbCondition :: !(Maybe SimpleCondition)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MultiConditionalBranch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcbNextActivity' - The unique identifier for the next activity to perform, after completing the activity for the path.
--
-- * 'mcbCondition' - The condition to evaluate for the activity path.
multiConditionalBranch ::
  MultiConditionalBranch
multiConditionalBranch =
  MultiConditionalBranch'
    { _mcbNextActivity = Nothing,
      _mcbCondition = Nothing
    }

-- | The unique identifier for the next activity to perform, after completing the activity for the path.
mcbNextActivity :: Lens' MultiConditionalBranch (Maybe Text)
mcbNextActivity = lens _mcbNextActivity (\s a -> s {_mcbNextActivity = a})

-- | The condition to evaluate for the activity path.
mcbCondition :: Lens' MultiConditionalBranch (Maybe SimpleCondition)
mcbCondition = lens _mcbCondition (\s a -> s {_mcbCondition = a})

instance FromJSON MultiConditionalBranch where
  parseJSON =
    withObject
      "MultiConditionalBranch"
      ( \x ->
          MultiConditionalBranch'
            <$> (x .:? "NextActivity") <*> (x .:? "Condition")
      )

instance Hashable MultiConditionalBranch

instance NFData MultiConditionalBranch

instance ToJSON MultiConditionalBranch where
  toJSON MultiConditionalBranch' {..} =
    object
      ( catMaybes
          [ ("NextActivity" .=) <$> _mcbNextActivity,
            ("Condition" .=) <$> _mcbCondition
          ]
      )
