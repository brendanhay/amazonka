{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ObjectiveStatusCounters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ObjectiveStatusCounters where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the number of training jobs that this hyperparameter tuning job launched, categorized by the status of their objective metric. The objective metric status shows whether the final objective metric for the training job has been evaluated by the tuning job and used in the hyperparameter tuning process.
--
--
--
-- /See:/ 'objectiveStatusCounters' smart constructor.
data ObjectiveStatusCounters = ObjectiveStatusCounters'
  { _oscPending ::
      !(Maybe Nat),
    _oscSucceeded :: !(Maybe Nat),
    _oscFailed :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ObjectiveStatusCounters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oscPending' - The number of training jobs that are in progress and pending evaluation of their final objective metric.
--
-- * 'oscSucceeded' - The number of training jobs whose final objective metric was evaluated by the hyperparameter tuning job and used in the hyperparameter tuning process.
--
-- * 'oscFailed' - The number of training jobs whose final objective metric was not evaluated and used in the hyperparameter tuning process. This typically occurs when the training job failed or did not emit an objective metric.
objectiveStatusCounters ::
  ObjectiveStatusCounters
objectiveStatusCounters =
  ObjectiveStatusCounters'
    { _oscPending = Nothing,
      _oscSucceeded = Nothing,
      _oscFailed = Nothing
    }

-- | The number of training jobs that are in progress and pending evaluation of their final objective metric.
oscPending :: Lens' ObjectiveStatusCounters (Maybe Natural)
oscPending = lens _oscPending (\s a -> s {_oscPending = a}) . mapping _Nat

-- | The number of training jobs whose final objective metric was evaluated by the hyperparameter tuning job and used in the hyperparameter tuning process.
oscSucceeded :: Lens' ObjectiveStatusCounters (Maybe Natural)
oscSucceeded = lens _oscSucceeded (\s a -> s {_oscSucceeded = a}) . mapping _Nat

-- | The number of training jobs whose final objective metric was not evaluated and used in the hyperparameter tuning process. This typically occurs when the training job failed or did not emit an objective metric.
oscFailed :: Lens' ObjectiveStatusCounters (Maybe Natural)
oscFailed = lens _oscFailed (\s a -> s {_oscFailed = a}) . mapping _Nat

instance FromJSON ObjectiveStatusCounters where
  parseJSON =
    withObject
      "ObjectiveStatusCounters"
      ( \x ->
          ObjectiveStatusCounters'
            <$> (x .:? "Pending") <*> (x .:? "Succeeded") <*> (x .:? "Failed")
      )

instance Hashable ObjectiveStatusCounters

instance NFData ObjectiveStatusCounters
