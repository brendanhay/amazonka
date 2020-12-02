{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AbortCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AbortCriteria where

import Network.AWS.IoT.Types.AbortAction
import Network.AWS.IoT.Types.JobExecutionFailureType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The criteria that determine when and how a job abort takes place.
--
--
--
-- /See:/ 'abortCriteria' smart constructor.
data AbortCriteria = AbortCriteria'
  { _acFailureType ::
      !JobExecutionFailureType,
    _acAction :: !AbortAction,
    _acThresholdPercentage :: !Double,
    _acMinNumberOfExecutedThings :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AbortCriteria' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acFailureType' - The type of job execution failures that can initiate a job abort.
--
-- * 'acAction' - The type of job action to take to initiate the job abort.
--
-- * 'acThresholdPercentage' - The minimum percentage of job execution failures that must occur to initiate the job abort. AWS IoT supports up to two digits after the decimal (for example, 10.9 and 10.99, but not 10.999).
--
-- * 'acMinNumberOfExecutedThings' - The minimum number of things which must receive job execution notifications before the job can be aborted.
abortCriteria ::
  -- | 'acFailureType'
  JobExecutionFailureType ->
  -- | 'acAction'
  AbortAction ->
  -- | 'acThresholdPercentage'
  Double ->
  -- | 'acMinNumberOfExecutedThings'
  Natural ->
  AbortCriteria
abortCriteria
  pFailureType_
  pAction_
  pThresholdPercentage_
  pMinNumberOfExecutedThings_ =
    AbortCriteria'
      { _acFailureType = pFailureType_,
        _acAction = pAction_,
        _acThresholdPercentage = pThresholdPercentage_,
        _acMinNumberOfExecutedThings = _Nat # pMinNumberOfExecutedThings_
      }

-- | The type of job execution failures that can initiate a job abort.
acFailureType :: Lens' AbortCriteria JobExecutionFailureType
acFailureType = lens _acFailureType (\s a -> s {_acFailureType = a})

-- | The type of job action to take to initiate the job abort.
acAction :: Lens' AbortCriteria AbortAction
acAction = lens _acAction (\s a -> s {_acAction = a})

-- | The minimum percentage of job execution failures that must occur to initiate the job abort. AWS IoT supports up to two digits after the decimal (for example, 10.9 and 10.99, but not 10.999).
acThresholdPercentage :: Lens' AbortCriteria Double
acThresholdPercentage = lens _acThresholdPercentage (\s a -> s {_acThresholdPercentage = a})

-- | The minimum number of things which must receive job execution notifications before the job can be aborted.
acMinNumberOfExecutedThings :: Lens' AbortCriteria Natural
acMinNumberOfExecutedThings = lens _acMinNumberOfExecutedThings (\s a -> s {_acMinNumberOfExecutedThings = a}) . _Nat

instance FromJSON AbortCriteria where
  parseJSON =
    withObject
      "AbortCriteria"
      ( \x ->
          AbortCriteria'
            <$> (x .: "failureType")
            <*> (x .: "action")
            <*> (x .: "thresholdPercentage")
            <*> (x .: "minNumberOfExecutedThings")
      )

instance Hashable AbortCriteria

instance NFData AbortCriteria

instance ToJSON AbortCriteria where
  toJSON AbortCriteria' {..} =
    object
      ( catMaybes
          [ Just ("failureType" .= _acFailureType),
            Just ("action" .= _acAction),
            Just ("thresholdPercentage" .= _acThresholdPercentage),
            Just
              ("minNumberOfExecutedThings" .= _acMinNumberOfExecutedThings)
          ]
      )
