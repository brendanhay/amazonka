{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AWSJobAbortCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AWSJobAbortCriteria where

import Network.AWS.IoT.Types.AWSJobAbortCriteriaAbortAction
import Network.AWS.IoT.Types.AWSJobAbortCriteriaFailureType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The criteria that determine when and how a job abort takes place.
--
--
--
-- /See:/ 'awsJobAbortCriteria' smart constructor.
data AWSJobAbortCriteria = AWSJobAbortCriteria'
  { _ajacFailureType ::
      !AWSJobAbortCriteriaFailureType,
    _ajacAction :: !AWSJobAbortCriteriaAbortAction,
    _ajacThresholdPercentage :: !Double,
    _ajacMinNumberOfExecutedThings :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AWSJobAbortCriteria' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ajacFailureType' - The type of job execution failures that can initiate a job abort.
--
-- * 'ajacAction' - The type of job action to take to initiate the job abort.
--
-- * 'ajacThresholdPercentage' - The minimum percentage of job execution failures that must occur to initiate the job abort. AWS IoT supports up to two digits after the decimal (for example, 10.9 and 10.99, but not 10.999).
--
-- * 'ajacMinNumberOfExecutedThings' - The minimum number of things which must receive job execution notifications before the job can be aborted.
awsJobAbortCriteria ::
  -- | 'ajacFailureType'
  AWSJobAbortCriteriaFailureType ->
  -- | 'ajacAction'
  AWSJobAbortCriteriaAbortAction ->
  -- | 'ajacThresholdPercentage'
  Double ->
  -- | 'ajacMinNumberOfExecutedThings'
  Natural ->
  AWSJobAbortCriteria
awsJobAbortCriteria
  pFailureType_
  pAction_
  pThresholdPercentage_
  pMinNumberOfExecutedThings_ =
    AWSJobAbortCriteria'
      { _ajacFailureType = pFailureType_,
        _ajacAction = pAction_,
        _ajacThresholdPercentage = pThresholdPercentage_,
        _ajacMinNumberOfExecutedThings =
          _Nat # pMinNumberOfExecutedThings_
      }

-- | The type of job execution failures that can initiate a job abort.
ajacFailureType :: Lens' AWSJobAbortCriteria AWSJobAbortCriteriaFailureType
ajacFailureType = lens _ajacFailureType (\s a -> s {_ajacFailureType = a})

-- | The type of job action to take to initiate the job abort.
ajacAction :: Lens' AWSJobAbortCriteria AWSJobAbortCriteriaAbortAction
ajacAction = lens _ajacAction (\s a -> s {_ajacAction = a})

-- | The minimum percentage of job execution failures that must occur to initiate the job abort. AWS IoT supports up to two digits after the decimal (for example, 10.9 and 10.99, but not 10.999).
ajacThresholdPercentage :: Lens' AWSJobAbortCriteria Double
ajacThresholdPercentage = lens _ajacThresholdPercentage (\s a -> s {_ajacThresholdPercentage = a})

-- | The minimum number of things which must receive job execution notifications before the job can be aborted.
ajacMinNumberOfExecutedThings :: Lens' AWSJobAbortCriteria Natural
ajacMinNumberOfExecutedThings = lens _ajacMinNumberOfExecutedThings (\s a -> s {_ajacMinNumberOfExecutedThings = a}) . _Nat

instance Hashable AWSJobAbortCriteria

instance NFData AWSJobAbortCriteria

instance ToJSON AWSJobAbortCriteria where
  toJSON AWSJobAbortCriteria' {..} =
    object
      ( catMaybes
          [ Just ("failureType" .= _ajacFailureType),
            Just ("action" .= _ajacAction),
            Just ("thresholdPercentage" .= _ajacThresholdPercentage),
            Just
              ("minNumberOfExecutedThings" .= _ajacMinNumberOfExecutedThings)
          ]
      )
