{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.AttemptDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.AttemptDetail where

import Network.AWS.Batch.Types.AttemptContainerDetail
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing a job attempt.
--
--
--
-- /See:/ 'attemptDetail' smart constructor.
data AttemptDetail = AttemptDetail'
  { _adStoppedAt ::
      !(Maybe Integer),
    _adStartedAt :: !(Maybe Integer),
    _adContainer :: !(Maybe AttemptContainerDetail),
    _adStatusReason :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttemptDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adStoppedAt' - The Unix timestamp (in milliseconds) for when the attempt was stopped (when the attempt transitioned from the @RUNNING@ state to a terminal state, such as @SUCCEEDED@ or @FAILED@ ).
--
-- * 'adStartedAt' - The Unix timestamp (in milliseconds) for when the attempt was started (when the attempt transitioned from the @STARTING@ state to the @RUNNING@ state).
--
-- * 'adContainer' - Details about the container in this job attempt.
--
-- * 'adStatusReason' - A short, human-readable string to provide additional details about the current status of the job attempt.
attemptDetail ::
  AttemptDetail
attemptDetail =
  AttemptDetail'
    { _adStoppedAt = Nothing,
      _adStartedAt = Nothing,
      _adContainer = Nothing,
      _adStatusReason = Nothing
    }

-- | The Unix timestamp (in milliseconds) for when the attempt was stopped (when the attempt transitioned from the @RUNNING@ state to a terminal state, such as @SUCCEEDED@ or @FAILED@ ).
adStoppedAt :: Lens' AttemptDetail (Maybe Integer)
adStoppedAt = lens _adStoppedAt (\s a -> s {_adStoppedAt = a})

-- | The Unix timestamp (in milliseconds) for when the attempt was started (when the attempt transitioned from the @STARTING@ state to the @RUNNING@ state).
adStartedAt :: Lens' AttemptDetail (Maybe Integer)
adStartedAt = lens _adStartedAt (\s a -> s {_adStartedAt = a})

-- | Details about the container in this job attempt.
adContainer :: Lens' AttemptDetail (Maybe AttemptContainerDetail)
adContainer = lens _adContainer (\s a -> s {_adContainer = a})

-- | A short, human-readable string to provide additional details about the current status of the job attempt.
adStatusReason :: Lens' AttemptDetail (Maybe Text)
adStatusReason = lens _adStatusReason (\s a -> s {_adStatusReason = a})

instance FromJSON AttemptDetail where
  parseJSON =
    withObject
      "AttemptDetail"
      ( \x ->
          AttemptDetail'
            <$> (x .:? "stoppedAt")
            <*> (x .:? "startedAt")
            <*> (x .:? "container")
            <*> (x .:? "statusReason")
      )

instance Hashable AttemptDetail

instance NFData AttemptDetail
