{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ProgressCounters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ProgressCounters where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An aggregate of step execution statuses displayed in the AWS Console for a multi-Region and multi-account Automation execution.
--
--
--
-- /See:/ 'progressCounters' smart constructor.
data ProgressCounters = ProgressCounters'
  { _pcFailedSteps ::
      !(Maybe Int),
    _pcCancelledSteps :: !(Maybe Int),
    _pcSuccessSteps :: !(Maybe Int),
    _pcTotalSteps :: !(Maybe Int),
    _pcTimedOutSteps :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProgressCounters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcFailedSteps' - The total number of steps that failed to run in all specified AWS Regions and accounts for the current Automation execution.
--
-- * 'pcCancelledSteps' - The total number of steps that the system cancelled in all specified AWS Regions and accounts for the current Automation execution.
--
-- * 'pcSuccessSteps' - The total number of steps that successfully completed in all specified AWS Regions and accounts for the current Automation execution.
--
-- * 'pcTotalSteps' - The total number of steps run in all specified AWS Regions and accounts for the current Automation execution.
--
-- * 'pcTimedOutSteps' - The total number of steps that timed out in all specified AWS Regions and accounts for the current Automation execution.
progressCounters ::
  ProgressCounters
progressCounters =
  ProgressCounters'
    { _pcFailedSteps = Nothing,
      _pcCancelledSteps = Nothing,
      _pcSuccessSteps = Nothing,
      _pcTotalSteps = Nothing,
      _pcTimedOutSteps = Nothing
    }

-- | The total number of steps that failed to run in all specified AWS Regions and accounts for the current Automation execution.
pcFailedSteps :: Lens' ProgressCounters (Maybe Int)
pcFailedSteps = lens _pcFailedSteps (\s a -> s {_pcFailedSteps = a})

-- | The total number of steps that the system cancelled in all specified AWS Regions and accounts for the current Automation execution.
pcCancelledSteps :: Lens' ProgressCounters (Maybe Int)
pcCancelledSteps = lens _pcCancelledSteps (\s a -> s {_pcCancelledSteps = a})

-- | The total number of steps that successfully completed in all specified AWS Regions and accounts for the current Automation execution.
pcSuccessSteps :: Lens' ProgressCounters (Maybe Int)
pcSuccessSteps = lens _pcSuccessSteps (\s a -> s {_pcSuccessSteps = a})

-- | The total number of steps run in all specified AWS Regions and accounts for the current Automation execution.
pcTotalSteps :: Lens' ProgressCounters (Maybe Int)
pcTotalSteps = lens _pcTotalSteps (\s a -> s {_pcTotalSteps = a})

-- | The total number of steps that timed out in all specified AWS Regions and accounts for the current Automation execution.
pcTimedOutSteps :: Lens' ProgressCounters (Maybe Int)
pcTimedOutSteps = lens _pcTimedOutSteps (\s a -> s {_pcTimedOutSteps = a})

instance FromJSON ProgressCounters where
  parseJSON =
    withObject
      "ProgressCounters"
      ( \x ->
          ProgressCounters'
            <$> (x .:? "FailedSteps")
            <*> (x .:? "CancelledSteps")
            <*> (x .:? "SuccessSteps")
            <*> (x .:? "TotalSteps")
            <*> (x .:? "TimedOutSteps")
      )

instance Hashable ProgressCounters

instance NFData ProgressCounters
