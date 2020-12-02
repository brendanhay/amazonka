{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HopDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HopDestination where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Optional. Configuration for a destination queue to which the job can hop once a customer-defined minimum wait time has passed.
--
-- /See:/ 'hopDestination' smart constructor.
data HopDestination = HopDestination'
  { _hdPriority :: !(Maybe Int),
    _hdQueue :: !(Maybe Text),
    _hdWaitMinutes :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HopDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hdPriority' - Optional. When you set up a job to use queue hopping, you can specify a different relative priority for the job in the destination queue. If you don't specify, the relative priority will remain the same as in the previous queue.
--
-- * 'hdQueue' - Optional unless the job is submitted on the default queue. When you set up a job to use queue hopping, you can specify a destination queue. This queue cannot be the original queue to which the job is submitted. If the original queue isn't the default queue and you don't specify the destination queue, the job will move to the default queue.
--
-- * 'hdWaitMinutes' - Required for setting up a job to use queue hopping. Minimum wait time in minutes until the job can hop to the destination queue. Valid range is 1 to 1440 minutes, inclusive.
hopDestination ::
  HopDestination
hopDestination =
  HopDestination'
    { _hdPriority = Nothing,
      _hdQueue = Nothing,
      _hdWaitMinutes = Nothing
    }

-- | Optional. When you set up a job to use queue hopping, you can specify a different relative priority for the job in the destination queue. If you don't specify, the relative priority will remain the same as in the previous queue.
hdPriority :: Lens' HopDestination (Maybe Int)
hdPriority = lens _hdPriority (\s a -> s {_hdPriority = a})

-- | Optional unless the job is submitted on the default queue. When you set up a job to use queue hopping, you can specify a destination queue. This queue cannot be the original queue to which the job is submitted. If the original queue isn't the default queue and you don't specify the destination queue, the job will move to the default queue.
hdQueue :: Lens' HopDestination (Maybe Text)
hdQueue = lens _hdQueue (\s a -> s {_hdQueue = a})

-- | Required for setting up a job to use queue hopping. Minimum wait time in minutes until the job can hop to the destination queue. Valid range is 1 to 1440 minutes, inclusive.
hdWaitMinutes :: Lens' HopDestination (Maybe Int)
hdWaitMinutes = lens _hdWaitMinutes (\s a -> s {_hdWaitMinutes = a})

instance FromJSON HopDestination where
  parseJSON =
    withObject
      "HopDestination"
      ( \x ->
          HopDestination'
            <$> (x .:? "priority") <*> (x .:? "queue") <*> (x .:? "waitMinutes")
      )

instance Hashable HopDestination

instance NFData HopDestination

instance ToJSON HopDestination where
  toJSON HopDestination' {..} =
    object
      ( catMaybes
          [ ("priority" .=) <$> _hdPriority,
            ("queue" .=) <$> _hdQueue,
            ("waitMinutes" .=) <$> _hdWaitMinutes
          ]
      )
