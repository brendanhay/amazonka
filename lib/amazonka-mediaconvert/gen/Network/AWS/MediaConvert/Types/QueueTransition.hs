{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.QueueTransition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.QueueTransition where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Description of the source and destination queues between which the job has moved, along with the timestamp of the move
--
-- /See:/ 'queueTransition' smart constructor.
data QueueTransition = QueueTransition'
  { _qtSourceQueue ::
      !(Maybe Text),
    _qtDestinationQueue :: !(Maybe Text),
    _qtTimestamp :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'QueueTransition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qtSourceQueue' - The queue that the job was on before the transition.
--
-- * 'qtDestinationQueue' - The queue that the job was on after the transition.
--
-- * 'qtTimestamp' - The time, in Unix epoch format, that the job moved from the source queue to the destination queue.
queueTransition ::
  QueueTransition
queueTransition =
  QueueTransition'
    { _qtSourceQueue = Nothing,
      _qtDestinationQueue = Nothing,
      _qtTimestamp = Nothing
    }

-- | The queue that the job was on before the transition.
qtSourceQueue :: Lens' QueueTransition (Maybe Text)
qtSourceQueue = lens _qtSourceQueue (\s a -> s {_qtSourceQueue = a})

-- | The queue that the job was on after the transition.
qtDestinationQueue :: Lens' QueueTransition (Maybe Text)
qtDestinationQueue = lens _qtDestinationQueue (\s a -> s {_qtDestinationQueue = a})

-- | The time, in Unix epoch format, that the job moved from the source queue to the destination queue.
qtTimestamp :: Lens' QueueTransition (Maybe UTCTime)
qtTimestamp = lens _qtTimestamp (\s a -> s {_qtTimestamp = a}) . mapping _Time

instance FromJSON QueueTransition where
  parseJSON =
    withObject
      "QueueTransition"
      ( \x ->
          QueueTransition'
            <$> (x .:? "sourceQueue")
            <*> (x .:? "destinationQueue")
            <*> (x .:? "timestamp")
      )

instance Hashable QueueTransition

instance NFData QueueTransition
