{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.ReplayDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.ReplayDestination where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A @ReplayDestination@ object that contains details about a replay.
--
--
--
-- /See:/ 'replayDestination' smart constructor.
data ReplayDestination = ReplayDestination'
  { _rdFilterARNs ::
      !(Maybe [Text]),
    _rdARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplayDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdFilterARNs' - A list of ARNs for rules to replay events to.
--
-- * 'rdARN' - The ARN of the event bus to replay event to. You can replay events only to the event bus specified to create the archive.
replayDestination ::
  -- | 'rdARN'
  Text ->
  ReplayDestination
replayDestination pARN_ =
  ReplayDestination' {_rdFilterARNs = Nothing, _rdARN = pARN_}

-- | A list of ARNs for rules to replay events to.
rdFilterARNs :: Lens' ReplayDestination [Text]
rdFilterARNs = lens _rdFilterARNs (\s a -> s {_rdFilterARNs = a}) . _Default . _Coerce

-- | The ARN of the event bus to replay event to. You can replay events only to the event bus specified to create the archive.
rdARN :: Lens' ReplayDestination Text
rdARN = lens _rdARN (\s a -> s {_rdARN = a})

instance FromJSON ReplayDestination where
  parseJSON =
    withObject
      "ReplayDestination"
      ( \x ->
          ReplayDestination'
            <$> (x .:? "FilterArns" .!= mempty) <*> (x .: "Arn")
      )

instance Hashable ReplayDestination

instance NFData ReplayDestination

instance ToJSON ReplayDestination where
  toJSON ReplayDestination' {..} =
    object
      ( catMaybes
          [("FilterArns" .=) <$> _rdFilterARNs, Just ("Arn" .= _rdARN)]
      )
