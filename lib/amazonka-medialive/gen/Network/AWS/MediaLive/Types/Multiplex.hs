{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Multiplex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Multiplex where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.MultiplexOutputDestination
import Network.AWS.MediaLive.Types.MultiplexSettings
import Network.AWS.MediaLive.Types.MultiplexState
import Network.AWS.Prelude

-- | The multiplex object.
--
-- /See:/ 'multiplex' smart constructor.
data Multiplex = Multiplex'
  { _mState :: !(Maybe MultiplexState),
    _mARN :: !(Maybe Text),
    _mPipelinesRunningCount :: !(Maybe Int),
    _mAvailabilityZones :: !(Maybe [Text]),
    _mProgramCount :: !(Maybe Int),
    _mDestinations :: !(Maybe [MultiplexOutputDestination]),
    _mName :: !(Maybe Text),
    _mId :: !(Maybe Text),
    _mMultiplexSettings :: !(Maybe MultiplexSettings),
    _mTags :: !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Multiplex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mState' - The current state of the multiplex.
--
-- * 'mARN' - The unique arn of the multiplex.
--
-- * 'mPipelinesRunningCount' - The number of currently healthy pipelines.
--
-- * 'mAvailabilityZones' - A list of availability zones for the multiplex.
--
-- * 'mProgramCount' - The number of programs in the multiplex.
--
-- * 'mDestinations' - A list of the multiplex output destinations.
--
-- * 'mName' - The name of the multiplex.
--
-- * 'mId' - The unique id of the multiplex.
--
-- * 'mMultiplexSettings' - Configuration for a multiplex event.
--
-- * 'mTags' - A collection of key-value pairs.
multiplex ::
  Multiplex
multiplex =
  Multiplex'
    { _mState = Nothing,
      _mARN = Nothing,
      _mPipelinesRunningCount = Nothing,
      _mAvailabilityZones = Nothing,
      _mProgramCount = Nothing,
      _mDestinations = Nothing,
      _mName = Nothing,
      _mId = Nothing,
      _mMultiplexSettings = Nothing,
      _mTags = Nothing
    }

-- | The current state of the multiplex.
mState :: Lens' Multiplex (Maybe MultiplexState)
mState = lens _mState (\s a -> s {_mState = a})

-- | The unique arn of the multiplex.
mARN :: Lens' Multiplex (Maybe Text)
mARN = lens _mARN (\s a -> s {_mARN = a})

-- | The number of currently healthy pipelines.
mPipelinesRunningCount :: Lens' Multiplex (Maybe Int)
mPipelinesRunningCount = lens _mPipelinesRunningCount (\s a -> s {_mPipelinesRunningCount = a})

-- | A list of availability zones for the multiplex.
mAvailabilityZones :: Lens' Multiplex [Text]
mAvailabilityZones = lens _mAvailabilityZones (\s a -> s {_mAvailabilityZones = a}) . _Default . _Coerce

-- | The number of programs in the multiplex.
mProgramCount :: Lens' Multiplex (Maybe Int)
mProgramCount = lens _mProgramCount (\s a -> s {_mProgramCount = a})

-- | A list of the multiplex output destinations.
mDestinations :: Lens' Multiplex [MultiplexOutputDestination]
mDestinations = lens _mDestinations (\s a -> s {_mDestinations = a}) . _Default . _Coerce

-- | The name of the multiplex.
mName :: Lens' Multiplex (Maybe Text)
mName = lens _mName (\s a -> s {_mName = a})

-- | The unique id of the multiplex.
mId :: Lens' Multiplex (Maybe Text)
mId = lens _mId (\s a -> s {_mId = a})

-- | Configuration for a multiplex event.
mMultiplexSettings :: Lens' Multiplex (Maybe MultiplexSettings)
mMultiplexSettings = lens _mMultiplexSettings (\s a -> s {_mMultiplexSettings = a})

-- | A collection of key-value pairs.
mTags :: Lens' Multiplex (HashMap Text (Text))
mTags = lens _mTags (\s a -> s {_mTags = a}) . _Default . _Map

instance FromJSON Multiplex where
  parseJSON =
    withObject
      "Multiplex"
      ( \x ->
          Multiplex'
            <$> (x .:? "state")
            <*> (x .:? "arn")
            <*> (x .:? "pipelinesRunningCount")
            <*> (x .:? "availabilityZones" .!= mempty)
            <*> (x .:? "programCount")
            <*> (x .:? "destinations" .!= mempty)
            <*> (x .:? "name")
            <*> (x .:? "id")
            <*> (x .:? "multiplexSettings")
            <*> (x .:? "tags" .!= mempty)
      )

instance Hashable Multiplex

instance NFData Multiplex
