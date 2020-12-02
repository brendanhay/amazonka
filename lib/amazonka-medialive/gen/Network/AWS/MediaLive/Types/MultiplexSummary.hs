{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexSummary where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.MultiplexSettingsSummary
import Network.AWS.MediaLive.Types.MultiplexState
import Network.AWS.Prelude

-- | Placeholder documentation for MultiplexSummary
--
-- /See:/ 'multiplexSummary' smart constructor.
data MultiplexSummary = MultiplexSummary'
  { _msState ::
      !(Maybe MultiplexState),
    _msARN :: !(Maybe Text),
    _msPipelinesRunningCount :: !(Maybe Int),
    _msAvailabilityZones :: !(Maybe [Text]),
    _msProgramCount :: !(Maybe Int),
    _msName :: !(Maybe Text),
    _msId :: !(Maybe Text),
    _msMultiplexSettings :: !(Maybe MultiplexSettingsSummary),
    _msTags :: !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MultiplexSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msState' - The current state of the multiplex.
--
-- * 'msARN' - The unique arn of the multiplex.
--
-- * 'msPipelinesRunningCount' - The number of currently healthy pipelines.
--
-- * 'msAvailabilityZones' - A list of availability zones for the multiplex.
--
-- * 'msProgramCount' - The number of programs in the multiplex.
--
-- * 'msName' - The name of the multiplex.
--
-- * 'msId' - The unique id of the multiplex.
--
-- * 'msMultiplexSettings' - Configuration for a multiplex event.
--
-- * 'msTags' - A collection of key-value pairs.
multiplexSummary ::
  MultiplexSummary
multiplexSummary =
  MultiplexSummary'
    { _msState = Nothing,
      _msARN = Nothing,
      _msPipelinesRunningCount = Nothing,
      _msAvailabilityZones = Nothing,
      _msProgramCount = Nothing,
      _msName = Nothing,
      _msId = Nothing,
      _msMultiplexSettings = Nothing,
      _msTags = Nothing
    }

-- | The current state of the multiplex.
msState :: Lens' MultiplexSummary (Maybe MultiplexState)
msState = lens _msState (\s a -> s {_msState = a})

-- | The unique arn of the multiplex.
msARN :: Lens' MultiplexSummary (Maybe Text)
msARN = lens _msARN (\s a -> s {_msARN = a})

-- | The number of currently healthy pipelines.
msPipelinesRunningCount :: Lens' MultiplexSummary (Maybe Int)
msPipelinesRunningCount = lens _msPipelinesRunningCount (\s a -> s {_msPipelinesRunningCount = a})

-- | A list of availability zones for the multiplex.
msAvailabilityZones :: Lens' MultiplexSummary [Text]
msAvailabilityZones = lens _msAvailabilityZones (\s a -> s {_msAvailabilityZones = a}) . _Default . _Coerce

-- | The number of programs in the multiplex.
msProgramCount :: Lens' MultiplexSummary (Maybe Int)
msProgramCount = lens _msProgramCount (\s a -> s {_msProgramCount = a})

-- | The name of the multiplex.
msName :: Lens' MultiplexSummary (Maybe Text)
msName = lens _msName (\s a -> s {_msName = a})

-- | The unique id of the multiplex.
msId :: Lens' MultiplexSummary (Maybe Text)
msId = lens _msId (\s a -> s {_msId = a})

-- | Configuration for a multiplex event.
msMultiplexSettings :: Lens' MultiplexSummary (Maybe MultiplexSettingsSummary)
msMultiplexSettings = lens _msMultiplexSettings (\s a -> s {_msMultiplexSettings = a})

-- | A collection of key-value pairs.
msTags :: Lens' MultiplexSummary (HashMap Text (Text))
msTags = lens _msTags (\s a -> s {_msTags = a}) . _Default . _Map

instance FromJSON MultiplexSummary where
  parseJSON =
    withObject
      "MultiplexSummary"
      ( \x ->
          MultiplexSummary'
            <$> (x .:? "state")
            <*> (x .:? "arn")
            <*> (x .:? "pipelinesRunningCount")
            <*> (x .:? "availabilityZones" .!= mempty)
            <*> (x .:? "programCount")
            <*> (x .:? "name")
            <*> (x .:? "id")
            <*> (x .:? "multiplexSettings")
            <*> (x .:? "tags" .!= mempty)
      )

instance Hashable MultiplexSummary

instance NFData MultiplexSummary
