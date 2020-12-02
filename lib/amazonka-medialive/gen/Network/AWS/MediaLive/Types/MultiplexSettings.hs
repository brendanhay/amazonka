{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains configuration for a Multiplex event
--
-- /See:/ 'multiplexSettings' smart constructor.
data MultiplexSettings = MultiplexSettings'
  { _msMaximumVideoBufferDelayMilliseconds ::
      !(Maybe Nat),
    _msTransportStreamReservedBitrate :: !(Maybe Nat),
    _msTransportStreamBitrate :: !Nat,
    _msTransportStreamId :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MultiplexSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msMaximumVideoBufferDelayMilliseconds' - Maximum video buffer delay in milliseconds.
--
-- * 'msTransportStreamReservedBitrate' - Transport stream reserved bit rate.
--
-- * 'msTransportStreamBitrate' - Transport stream bit rate.
--
-- * 'msTransportStreamId' - Transport stream ID.
multiplexSettings ::
  -- | 'msTransportStreamBitrate'
  Natural ->
  -- | 'msTransportStreamId'
  Natural ->
  MultiplexSettings
multiplexSettings pTransportStreamBitrate_ pTransportStreamId_ =
  MultiplexSettings'
    { _msMaximumVideoBufferDelayMilliseconds =
        Nothing,
      _msTransportStreamReservedBitrate = Nothing,
      _msTransportStreamBitrate = _Nat # pTransportStreamBitrate_,
      _msTransportStreamId = _Nat # pTransportStreamId_
    }

-- | Maximum video buffer delay in milliseconds.
msMaximumVideoBufferDelayMilliseconds :: Lens' MultiplexSettings (Maybe Natural)
msMaximumVideoBufferDelayMilliseconds = lens _msMaximumVideoBufferDelayMilliseconds (\s a -> s {_msMaximumVideoBufferDelayMilliseconds = a}) . mapping _Nat

-- | Transport stream reserved bit rate.
msTransportStreamReservedBitrate :: Lens' MultiplexSettings (Maybe Natural)
msTransportStreamReservedBitrate = lens _msTransportStreamReservedBitrate (\s a -> s {_msTransportStreamReservedBitrate = a}) . mapping _Nat

-- | Transport stream bit rate.
msTransportStreamBitrate :: Lens' MultiplexSettings Natural
msTransportStreamBitrate = lens _msTransportStreamBitrate (\s a -> s {_msTransportStreamBitrate = a}) . _Nat

-- | Transport stream ID.
msTransportStreamId :: Lens' MultiplexSettings Natural
msTransportStreamId = lens _msTransportStreamId (\s a -> s {_msTransportStreamId = a}) . _Nat

instance FromJSON MultiplexSettings where
  parseJSON =
    withObject
      "MultiplexSettings"
      ( \x ->
          MultiplexSettings'
            <$> (x .:? "maximumVideoBufferDelayMilliseconds")
            <*> (x .:? "transportStreamReservedBitrate")
            <*> (x .: "transportStreamBitrate")
            <*> (x .: "transportStreamId")
      )

instance Hashable MultiplexSettings

instance NFData MultiplexSettings

instance ToJSON MultiplexSettings where
  toJSON MultiplexSettings' {..} =
    object
      ( catMaybes
          [ ("maximumVideoBufferDelayMilliseconds" .=)
              <$> _msMaximumVideoBufferDelayMilliseconds,
            ("transportStreamReservedBitrate" .=)
              <$> _msTransportStreamReservedBitrate,
            Just ("transportStreamBitrate" .= _msTransportStreamBitrate),
            Just ("transportStreamId" .= _msTransportStreamId)
          ]
      )
