{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexSettingsSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexSettingsSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains summary configuration for a Multiplex event.
--
-- /See:/ 'multiplexSettingsSummary' smart constructor.
newtype MultiplexSettingsSummary = MultiplexSettingsSummary'
  { _mssTransportStreamBitrate ::
      Maybe Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MultiplexSettingsSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mssTransportStreamBitrate' - Transport stream bit rate.
multiplexSettingsSummary ::
  MultiplexSettingsSummary
multiplexSettingsSummary =
  MultiplexSettingsSummary' {_mssTransportStreamBitrate = Nothing}

-- | Transport stream bit rate.
mssTransportStreamBitrate :: Lens' MultiplexSettingsSummary (Maybe Natural)
mssTransportStreamBitrate = lens _mssTransportStreamBitrate (\s a -> s {_mssTransportStreamBitrate = a}) . mapping _Nat

instance FromJSON MultiplexSettingsSummary where
  parseJSON =
    withObject
      "MultiplexSettingsSummary"
      ( \x ->
          MultiplexSettingsSummary' <$> (x .:? "transportStreamBitrate")
      )

instance Hashable MultiplexSettingsSummary

instance NFData MultiplexSettingsSummary
