{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DvbSubSourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DvbSubSourceSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | DVB Sub Source Settings
--
-- /See:/ 'dvbSubSourceSettings' smart constructor.
newtype DvbSubSourceSettings = DvbSubSourceSettings'
  { _dsssPid ::
      Maybe Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DvbSubSourceSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsssPid' - When using DVB-Sub with Burn-In or SMPTE-TT, use this PID for the source content. Unused for DVB-Sub passthrough. All DVB-Sub content is passed through, regardless of selectors.
dvbSubSourceSettings ::
  DvbSubSourceSettings
dvbSubSourceSettings = DvbSubSourceSettings' {_dsssPid = Nothing}

-- | When using DVB-Sub with Burn-In or SMPTE-TT, use this PID for the source content. Unused for DVB-Sub passthrough. All DVB-Sub content is passed through, regardless of selectors.
dsssPid :: Lens' DvbSubSourceSettings (Maybe Natural)
dsssPid = lens _dsssPid (\s a -> s {_dsssPid = a}) . mapping _Nat

instance FromJSON DvbSubSourceSettings where
  parseJSON =
    withObject
      "DvbSubSourceSettings"
      (\x -> DvbSubSourceSettings' <$> (x .:? "pid"))

instance Hashable DvbSubSourceSettings

instance NFData DvbSubSourceSettings

instance ToJSON DvbSubSourceSettings where
  toJSON DvbSubSourceSettings' {..} =
    object (catMaybes [("pid" .=) <$> _dsssPid])
