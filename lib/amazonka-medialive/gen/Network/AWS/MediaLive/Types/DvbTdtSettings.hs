{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DvbTdtSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DvbTdtSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | DVB Time and Date Table (SDT)
--
-- /See:/ 'dvbTdtSettings' smart constructor.
newtype DvbTdtSettings = DvbTdtSettings'
  { _dtsRepInterval ::
      Maybe Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DvbTdtSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtsRepInterval' - The number of milliseconds between instances of this table in the output transport stream.
dvbTdtSettings ::
  DvbTdtSettings
dvbTdtSettings = DvbTdtSettings' {_dtsRepInterval = Nothing}

-- | The number of milliseconds between instances of this table in the output transport stream.
dtsRepInterval :: Lens' DvbTdtSettings (Maybe Natural)
dtsRepInterval = lens _dtsRepInterval (\s a -> s {_dtsRepInterval = a}) . mapping _Nat

instance FromJSON DvbTdtSettings where
  parseJSON =
    withObject
      "DvbTdtSettings"
      (\x -> DvbTdtSettings' <$> (x .:? "repInterval"))

instance Hashable DvbTdtSettings

instance NFData DvbTdtSettings

instance ToJSON DvbTdtSettings where
  toJSON DvbTdtSettings' {..} =
    object (catMaybes [("repInterval" .=) <$> _dtsRepInterval])
