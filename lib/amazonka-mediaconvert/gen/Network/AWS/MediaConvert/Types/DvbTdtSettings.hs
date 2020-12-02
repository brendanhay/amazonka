{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DvbTdtSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DvbTdtSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Inserts DVB Time and Date Table (TDT) at the specified table repetition interval.
--
-- /See:/ 'dvbTdtSettings' smart constructor.
newtype DvbTdtSettings = DvbTdtSettings'
  { _dtsTdtInterval ::
      Maybe Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DvbTdtSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtsTdtInterval' - The number of milliseconds between instances of this table in the output transport stream.
dvbTdtSettings ::
  DvbTdtSettings
dvbTdtSettings = DvbTdtSettings' {_dtsTdtInterval = Nothing}

-- | The number of milliseconds between instances of this table in the output transport stream.
dtsTdtInterval :: Lens' DvbTdtSettings (Maybe Natural)
dtsTdtInterval = lens _dtsTdtInterval (\s a -> s {_dtsTdtInterval = a}) . mapping _Nat

instance FromJSON DvbTdtSettings where
  parseJSON =
    withObject
      "DvbTdtSettings"
      (\x -> DvbTdtSettings' <$> (x .:? "tdtInterval"))

instance Hashable DvbTdtSettings

instance NFData DvbTdtSettings

instance ToJSON DvbTdtSettings where
  toJSON DvbTdtSettings' {..} =
    object (catMaybes [("tdtInterval" .=) <$> _dtsTdtInterval])
