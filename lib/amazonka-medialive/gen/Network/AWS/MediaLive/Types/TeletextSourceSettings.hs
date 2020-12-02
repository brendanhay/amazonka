{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.TeletextSourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.TeletextSourceSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Teletext Source Settings
--
-- /See:/ 'teletextSourceSettings' smart constructor.
newtype TeletextSourceSettings = TeletextSourceSettings'
  { _tssPageNumber ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TeletextSourceSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tssPageNumber' - Specifies the teletext page number within the data stream from which to extract captions. Range of 0x100 (256) to 0x8FF (2303). Unused for passthrough. Should be specified as a hexadecimal string with no "0x" prefix.
teletextSourceSettings ::
  TeletextSourceSettings
teletextSourceSettings =
  TeletextSourceSettings' {_tssPageNumber = Nothing}

-- | Specifies the teletext page number within the data stream from which to extract captions. Range of 0x100 (256) to 0x8FF (2303). Unused for passthrough. Should be specified as a hexadecimal string with no "0x" prefix.
tssPageNumber :: Lens' TeletextSourceSettings (Maybe Text)
tssPageNumber = lens _tssPageNumber (\s a -> s {_tssPageNumber = a})

instance FromJSON TeletextSourceSettings where
  parseJSON =
    withObject
      "TeletextSourceSettings"
      (\x -> TeletextSourceSettings' <$> (x .:? "pageNumber"))

instance Hashable TeletextSourceSettings

instance NFData TeletextSourceSettings

instance ToJSON TeletextSourceSettings where
  toJSON TeletextSourceSettings' {..} =
    object (catMaybes [("pageNumber" .=) <$> _tssPageNumber])
