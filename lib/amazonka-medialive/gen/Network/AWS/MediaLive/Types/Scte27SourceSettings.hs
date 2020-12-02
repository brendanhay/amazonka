{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte27SourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte27SourceSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Scte27 Source Settings
--
-- /See:/ 'scte27SourceSettings' smart constructor.
newtype Scte27SourceSettings = Scte27SourceSettings'
  { _sssPid ::
      Maybe Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Scte27SourceSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sssPid' - The pid field is used in conjunction with the caption selector languageCode field as follows:   - Specify PID and Language: Extracts captions from that PID; the language is "informational".   - Specify PID and omit Language: Extracts the specified PID.   - Omit PID and specify Language: Extracts the specified language, whichever PID that happens to be.   - Omit PID and omit Language: Valid only if source is DVB-Sub that is being passed through; all languages will be passed through.
scte27SourceSettings ::
  Scte27SourceSettings
scte27SourceSettings = Scte27SourceSettings' {_sssPid = Nothing}

-- | The pid field is used in conjunction with the caption selector languageCode field as follows:   - Specify PID and Language: Extracts captions from that PID; the language is "informational".   - Specify PID and omit Language: Extracts the specified PID.   - Omit PID and specify Language: Extracts the specified language, whichever PID that happens to be.   - Omit PID and omit Language: Valid only if source is DVB-Sub that is being passed through; all languages will be passed through.
sssPid :: Lens' Scte27SourceSettings (Maybe Natural)
sssPid = lens _sssPid (\s a -> s {_sssPid = a}) . mapping _Nat

instance FromJSON Scte27SourceSettings where
  parseJSON =
    withObject
      "Scte27SourceSettings"
      (\x -> Scte27SourceSettings' <$> (x .:? "pid"))

instance Hashable Scte27SourceSettings

instance NFData Scte27SourceSettings

instance ToJSON Scte27SourceSettings where
  toJSON Scte27SourceSettings' {..} =
    object (catMaybes [("pid" .=) <$> _sssPid])
