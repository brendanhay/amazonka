{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Session
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Session where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about a session.
--
--
--
-- /See:/ 'session' smart constructor.
data Session = Session'
  { _sesStopTimestamp :: !(Maybe Text),
    _sesDuration :: !(Maybe Int),
    _sesStartTimestamp :: !Text,
    _sesId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Session' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sesStopTimestamp' - The date and time when the session ended.
--
-- * 'sesDuration' - The duration of the session, in milliseconds.
--
-- * 'sesStartTimestamp' - The date and time when the session began.
--
-- * 'sesId' - The unique identifier for the session.
session ::
  -- | 'sesStartTimestamp'
  Text ->
  -- | 'sesId'
  Text ->
  Session
session pStartTimestamp_ pId_ =
  Session'
    { _sesStopTimestamp = Nothing,
      _sesDuration = Nothing,
      _sesStartTimestamp = pStartTimestamp_,
      _sesId = pId_
    }

-- | The date and time when the session ended.
sesStopTimestamp :: Lens' Session (Maybe Text)
sesStopTimestamp = lens _sesStopTimestamp (\s a -> s {_sesStopTimestamp = a})

-- | The duration of the session, in milliseconds.
sesDuration :: Lens' Session (Maybe Int)
sesDuration = lens _sesDuration (\s a -> s {_sesDuration = a})

-- | The date and time when the session began.
sesStartTimestamp :: Lens' Session Text
sesStartTimestamp = lens _sesStartTimestamp (\s a -> s {_sesStartTimestamp = a})

-- | The unique identifier for the session.
sesId :: Lens' Session Text
sesId = lens _sesId (\s a -> s {_sesId = a})

instance Hashable Session

instance NFData Session

instance ToJSON Session where
  toJSON Session' {..} =
    object
      ( catMaybes
          [ ("StopTimestamp" .=) <$> _sesStopTimestamp,
            ("Duration" .=) <$> _sesDuration,
            Just ("StartTimestamp" .= _sesStartTimestamp),
            Just ("Id" .= _sesId)
          ]
      )
