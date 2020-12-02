{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.ActiveContextTimeToLive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.ActiveContextTimeToLive where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The length of time or number of turns that a context remains active.
--
--
--
-- /See:/ 'activeContextTimeToLive' smart constructor.
data ActiveContextTimeToLive = ActiveContextTimeToLive'
  { _acttlTurnsToLive ::
      !(Maybe Nat),
    _acttlTimeToLiveInSeconds :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActiveContextTimeToLive' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acttlTurnsToLive' - The number of conversation turns that the context should be active. A conversation turn is one @PostContent@ or @PostText@ request and the corresponding response from Amazon Lex.
--
-- * 'acttlTimeToLiveInSeconds' - The number of seconds that the context should be active after it is first sent in a @PostContent@ or @PostText@ response. You can set the value between 5 and 86,400 seconds (24 hours).
activeContextTimeToLive ::
  ActiveContextTimeToLive
activeContextTimeToLive =
  ActiveContextTimeToLive'
    { _acttlTurnsToLive = Nothing,
      _acttlTimeToLiveInSeconds = Nothing
    }

-- | The number of conversation turns that the context should be active. A conversation turn is one @PostContent@ or @PostText@ request and the corresponding response from Amazon Lex.
acttlTurnsToLive :: Lens' ActiveContextTimeToLive (Maybe Natural)
acttlTurnsToLive = lens _acttlTurnsToLive (\s a -> s {_acttlTurnsToLive = a}) . mapping _Nat

-- | The number of seconds that the context should be active after it is first sent in a @PostContent@ or @PostText@ response. You can set the value between 5 and 86,400 seconds (24 hours).
acttlTimeToLiveInSeconds :: Lens' ActiveContextTimeToLive (Maybe Natural)
acttlTimeToLiveInSeconds = lens _acttlTimeToLiveInSeconds (\s a -> s {_acttlTimeToLiveInSeconds = a}) . mapping _Nat

instance FromJSON ActiveContextTimeToLive where
  parseJSON =
    withObject
      "ActiveContextTimeToLive"
      ( \x ->
          ActiveContextTimeToLive'
            <$> (x .:? "turnsToLive") <*> (x .:? "timeToLiveInSeconds")
      )

instance Hashable ActiveContextTimeToLive

instance NFData ActiveContextTimeToLive

instance ToJSON ActiveContextTimeToLive where
  toJSON ActiveContextTimeToLive' {..} =
    object
      ( catMaybes
          [ ("turnsToLive" .=) <$> _acttlTurnsToLive,
            ("timeToLiveInSeconds" .=) <$> _acttlTimeToLiveInSeconds
          ]
      )
