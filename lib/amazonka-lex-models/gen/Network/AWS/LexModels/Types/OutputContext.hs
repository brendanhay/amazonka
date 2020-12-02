{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.OutputContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.OutputContext where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The specification of an output context that is set when an intent is fulfilled.
--
--
--
-- /See:/ 'outputContext' smart constructor.
data OutputContext = OutputContext'
  { _ocName :: !Text,
    _ocTimeToLiveInSeconds :: !Nat,
    _ocTurnsToLive :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OutputContext' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocName' - The name of the context.
--
-- * 'ocTimeToLiveInSeconds' - The number of seconds that the context should be active after it is first sent in a @PostContent@ or @PostText@ response. You can set the value between 5 and 86,400 seconds (24 hours).
--
-- * 'ocTurnsToLive' - The number of conversation turns that the context should be active. A conversation turn is one @PostContent@ or @PostText@ request and the corresponding response from Amazon Lex.
outputContext ::
  -- | 'ocName'
  Text ->
  -- | 'ocTimeToLiveInSeconds'
  Natural ->
  -- | 'ocTurnsToLive'
  Natural ->
  OutputContext
outputContext pName_ pTimeToLiveInSeconds_ pTurnsToLive_ =
  OutputContext'
    { _ocName = pName_,
      _ocTimeToLiveInSeconds = _Nat # pTimeToLiveInSeconds_,
      _ocTurnsToLive = _Nat # pTurnsToLive_
    }

-- | The name of the context.
ocName :: Lens' OutputContext Text
ocName = lens _ocName (\s a -> s {_ocName = a})

-- | The number of seconds that the context should be active after it is first sent in a @PostContent@ or @PostText@ response. You can set the value between 5 and 86,400 seconds (24 hours).
ocTimeToLiveInSeconds :: Lens' OutputContext Natural
ocTimeToLiveInSeconds = lens _ocTimeToLiveInSeconds (\s a -> s {_ocTimeToLiveInSeconds = a}) . _Nat

-- | The number of conversation turns that the context should be active. A conversation turn is one @PostContent@ or @PostText@ request and the corresponding response from Amazon Lex.
ocTurnsToLive :: Lens' OutputContext Natural
ocTurnsToLive = lens _ocTurnsToLive (\s a -> s {_ocTurnsToLive = a}) . _Nat

instance FromJSON OutputContext where
  parseJSON =
    withObject
      "OutputContext"
      ( \x ->
          OutputContext'
            <$> (x .: "name")
            <*> (x .: "timeToLiveInSeconds")
            <*> (x .: "turnsToLive")
      )

instance Hashable OutputContext

instance NFData OutputContext

instance ToJSON OutputContext where
  toJSON OutputContext' {..} =
    object
      ( catMaybes
          [ Just ("name" .= _ocName),
            Just ("timeToLiveInSeconds" .= _ocTimeToLiveInSeconds),
            Just ("turnsToLive" .= _ocTurnsToLive)
          ]
      )
