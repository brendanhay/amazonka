{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.ActiveContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.ActiveContext where

import Network.AWS.Lens
import Network.AWS.LexRuntime.Types.ActiveContextTimeToLive
import Network.AWS.Prelude

-- | A context is a variable that contains information about the current state of the conversation between a user and Amazon Lex. Context can be set automatically by Amazon Lex when an intent is fulfilled, or it can be set at runtime using the @PutContent@ , @PutText@ , or @PutSession@ operation.
--
--
--
-- /See:/ 'activeContext' smart constructor.
data ActiveContext = ActiveContext'
  { _acName :: !Text,
    _acTimeToLive :: !ActiveContextTimeToLive,
    _acParameters :: !(Map Text (Sensitive Text))
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActiveContext' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acName' - The name of the context.
--
-- * 'acTimeToLive' - The length of time or number of turns that a context remains active.
--
-- * 'acParameters' - State variables for the current context. You can use these values as default values for slots in subsequent events.
activeContext ::
  -- | 'acName'
  Text ->
  -- | 'acTimeToLive'
  ActiveContextTimeToLive ->
  ActiveContext
activeContext pName_ pTimeToLive_ =
  ActiveContext'
    { _acName = pName_,
      _acTimeToLive = pTimeToLive_,
      _acParameters = mempty
    }

-- | The name of the context.
acName :: Lens' ActiveContext Text
acName = lens _acName (\s a -> s {_acName = a})

-- | The length of time or number of turns that a context remains active.
acTimeToLive :: Lens' ActiveContext ActiveContextTimeToLive
acTimeToLive = lens _acTimeToLive (\s a -> s {_acTimeToLive = a})

-- | State variables for the current context. You can use these values as default values for slots in subsequent events.
acParameters :: Lens' ActiveContext (HashMap Text (Text))
acParameters = lens _acParameters (\s a -> s {_acParameters = a}) . _Map

instance FromJSON ActiveContext where
  parseJSON =
    withObject
      "ActiveContext"
      ( \x ->
          ActiveContext'
            <$> (x .: "name")
            <*> (x .: "timeToLive")
            <*> (x .:? "parameters" .!= mempty)
      )

instance Hashable ActiveContext

instance NFData ActiveContext

instance ToJSON ActiveContext where
  toJSON ActiveContext' {..} =
    object
      ( catMaybes
          [ Just ("name" .= _acName),
            Just ("timeToLive" .= _acTimeToLive),
            Just ("parameters" .= _acParameters)
          ]
      )
