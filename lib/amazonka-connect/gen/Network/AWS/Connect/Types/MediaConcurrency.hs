{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.MediaConcurrency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.MediaConcurrency where

import Network.AWS.Connect.Types.Channel
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about which channels are supported, and how many contacts an agent can have on a channel simultaneously.
--
--
--
-- /See:/ 'mediaConcurrency' smart constructor.
data MediaConcurrency = MediaConcurrency'
  { _mcChannel :: !Channel,
    _mcConcurrency :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MediaConcurrency' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcChannel' - The channels that agents can handle in the Contact Control Panel (CCP).
--
-- * 'mcConcurrency' - The number of contacts an agent can have on a channel simultaneously.
mediaConcurrency ::
  -- | 'mcChannel'
  Channel ->
  -- | 'mcConcurrency'
  Natural ->
  MediaConcurrency
mediaConcurrency pChannel_ pConcurrency_ =
  MediaConcurrency'
    { _mcChannel = pChannel_,
      _mcConcurrency = _Nat # pConcurrency_
    }

-- | The channels that agents can handle in the Contact Control Panel (CCP).
mcChannel :: Lens' MediaConcurrency Channel
mcChannel = lens _mcChannel (\s a -> s {_mcChannel = a})

-- | The number of contacts an agent can have on a channel simultaneously.
mcConcurrency :: Lens' MediaConcurrency Natural
mcConcurrency = lens _mcConcurrency (\s a -> s {_mcConcurrency = a}) . _Nat

instance FromJSON MediaConcurrency where
  parseJSON =
    withObject
      "MediaConcurrency"
      ( \x ->
          MediaConcurrency' <$> (x .: "Channel") <*> (x .: "Concurrency")
      )

instance Hashable MediaConcurrency

instance NFData MediaConcurrency

instance ToJSON MediaConcurrency where
  toJSON MediaConcurrency' {..} =
    object
      ( catMaybes
          [ Just ("Channel" .= _mcChannel),
            Just ("Concurrency" .= _mcConcurrency)
          ]
      )
