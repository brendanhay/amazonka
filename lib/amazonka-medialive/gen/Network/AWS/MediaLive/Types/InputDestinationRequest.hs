{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDestinationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDestinationRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Endpoint settings for a PUSH type input.
--
-- /See:/ 'inputDestinationRequest' smart constructor.
newtype InputDestinationRequest = InputDestinationRequest'
  { _idrStreamName ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputDestinationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idrStreamName' - A unique name for the location the RTMP stream is being pushed to.
inputDestinationRequest ::
  InputDestinationRequest
inputDestinationRequest =
  InputDestinationRequest' {_idrStreamName = Nothing}

-- | A unique name for the location the RTMP stream is being pushed to.
idrStreamName :: Lens' InputDestinationRequest (Maybe Text)
idrStreamName = lens _idrStreamName (\s a -> s {_idrStreamName = a})

instance Hashable InputDestinationRequest

instance NFData InputDestinationRequest

instance ToJSON InputDestinationRequest where
  toJSON InputDestinationRequest' {..} =
    object (catMaybes [("streamName" .=) <$> _idrStreamName])
