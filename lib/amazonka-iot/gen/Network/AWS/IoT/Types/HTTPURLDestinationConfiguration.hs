{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.HTTPURLDestinationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.HTTPURLDestinationConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | HTTP URL destination configuration used by the topic rule's HTTP action.
--
--
--
-- /See:/ 'hTTPURLDestinationConfiguration' smart constructor.
newtype HTTPURLDestinationConfiguration = HTTPURLDestinationConfiguration'
  { _httpudcConfirmationURL ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HTTPURLDestinationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httpudcConfirmationURL' - The URL AWS IoT uses to confirm ownership of or access to the topic rule destination URL.
hTTPURLDestinationConfiguration ::
  -- | 'httpudcConfirmationURL'
  Text ->
  HTTPURLDestinationConfiguration
hTTPURLDestinationConfiguration pConfirmationURL_ =
  HTTPURLDestinationConfiguration'
    { _httpudcConfirmationURL =
        pConfirmationURL_
    }

-- | The URL AWS IoT uses to confirm ownership of or access to the topic rule destination URL.
httpudcConfirmationURL :: Lens' HTTPURLDestinationConfiguration Text
httpudcConfirmationURL = lens _httpudcConfirmationURL (\s a -> s {_httpudcConfirmationURL = a})

instance Hashable HTTPURLDestinationConfiguration

instance NFData HTTPURLDestinationConfiguration

instance ToJSON HTTPURLDestinationConfiguration where
  toJSON HTTPURLDestinationConfiguration' {..} =
    object
      (catMaybes [Just ("confirmationUrl" .= _httpudcConfirmationURL)])
