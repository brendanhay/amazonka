{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.HTTPURLDestinationProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.HTTPURLDestinationProperties where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | HTTP URL destination properties.
--
--
--
-- /See:/ 'hTTPURLDestinationProperties' smart constructor.
newtype HTTPURLDestinationProperties = HTTPURLDestinationProperties'
  { _httpudpConfirmationURL ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HTTPURLDestinationProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httpudpConfirmationURL' - The URL used to confirm the HTTP topic rule destination URL.
hTTPURLDestinationProperties ::
  HTTPURLDestinationProperties
hTTPURLDestinationProperties =
  HTTPURLDestinationProperties' {_httpudpConfirmationURL = Nothing}

-- | The URL used to confirm the HTTP topic rule destination URL.
httpudpConfirmationURL :: Lens' HTTPURLDestinationProperties (Maybe Text)
httpudpConfirmationURL = lens _httpudpConfirmationURL (\s a -> s {_httpudpConfirmationURL = a})

instance FromJSON HTTPURLDestinationProperties where
  parseJSON =
    withObject
      "HTTPURLDestinationProperties"
      ( \x ->
          HTTPURLDestinationProperties' <$> (x .:? "confirmationUrl")
      )

instance Hashable HTTPURLDestinationProperties

instance NFData HTTPURLDestinationProperties
