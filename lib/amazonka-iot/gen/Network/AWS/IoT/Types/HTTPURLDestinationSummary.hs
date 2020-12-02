{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.HTTPURLDestinationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.HTTPURLDestinationSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an HTTP URL destination.
--
--
--
-- /See:/ 'hTTPURLDestinationSummary' smart constructor.
newtype HTTPURLDestinationSummary = HTTPURLDestinationSummary'
  { _httpudsConfirmationURL ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HTTPURLDestinationSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httpudsConfirmationURL' - The URL used to confirm ownership of or access to the HTTP topic rule destination URL.
hTTPURLDestinationSummary ::
  HTTPURLDestinationSummary
hTTPURLDestinationSummary =
  HTTPURLDestinationSummary' {_httpudsConfirmationURL = Nothing}

-- | The URL used to confirm ownership of or access to the HTTP topic rule destination URL.
httpudsConfirmationURL :: Lens' HTTPURLDestinationSummary (Maybe Text)
httpudsConfirmationURL = lens _httpudsConfirmationURL (\s a -> s {_httpudsConfirmationURL = a})

instance FromJSON HTTPURLDestinationSummary where
  parseJSON =
    withObject
      "HTTPURLDestinationSummary"
      (\x -> HTTPURLDestinationSummary' <$> (x .:? "confirmationUrl"))

instance Hashable HTTPURLDestinationSummary

instance NFData HTTPURLDestinationSummary
