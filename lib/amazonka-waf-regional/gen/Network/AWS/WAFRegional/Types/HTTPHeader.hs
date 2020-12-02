{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.HTTPHeader
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.HTTPHeader where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The response from a 'GetSampledRequests' request includes an @HTTPHeader@ complex type that appears as @Headers@ in the response syntax. @HTTPHeader@ contains the names and values of all of the headers that appear in one of the web requests that were returned by @GetSampledRequests@ .
--
--
--
-- /See:/ 'hTTPHeader' smart constructor.
data HTTPHeader = HTTPHeader'
  { _httphValue :: !(Maybe Text),
    _httphName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HTTPHeader' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httphValue' - The value of one of the headers in the sampled web request.
--
-- * 'httphName' - The name of one of the headers in the sampled web request.
hTTPHeader ::
  HTTPHeader
hTTPHeader =
  HTTPHeader' {_httphValue = Nothing, _httphName = Nothing}

-- | The value of one of the headers in the sampled web request.
httphValue :: Lens' HTTPHeader (Maybe Text)
httphValue = lens _httphValue (\s a -> s {_httphValue = a})

-- | The name of one of the headers in the sampled web request.
httphName :: Lens' HTTPHeader (Maybe Text)
httphName = lens _httphName (\s a -> s {_httphName = a})

instance FromJSON HTTPHeader where
  parseJSON =
    withObject
      "HTTPHeader"
      (\x -> HTTPHeader' <$> (x .:? "Value") <*> (x .:? "Name"))

instance Hashable HTTPHeader

instance NFData HTTPHeader
