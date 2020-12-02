{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.HTTPHeader
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.HTTPHeader where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The HTTP header.
--
--
--
-- /See:/ 'hTTPHeader' smart constructor.
data HTTPHeader = HTTPHeader'
  { _httphHeaderValue :: !(Maybe Text),
    _httphHeaderName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HTTPHeader' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httphHeaderValue' - The header value.
--
-- * 'httphHeaderName' - The header name
hTTPHeader ::
  HTTPHeader
hTTPHeader =
  HTTPHeader'
    { _httphHeaderValue = Nothing,
      _httphHeaderName = Nothing
    }

-- | The header value.
httphHeaderValue :: Lens' HTTPHeader (Maybe Text)
httphHeaderValue = lens _httphHeaderValue (\s a -> s {_httphHeaderValue = a})

-- | The header name
httphHeaderName :: Lens' HTTPHeader (Maybe Text)
httphHeaderName = lens _httphHeaderName (\s a -> s {_httphHeaderName = a})

instance Hashable HTTPHeader

instance NFData HTTPHeader

instance ToJSON HTTPHeader where
  toJSON HTTPHeader' {..} =
    object
      ( catMaybes
          [ ("headerValue" .=) <$> _httphHeaderValue,
            ("headerName" .=) <$> _httphHeaderName
          ]
      )
