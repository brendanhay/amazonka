{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.HTTPContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.HTTPContext where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the HTTP context to use for the test authorizer request.
--
--
--
-- /See:/ 'hTTPContext' smart constructor.
data HTTPContext = HTTPContext'
  { _httpcHeaders ::
      !(Maybe (Map Text (Text))),
    _httpcQueryString :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HTTPContext' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httpcHeaders' - The header keys and values in an HTTP authorization request.
--
-- * 'httpcQueryString' - The query string keys and values in an HTTP authorization request.
hTTPContext ::
  HTTPContext
hTTPContext =
  HTTPContext'
    { _httpcHeaders = Nothing,
      _httpcQueryString = Nothing
    }

-- | The header keys and values in an HTTP authorization request.
httpcHeaders :: Lens' HTTPContext (HashMap Text (Text))
httpcHeaders = lens _httpcHeaders (\s a -> s {_httpcHeaders = a}) . _Default . _Map

-- | The query string keys and values in an HTTP authorization request.
httpcQueryString :: Lens' HTTPContext (Maybe Text)
httpcQueryString = lens _httpcQueryString (\s a -> s {_httpcQueryString = a})

instance Hashable HTTPContext

instance NFData HTTPContext

instance ToJSON HTTPContext where
  toJSON HTTPContext' {..} =
    object
      ( catMaybes
          [ ("headers" .=) <$> _httpcHeaders,
            ("queryString" .=) <$> _httpcQueryString
          ]
      )
