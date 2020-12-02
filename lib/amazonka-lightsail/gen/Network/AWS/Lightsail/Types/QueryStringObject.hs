{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.QueryStringObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.QueryStringObject where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the query string parameters that an Amazon Lightsail content delivery network (CDN) distribution to bases caching on.
--
--
-- For the query strings that you specify, your distribution caches separate versions of the specified content based on the query string values in viewer requests.
--
--
-- /See:/ 'queryStringObject' smart constructor.
data QueryStringObject = QueryStringObject'
  { _qsoQueryStringsAllowList ::
      !(Maybe [Text]),
    _qsoOption :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'QueryStringObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qsoQueryStringsAllowList' - The specific query strings that the distribution forwards to the origin. Your distribution will cache content based on the specified query strings. If the @option@ parameter is true, then your distribution forwards all query strings, regardless of what you specify using the @queryStringsAllowList@ parameter.
--
-- * 'qsoOption' - Indicates whether the distribution forwards and caches based on query strings.
queryStringObject ::
  QueryStringObject
queryStringObject =
  QueryStringObject'
    { _qsoQueryStringsAllowList = Nothing,
      _qsoOption = Nothing
    }

-- | The specific query strings that the distribution forwards to the origin. Your distribution will cache content based on the specified query strings. If the @option@ parameter is true, then your distribution forwards all query strings, regardless of what you specify using the @queryStringsAllowList@ parameter.
qsoQueryStringsAllowList :: Lens' QueryStringObject [Text]
qsoQueryStringsAllowList = lens _qsoQueryStringsAllowList (\s a -> s {_qsoQueryStringsAllowList = a}) . _Default . _Coerce

-- | Indicates whether the distribution forwards and caches based on query strings.
qsoOption :: Lens' QueryStringObject (Maybe Bool)
qsoOption = lens _qsoOption (\s a -> s {_qsoOption = a})

instance FromJSON QueryStringObject where
  parseJSON =
    withObject
      "QueryStringObject"
      ( \x ->
          QueryStringObject'
            <$> (x .:? "queryStringsAllowList" .!= mempty) <*> (x .:? "option")
      )

instance Hashable QueryStringObject

instance NFData QueryStringObject

instance ToJSON QueryStringObject where
  toJSON QueryStringObject' {..} =
    object
      ( catMaybes
          [ ("queryStringsAllowList" .=) <$> _qsoQueryStringsAllowList,
            ("option" .=) <$> _qsoOption
          ]
      )
