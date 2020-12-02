{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.HTTPHeaderConditionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.HTTPHeaderConditionConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an HTTP header condition.
--
--
-- There is a set of standard HTTP header fields. You can also define custom HTTP header fields.
--
--
-- /See:/ 'hTTPHeaderConditionConfig' smart constructor.
data HTTPHeaderConditionConfig = HTTPHeaderConditionConfig'
  { _httphccValues ::
      !(Maybe [Text]),
    _httphccHTTPHeaderName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HTTPHeaderConditionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httphccValues' - One or more strings to compare against the value of the HTTP header. The maximum size of each string is 128 characters. The comparison strings are case insensitive. The following wildcard characters are supported: * (matches 0 or more characters) and ? (matches exactly 1 character). If the same header appears multiple times in the request, we search them in order until a match is found. If you specify multiple strings, the condition is satisfied if one of the strings matches the value of the HTTP header. To require that all of the strings are a match, create one condition per string.
--
-- * 'httphccHTTPHeaderName' - The name of the HTTP header field. The maximum size is 40 characters. The header name is case insensitive. The allowed characters are specified by RFC 7230. Wildcards are not supported. You can't use an HTTP header condition to specify the host header. Use 'HostHeaderConditionConfig' to specify a host header condition.
hTTPHeaderConditionConfig ::
  HTTPHeaderConditionConfig
hTTPHeaderConditionConfig =
  HTTPHeaderConditionConfig'
    { _httphccValues = Nothing,
      _httphccHTTPHeaderName = Nothing
    }

-- | One or more strings to compare against the value of the HTTP header. The maximum size of each string is 128 characters. The comparison strings are case insensitive. The following wildcard characters are supported: * (matches 0 or more characters) and ? (matches exactly 1 character). If the same header appears multiple times in the request, we search them in order until a match is found. If you specify multiple strings, the condition is satisfied if one of the strings matches the value of the HTTP header. To require that all of the strings are a match, create one condition per string.
httphccValues :: Lens' HTTPHeaderConditionConfig [Text]
httphccValues = lens _httphccValues (\s a -> s {_httphccValues = a}) . _Default . _Coerce

-- | The name of the HTTP header field. The maximum size is 40 characters. The header name is case insensitive. The allowed characters are specified by RFC 7230. Wildcards are not supported. You can't use an HTTP header condition to specify the host header. Use 'HostHeaderConditionConfig' to specify a host header condition.
httphccHTTPHeaderName :: Lens' HTTPHeaderConditionConfig (Maybe Text)
httphccHTTPHeaderName = lens _httphccHTTPHeaderName (\s a -> s {_httphccHTTPHeaderName = a})

instance FromXML HTTPHeaderConditionConfig where
  parseXML x =
    HTTPHeaderConditionConfig'
      <$> (x .@? "Values" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "HttpHeaderName")

instance Hashable HTTPHeaderConditionConfig

instance NFData HTTPHeaderConditionConfig

instance ToQuery HTTPHeaderConditionConfig where
  toQuery HTTPHeaderConditionConfig' {..} =
    mconcat
      [ "Values" =: toQuery (toQueryList "member" <$> _httphccValues),
        "HttpHeaderName" =: _httphccHTTPHeaderName
      ]
