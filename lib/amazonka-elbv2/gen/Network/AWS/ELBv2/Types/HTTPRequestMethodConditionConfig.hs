{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.HTTPRequestMethodConditionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.HTTPRequestMethodConditionConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an HTTP method condition.
--
--
-- HTTP defines a set of request methods, also referred to as HTTP verbs. For more information, see the <https://www.iana.org/assignments/http-methods/http-methods.xhtml HTTP Method Registry> . You can also define custom HTTP methods.
--
--
-- /See:/ 'hTTPRequestMethodConditionConfig' smart constructor.
newtype HTTPRequestMethodConditionConfig = HTTPRequestMethodConditionConfig'
  { _httprmccValues ::
      Maybe [Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HTTPRequestMethodConditionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httprmccValues' - The name of the request method. The maximum size is 40 characters. The allowed characters are A-Z, hyphen (-), and underscore (_). The comparison is case sensitive. Wildcards are not supported; therefore, the method name must be an exact match. If you specify multiple strings, the condition is satisfied if one of the strings matches the HTTP request method. We recommend that you route GET and HEAD requests in the same way, because the response to a HEAD request may be cached.
hTTPRequestMethodConditionConfig ::
  HTTPRequestMethodConditionConfig
hTTPRequestMethodConditionConfig =
  HTTPRequestMethodConditionConfig' {_httprmccValues = Nothing}

-- | The name of the request method. The maximum size is 40 characters. The allowed characters are A-Z, hyphen (-), and underscore (_). The comparison is case sensitive. Wildcards are not supported; therefore, the method name must be an exact match. If you specify multiple strings, the condition is satisfied if one of the strings matches the HTTP request method. We recommend that you route GET and HEAD requests in the same way, because the response to a HEAD request may be cached.
httprmccValues :: Lens' HTTPRequestMethodConditionConfig [Text]
httprmccValues = lens _httprmccValues (\s a -> s {_httprmccValues = a}) . _Default . _Coerce

instance FromXML HTTPRequestMethodConditionConfig where
  parseXML x =
    HTTPRequestMethodConditionConfig'
      <$> (x .@? "Values" .!@ mempty >>= may (parseXMLList "member"))

instance Hashable HTTPRequestMethodConditionConfig

instance NFData HTTPRequestMethodConditionConfig

instance ToQuery HTTPRequestMethodConditionConfig where
  toQuery HTTPRequestMethodConditionConfig' {..} =
    mconcat
      ["Values" =: toQuery (toQueryList "member" <$> _httprmccValues)]
