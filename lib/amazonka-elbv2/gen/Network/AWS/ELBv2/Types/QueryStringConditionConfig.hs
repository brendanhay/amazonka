{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.QueryStringConditionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.QueryStringConditionConfig where

import Network.AWS.ELBv2.Types.QueryStringKeyValuePair
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a query string condition.
--
--
-- The query string component of a URI starts after the first '?' character and is terminated by either a '#' character or the end of the URI. A typical query string contains key/value pairs separated by '&' characters. The allowed characters are specified by RFC 3986. Any character can be percentage encoded.
--
--
-- /See:/ 'queryStringConditionConfig' smart constructor.
newtype QueryStringConditionConfig = QueryStringConditionConfig'
  { _qsccValues ::
      Maybe [QueryStringKeyValuePair]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'QueryStringConditionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qsccValues' - One or more key/value pairs or values to find in the query string. The maximum size of each string is 128 characters. The comparison is case insensitive. The following wildcard characters are supported: * (matches 0 or more characters) and ? (matches exactly 1 character). To search for a literal '*' or '?' character in a query string, you must escape these characters in @Values@ using a '\' character. If you specify multiple key/value pairs or values, the condition is satisfied if one of them is found in the query string.
queryStringConditionConfig ::
  QueryStringConditionConfig
queryStringConditionConfig =
  QueryStringConditionConfig' {_qsccValues = Nothing}

-- | One or more key/value pairs or values to find in the query string. The maximum size of each string is 128 characters. The comparison is case insensitive. The following wildcard characters are supported: * (matches 0 or more characters) and ? (matches exactly 1 character). To search for a literal '*' or '?' character in a query string, you must escape these characters in @Values@ using a '\' character. If you specify multiple key/value pairs or values, the condition is satisfied if one of them is found in the query string.
qsccValues :: Lens' QueryStringConditionConfig [QueryStringKeyValuePair]
qsccValues = lens _qsccValues (\s a -> s {_qsccValues = a}) . _Default . _Coerce

instance FromXML QueryStringConditionConfig where
  parseXML x =
    QueryStringConditionConfig'
      <$> (x .@? "Values" .!@ mempty >>= may (parseXMLList "member"))

instance Hashable QueryStringConditionConfig

instance NFData QueryStringConditionConfig

instance ToQuery QueryStringConditionConfig where
  toQuery QueryStringConditionConfig' {..} =
    mconcat
      ["Values" =: toQuery (toQueryList "member" <$> _qsccValues)]
