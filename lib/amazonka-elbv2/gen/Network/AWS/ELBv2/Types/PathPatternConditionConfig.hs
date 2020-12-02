{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.PathPatternConditionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.PathPatternConditionConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a path pattern condition.
--
--
--
-- /See:/ 'pathPatternConditionConfig' smart constructor.
newtype PathPatternConditionConfig = PathPatternConditionConfig'
  { _ppccValues ::
      Maybe [Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PathPatternConditionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppccValues' - One or more path patterns to compare against the request URL. The maximum size of each string is 128 characters. The comparison is case sensitive. The following wildcard characters are supported: * (matches 0 or more characters) and ? (matches exactly 1 character). If you specify multiple strings, the condition is satisfied if one of them matches the request URL. The path pattern is compared only to the path of the URL, not to its query string. To compare against the query string, use 'QueryStringConditionConfig' .
pathPatternConditionConfig ::
  PathPatternConditionConfig
pathPatternConditionConfig =
  PathPatternConditionConfig' {_ppccValues = Nothing}

-- | One or more path patterns to compare against the request URL. The maximum size of each string is 128 characters. The comparison is case sensitive. The following wildcard characters are supported: * (matches 0 or more characters) and ? (matches exactly 1 character). If you specify multiple strings, the condition is satisfied if one of them matches the request URL. The path pattern is compared only to the path of the URL, not to its query string. To compare against the query string, use 'QueryStringConditionConfig' .
ppccValues :: Lens' PathPatternConditionConfig [Text]
ppccValues = lens _ppccValues (\s a -> s {_ppccValues = a}) . _Default . _Coerce

instance FromXML PathPatternConditionConfig where
  parseXML x =
    PathPatternConditionConfig'
      <$> (x .@? "Values" .!@ mempty >>= may (parseXMLList "member"))

instance Hashable PathPatternConditionConfig

instance NFData PathPatternConditionConfig

instance ToQuery PathPatternConditionConfig where
  toQuery PathPatternConditionConfig' {..} =
    mconcat
      ["Values" =: toQuery (toQueryList "member" <$> _ppccValues)]
