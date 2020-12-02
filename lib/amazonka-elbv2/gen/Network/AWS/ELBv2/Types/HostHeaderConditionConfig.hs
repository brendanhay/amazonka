{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.HostHeaderConditionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.HostHeaderConditionConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a host header condition.
--
--
--
-- /See:/ 'hostHeaderConditionConfig' smart constructor.
newtype HostHeaderConditionConfig = HostHeaderConditionConfig'
  { _hhccValues ::
      Maybe [Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HostHeaderConditionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hhccValues' - One or more host names. The maximum size of each name is 128 characters. The comparison is case insensitive. The following wildcard characters are supported: * (matches 0 or more characters) and ? (matches exactly 1 character). If you specify multiple strings, the condition is satisfied if one of the strings matches the host name.
hostHeaderConditionConfig ::
  HostHeaderConditionConfig
hostHeaderConditionConfig =
  HostHeaderConditionConfig' {_hhccValues = Nothing}

-- | One or more host names. The maximum size of each name is 128 characters. The comparison is case insensitive. The following wildcard characters are supported: * (matches 0 or more characters) and ? (matches exactly 1 character). If you specify multiple strings, the condition is satisfied if one of the strings matches the host name.
hhccValues :: Lens' HostHeaderConditionConfig [Text]
hhccValues = lens _hhccValues (\s a -> s {_hhccValues = a}) . _Default . _Coerce

instance FromXML HostHeaderConditionConfig where
  parseXML x =
    HostHeaderConditionConfig'
      <$> (x .@? "Values" .!@ mempty >>= may (parseXMLList "member"))

instance Hashable HostHeaderConditionConfig

instance NFData HostHeaderConditionConfig

instance ToQuery HostHeaderConditionConfig where
  toQuery HostHeaderConditionConfig' {..} =
    mconcat
      ["Values" =: toQuery (toQueryList "member" <$> _hhccValues)]
