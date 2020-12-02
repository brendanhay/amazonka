{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.SourceIPConditionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.SourceIPConditionConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a source IP condition.
--
--
-- You can use this condition to route based on the IP address of the source that connects to the load balancer. If a client is behind a proxy, this is the IP address of the proxy not the IP address of the client.
--
--
-- /See:/ 'sourceIPConditionConfig' smart constructor.
newtype SourceIPConditionConfig = SourceIPConditionConfig'
  { _siccValues ::
      Maybe [Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SourceIPConditionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siccValues' - One or more source IP addresses, in CIDR format. You can use both IPv4 and IPv6 addresses. Wildcards are not supported. If you specify multiple addresses, the condition is satisfied if the source IP address of the request matches one of the CIDR blocks. This condition is not satisfied by the addresses in the X-Forwarded-For header. To search for addresses in the X-Forwarded-For header, use 'HttpHeaderConditionConfig' .
sourceIPConditionConfig ::
  SourceIPConditionConfig
sourceIPConditionConfig =
  SourceIPConditionConfig' {_siccValues = Nothing}

-- | One or more source IP addresses, in CIDR format. You can use both IPv4 and IPv6 addresses. Wildcards are not supported. If you specify multiple addresses, the condition is satisfied if the source IP address of the request matches one of the CIDR blocks. This condition is not satisfied by the addresses in the X-Forwarded-For header. To search for addresses in the X-Forwarded-For header, use 'HttpHeaderConditionConfig' .
siccValues :: Lens' SourceIPConditionConfig [Text]
siccValues = lens _siccValues (\s a -> s {_siccValues = a}) . _Default . _Coerce

instance FromXML SourceIPConditionConfig where
  parseXML x =
    SourceIPConditionConfig'
      <$> (x .@? "Values" .!@ mempty >>= may (parseXMLList "member"))

instance Hashable SourceIPConditionConfig

instance NFData SourceIPConditionConfig

instance ToQuery SourceIPConditionConfig where
  toQuery SourceIPConditionConfig' {..} =
    mconcat
      ["Values" =: toQuery (toQueryList "member" <$> _siccValues)]
