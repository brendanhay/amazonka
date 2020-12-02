{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.IPSetDescriptor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.IPSetDescriptor where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAF.Types.IPSetDescriptorType

-- | Specifies the IP address type (@IPV4@ or @IPV6@ ) and the IP address range (in CIDR format) that web requests originate from.
--
--
--
-- /See:/ 'ipSetDescriptor' smart constructor.
data IPSetDescriptor = IPSetDescriptor'
  { _isdType ::
      !IPSetDescriptorType,
    _isdValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IPSetDescriptor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isdType' - Specify @IPV4@ or @IPV6@ .
--
-- * 'isdValue' - Specify an IPv4 address by using CIDR notation. For example:     * To configure AWS WAF to allow, block, or count requests that originated from the IP address 192.0.2.44, specify @192.0.2.44/32@ .     * To configure AWS WAF to allow, block, or count requests that originated from IP addresses from 192.0.2.0 to 192.0.2.255, specify @192.0.2.0/24@ . For more information about CIDR notation, see the Wikipedia entry <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing> . Specify an IPv6 address by using CIDR notation. For example:     * To configure AWS WAF to allow, block, or count requests that originated from the IP address 1111:0000:0000:0000:0000:0000:0000:0111, specify @1111:0000:0000:0000:0000:0000:0000:0111/128@ .     * To configure AWS WAF to allow, block, or count requests that originated from IP addresses 1111:0000:0000:0000:0000:0000:0000:0000 to 1111:0000:0000:0000:ffff:ffff:ffff:ffff, specify @1111:0000:0000:0000:0000:0000:0000:0000/64@ .
ipSetDescriptor ::
  -- | 'isdType'
  IPSetDescriptorType ->
  -- | 'isdValue'
  Text ->
  IPSetDescriptor
ipSetDescriptor pType_ pValue_ =
  IPSetDescriptor' {_isdType = pType_, _isdValue = pValue_}

-- | Specify @IPV4@ or @IPV6@ .
isdType :: Lens' IPSetDescriptor IPSetDescriptorType
isdType = lens _isdType (\s a -> s {_isdType = a})

-- | Specify an IPv4 address by using CIDR notation. For example:     * To configure AWS WAF to allow, block, or count requests that originated from the IP address 192.0.2.44, specify @192.0.2.44/32@ .     * To configure AWS WAF to allow, block, or count requests that originated from IP addresses from 192.0.2.0 to 192.0.2.255, specify @192.0.2.0/24@ . For more information about CIDR notation, see the Wikipedia entry <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing> . Specify an IPv6 address by using CIDR notation. For example:     * To configure AWS WAF to allow, block, or count requests that originated from the IP address 1111:0000:0000:0000:0000:0000:0000:0111, specify @1111:0000:0000:0000:0000:0000:0000:0111/128@ .     * To configure AWS WAF to allow, block, or count requests that originated from IP addresses 1111:0000:0000:0000:0000:0000:0000:0000 to 1111:0000:0000:0000:ffff:ffff:ffff:ffff, specify @1111:0000:0000:0000:0000:0000:0000:0000/64@ .
isdValue :: Lens' IPSetDescriptor Text
isdValue = lens _isdValue (\s a -> s {_isdValue = a})

instance FromJSON IPSetDescriptor where
  parseJSON =
    withObject
      "IPSetDescriptor"
      (\x -> IPSetDescriptor' <$> (x .: "Type") <*> (x .: "Value"))

instance Hashable IPSetDescriptor

instance NFData IPSetDescriptor

instance ToJSON IPSetDescriptor where
  toJSON IPSetDescriptor' {..} =
    object
      ( catMaybes
          [Just ("Type" .= _isdType), Just ("Value" .= _isdValue)]
      )
