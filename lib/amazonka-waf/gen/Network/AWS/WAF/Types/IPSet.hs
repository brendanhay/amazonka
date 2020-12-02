{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.IPSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.IPSet where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAF.Types.IPSetDescriptor

-- | Contains one or more IP addresses or blocks of IP addresses specified in Classless Inter-Domain Routing (CIDR) notation. AWS WAF supports IPv4 address ranges: /8 and any range between /16 through /32. AWS WAF supports IPv6 address ranges: /24, /32, /48, /56, /64, and /128.
--
--
-- To specify an individual IP address, you specify the four-part IP address followed by a @/32@ , for example, 192.0.2.0/32. To block a range of IP addresses, you can specify /8 or any range between /16 through /32 (for IPv4) or /24, /32, /48, /56, /64, or /128 (for IPv6). For more information about CIDR notation, see the Wikipedia entry <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing> .
--
--
-- /See:/ 'ipSet' smart constructor.
data IPSet = IPSet'
  { _isName :: !(Maybe Text),
    _isIPSetId :: !Text,
    _isIPSetDescriptors :: ![IPSetDescriptor]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IPSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isName' - A friendly name or description of the 'IPSet' . You can't change the name of an @IPSet@ after you create it.
--
-- * 'isIPSetId' - The @IPSetId@ for an @IPSet@ . You use @IPSetId@ to get information about an @IPSet@ (see 'GetIPSet' ), update an @IPSet@ (see 'UpdateIPSet' ), insert an @IPSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete an @IPSet@ from AWS WAF (see 'DeleteIPSet' ). @IPSetId@ is returned by 'CreateIPSet' and by 'ListIPSets' .
--
-- * 'isIPSetDescriptors' - The IP address type (@IPV4@ or @IPV6@ ) and the IP address range (in CIDR notation) that web requests originate from. If the @WebACL@ is associated with a CloudFront distribution and the viewer did not use an HTTP proxy or a load balancer to send the request, this is the value of the c-ip field in the CloudFront access logs.
ipSet ::
  -- | 'isIPSetId'
  Text ->
  IPSet
ipSet pIPSetId_ =
  IPSet'
    { _isName = Nothing,
      _isIPSetId = pIPSetId_,
      _isIPSetDescriptors = mempty
    }

-- | A friendly name or description of the 'IPSet' . You can't change the name of an @IPSet@ after you create it.
isName :: Lens' IPSet (Maybe Text)
isName = lens _isName (\s a -> s {_isName = a})

-- | The @IPSetId@ for an @IPSet@ . You use @IPSetId@ to get information about an @IPSet@ (see 'GetIPSet' ), update an @IPSet@ (see 'UpdateIPSet' ), insert an @IPSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete an @IPSet@ from AWS WAF (see 'DeleteIPSet' ). @IPSetId@ is returned by 'CreateIPSet' and by 'ListIPSets' .
isIPSetId :: Lens' IPSet Text
isIPSetId = lens _isIPSetId (\s a -> s {_isIPSetId = a})

-- | The IP address type (@IPV4@ or @IPV6@ ) and the IP address range (in CIDR notation) that web requests originate from. If the @WebACL@ is associated with a CloudFront distribution and the viewer did not use an HTTP proxy or a load balancer to send the request, this is the value of the c-ip field in the CloudFront access logs.
isIPSetDescriptors :: Lens' IPSet [IPSetDescriptor]
isIPSetDescriptors = lens _isIPSetDescriptors (\s a -> s {_isIPSetDescriptors = a}) . _Coerce

instance FromJSON IPSet where
  parseJSON =
    withObject
      "IPSet"
      ( \x ->
          IPSet'
            <$> (x .:? "Name")
            <*> (x .: "IPSetId")
            <*> (x .:? "IPSetDescriptors" .!= mempty)
      )

instance Hashable IPSet

instance NFData IPSet
