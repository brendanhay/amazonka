{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ProvisionByoipCidr
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions an IPv4 or IPv6 address range for use with your AWS resources through bring your own IP addresses (BYOIP) and creates a corresponding address pool. After the address range is provisioned, it is ready to be advertised using 'AdvertiseByoipCidr' .
--
--
-- AWS verifies that you own the address range and are authorized to advertise it. You must ensure that the address range is registered to you and that you created an RPKI ROA to authorize Amazon ASNs 16509 and 14618 to advertise the address range. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-byoip.html Bring Your Own IP Addresses (BYOIP)> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- Provisioning an address range is an asynchronous operation, so the call returns immediately, but the address range is not ready to use until its status changes from @pending-provision@ to @provisioned@ . To monitor the status of an address range, use 'DescribeByoipCidrs' . To allocate an Elastic IP address from your IPv4 address pool, use 'AllocateAddress' with either the specific address from the address pool or the ID of the address pool.
module Network.AWS.EC2.ProvisionByoipCidr
  ( -- * Creating a Request
    provisionByoipCidr,
    ProvisionByoipCidr,

    -- * Request Lenses
    pbcCidrAuthorizationContext,
    pbcPoolTagSpecifications,
    pbcPubliclyAdvertisable,
    pbcDescription,
    pbcDryRun,
    pbcCidr,

    -- * Destructuring the Response
    provisionByoipCidrResponse,
    ProvisionByoipCidrResponse,

    -- * Response Lenses
    pbcrsByoipCidr,
    pbcrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'provisionByoipCidr' smart constructor.
data ProvisionByoipCidr = ProvisionByoipCidr'
  { _pbcCidrAuthorizationContext ::
      !(Maybe CidrAuthorizationContext),
    _pbcPoolTagSpecifications ::
      !(Maybe [TagSpecification]),
    _pbcPubliclyAdvertisable :: !(Maybe Bool),
    _pbcDescription :: !(Maybe Text),
    _pbcDryRun :: !(Maybe Bool),
    _pbcCidr :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProvisionByoipCidr' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbcCidrAuthorizationContext' - A signed document that proves that you are authorized to bring the specified IP address range to Amazon using BYOIP.
--
-- * 'pbcPoolTagSpecifications' - The tags to apply to the address pool.
--
-- * 'pbcPubliclyAdvertisable' - (IPv6 only) Indicate whether the address range will be publicly advertised to the internet. Default: true
--
-- * 'pbcDescription' - A description for the address range and the address pool.
--
-- * 'pbcDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'pbcCidr' - The public IPv4 or IPv6 address range, in CIDR notation. The most specific IPv4 prefix that you can specify is /24. The most specific IPv6 prefix you can specify is /56. The address range cannot overlap with another address range that you've brought to this or another Region.
provisionByoipCidr ::
  -- | 'pbcCidr'
  Text ->
  ProvisionByoipCidr
provisionByoipCidr pCidr_ =
  ProvisionByoipCidr'
    { _pbcCidrAuthorizationContext = Nothing,
      _pbcPoolTagSpecifications = Nothing,
      _pbcPubliclyAdvertisable = Nothing,
      _pbcDescription = Nothing,
      _pbcDryRun = Nothing,
      _pbcCidr = pCidr_
    }

-- | A signed document that proves that you are authorized to bring the specified IP address range to Amazon using BYOIP.
pbcCidrAuthorizationContext :: Lens' ProvisionByoipCidr (Maybe CidrAuthorizationContext)
pbcCidrAuthorizationContext = lens _pbcCidrAuthorizationContext (\s a -> s {_pbcCidrAuthorizationContext = a})

-- | The tags to apply to the address pool.
pbcPoolTagSpecifications :: Lens' ProvisionByoipCidr [TagSpecification]
pbcPoolTagSpecifications = lens _pbcPoolTagSpecifications (\s a -> s {_pbcPoolTagSpecifications = a}) . _Default . _Coerce

-- | (IPv6 only) Indicate whether the address range will be publicly advertised to the internet. Default: true
pbcPubliclyAdvertisable :: Lens' ProvisionByoipCidr (Maybe Bool)
pbcPubliclyAdvertisable = lens _pbcPubliclyAdvertisable (\s a -> s {_pbcPubliclyAdvertisable = a})

-- | A description for the address range and the address pool.
pbcDescription :: Lens' ProvisionByoipCidr (Maybe Text)
pbcDescription = lens _pbcDescription (\s a -> s {_pbcDescription = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
pbcDryRun :: Lens' ProvisionByoipCidr (Maybe Bool)
pbcDryRun = lens _pbcDryRun (\s a -> s {_pbcDryRun = a})

-- | The public IPv4 or IPv6 address range, in CIDR notation. The most specific IPv4 prefix that you can specify is /24. The most specific IPv6 prefix you can specify is /56. The address range cannot overlap with another address range that you've brought to this or another Region.
pbcCidr :: Lens' ProvisionByoipCidr Text
pbcCidr = lens _pbcCidr (\s a -> s {_pbcCidr = a})

instance AWSRequest ProvisionByoipCidr where
  type Rs ProvisionByoipCidr = ProvisionByoipCidrResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          ProvisionByoipCidrResponse'
            <$> (x .@? "byoipCidr") <*> (pure (fromEnum s))
      )

instance Hashable ProvisionByoipCidr

instance NFData ProvisionByoipCidr

instance ToHeaders ProvisionByoipCidr where
  toHeaders = const mempty

instance ToPath ProvisionByoipCidr where
  toPath = const "/"

instance ToQuery ProvisionByoipCidr where
  toQuery ProvisionByoipCidr' {..} =
    mconcat
      [ "Action" =: ("ProvisionByoipCidr" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "CidrAuthorizationContext" =: _pbcCidrAuthorizationContext,
        toQuery
          (toQueryList "PoolTagSpecification" <$> _pbcPoolTagSpecifications),
        "PubliclyAdvertisable" =: _pbcPubliclyAdvertisable,
        "Description" =: _pbcDescription,
        "DryRun" =: _pbcDryRun,
        "Cidr" =: _pbcCidr
      ]

-- | /See:/ 'provisionByoipCidrResponse' smart constructor.
data ProvisionByoipCidrResponse = ProvisionByoipCidrResponse'
  { _pbcrsByoipCidr ::
      !(Maybe ByoipCidr),
    _pbcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProvisionByoipCidrResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbcrsByoipCidr' - Information about the address range.
--
-- * 'pbcrsResponseStatus' - -- | The response status code.
provisionByoipCidrResponse ::
  -- | 'pbcrsResponseStatus'
  Int ->
  ProvisionByoipCidrResponse
provisionByoipCidrResponse pResponseStatus_ =
  ProvisionByoipCidrResponse'
    { _pbcrsByoipCidr = Nothing,
      _pbcrsResponseStatus = pResponseStatus_
    }

-- | Information about the address range.
pbcrsByoipCidr :: Lens' ProvisionByoipCidrResponse (Maybe ByoipCidr)
pbcrsByoipCidr = lens _pbcrsByoipCidr (\s a -> s {_pbcrsByoipCidr = a})

-- | -- | The response status code.
pbcrsResponseStatus :: Lens' ProvisionByoipCidrResponse Int
pbcrsResponseStatus = lens _pbcrsResponseStatus (\s a -> s {_pbcrsResponseStatus = a})

instance NFData ProvisionByoipCidrResponse
