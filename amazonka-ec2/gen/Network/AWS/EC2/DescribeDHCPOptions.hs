{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeDHCPOptions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your DHCP options sets.
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_DHCP_Options.html DHCP Options Sets> in the /Amazon Virtual Private Cloud User Guide/ .
--
module Network.AWS.EC2.DescribeDHCPOptions
    (
    -- * Creating a Request
      describeDHCPOptions
    , DescribeDHCPOptions
    -- * Request Lenses
    , ddoFilters
    , ddoDHCPOptionsIds
    , ddoDryRun

    -- * Destructuring the Response
    , describeDHCPOptionsResponse
    , DescribeDHCPOptionsResponse
    -- * Response Lenses
    , ddorsDHCPOptions
    , ddorsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeDHCPOptions' smart constructor.
data DescribeDHCPOptions = DescribeDHCPOptions'
  { _ddoFilters        :: !(Maybe [Filter])
  , _ddoDHCPOptionsIds :: !(Maybe [Text])
  , _ddoDryRun         :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDHCPOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddoFilters' - One or more filters.     * @dhcp-options-id@ - The ID of a DHCP options set.     * @key@ - The key for one of the options (for example, @domain-name@ ).     * @value@ - The value for one of the options.     * @owner-id@ - The ID of the AWS account that owns the DHCP options set.     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
-- * 'ddoDHCPOptionsIds' - The IDs of one or more DHCP options sets. Default: Describes all your DHCP options sets.
--
-- * 'ddoDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
describeDHCPOptions
    :: DescribeDHCPOptions
describeDHCPOptions =
  DescribeDHCPOptions'
    {_ddoFilters = Nothing, _ddoDHCPOptionsIds = Nothing, _ddoDryRun = Nothing}


-- | One or more filters.     * @dhcp-options-id@ - The ID of a DHCP options set.     * @key@ - The key for one of the options (for example, @domain-name@ ).     * @value@ - The value for one of the options.     * @owner-id@ - The ID of the AWS account that owns the DHCP options set.     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
ddoFilters :: Lens' DescribeDHCPOptions [Filter]
ddoFilters = lens _ddoFilters (\ s a -> s{_ddoFilters = a}) . _Default . _Coerce

-- | The IDs of one or more DHCP options sets. Default: Describes all your DHCP options sets.
ddoDHCPOptionsIds :: Lens' DescribeDHCPOptions [Text]
ddoDHCPOptionsIds = lens _ddoDHCPOptionsIds (\ s a -> s{_ddoDHCPOptionsIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
ddoDryRun :: Lens' DescribeDHCPOptions (Maybe Bool)
ddoDryRun = lens _ddoDryRun (\ s a -> s{_ddoDryRun = a})

instance AWSRequest DescribeDHCPOptions where
        type Rs DescribeDHCPOptions =
             DescribeDHCPOptionsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeDHCPOptionsResponse' <$>
                   (x .@? "dhcpOptionsSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeDHCPOptions where

instance NFData DescribeDHCPOptions where

instance ToHeaders DescribeDHCPOptions where
        toHeaders = const mempty

instance ToPath DescribeDHCPOptions where
        toPath = const "/"

instance ToQuery DescribeDHCPOptions where
        toQuery DescribeDHCPOptions'{..}
          = mconcat
              ["Action" =: ("DescribeDhcpOptions" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _ddoFilters),
               toQuery
                 (toQueryList "DhcpOptionsId" <$> _ddoDHCPOptionsIds),
               "DryRun" =: _ddoDryRun]

-- | /See:/ 'describeDHCPOptionsResponse' smart constructor.
data DescribeDHCPOptionsResponse = DescribeDHCPOptionsResponse'
  { _ddorsDHCPOptions    :: !(Maybe [DHCPOptions])
  , _ddorsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDHCPOptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddorsDHCPOptions' - Information about one or more DHCP options sets.
--
-- * 'ddorsResponseStatus' - -- | The response status code.
describeDHCPOptionsResponse
    :: Int -- ^ 'ddorsResponseStatus'
    -> DescribeDHCPOptionsResponse
describeDHCPOptionsResponse pResponseStatus_ =
  DescribeDHCPOptionsResponse'
    {_ddorsDHCPOptions = Nothing, _ddorsResponseStatus = pResponseStatus_}


-- | Information about one or more DHCP options sets.
ddorsDHCPOptions :: Lens' DescribeDHCPOptionsResponse [DHCPOptions]
ddorsDHCPOptions = lens _ddorsDHCPOptions (\ s a -> s{_ddorsDHCPOptions = a}) . _Default . _Coerce

-- | -- | The response status code.
ddorsResponseStatus :: Lens' DescribeDHCPOptionsResponse Int
ddorsResponseStatus = lens _ddorsResponseStatus (\ s a -> s{_ddorsResponseStatus = a})

instance NFData DescribeDHCPOptionsResponse where
