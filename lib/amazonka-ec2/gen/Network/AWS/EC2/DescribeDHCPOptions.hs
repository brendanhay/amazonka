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
-- Module      : Network.AWS.EC2.DescribeDHCPOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your DHCP options sets.
--
--
-- For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_DHCP_Options.html DHCP Options Sets> in the /Amazon Virtual Private Cloud User Guide/ .
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeDHCPOptions
  ( -- * Creating a Request
    describeDHCPOptions,
    DescribeDHCPOptions,

    -- * Request Lenses
    ddoFilters,
    ddoDHCPOptionsIds,
    ddoNextToken,
    ddoDryRun,
    ddoMaxResults,

    -- * Destructuring the Response
    describeDHCPOptionsResponse,
    DescribeDHCPOptionsResponse,

    -- * Response Lenses
    ddorsDHCPOptions,
    ddorsNextToken,
    ddorsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeDHCPOptions' smart constructor.
data DescribeDHCPOptions = DescribeDHCPOptions'
  { _ddoFilters ::
      !(Maybe [Filter]),
    _ddoDHCPOptionsIds :: !(Maybe [Text]),
    _ddoNextToken :: !(Maybe Text),
    _ddoDryRun :: !(Maybe Bool),
    _ddoMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeDHCPOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddoFilters' - One or more filters.     * @dhcp-options-id@ - The ID of a DHCP options set.     * @key@ - The key for one of the options (for example, @domain-name@ ).     * @value@ - The value for one of the options.     * @owner-id@ - The ID of the AWS account that owns the DHCP options set.     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
-- * 'ddoDHCPOptionsIds' - The IDs of one or more DHCP options sets. Default: Describes all your DHCP options sets.
--
-- * 'ddoNextToken' - The token for the next page of results.
--
-- * 'ddoDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'ddoMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
describeDHCPOptions ::
  DescribeDHCPOptions
describeDHCPOptions =
  DescribeDHCPOptions'
    { _ddoFilters = Nothing,
      _ddoDHCPOptionsIds = Nothing,
      _ddoNextToken = Nothing,
      _ddoDryRun = Nothing,
      _ddoMaxResults = Nothing
    }

-- | One or more filters.     * @dhcp-options-id@ - The ID of a DHCP options set.     * @key@ - The key for one of the options (for example, @domain-name@ ).     * @value@ - The value for one of the options.     * @owner-id@ - The ID of the AWS account that owns the DHCP options set.     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
ddoFilters :: Lens' DescribeDHCPOptions [Filter]
ddoFilters = lens _ddoFilters (\s a -> s {_ddoFilters = a}) . _Default . _Coerce

-- | The IDs of one or more DHCP options sets. Default: Describes all your DHCP options sets.
ddoDHCPOptionsIds :: Lens' DescribeDHCPOptions [Text]
ddoDHCPOptionsIds = lens _ddoDHCPOptionsIds (\s a -> s {_ddoDHCPOptionsIds = a}) . _Default . _Coerce

-- | The token for the next page of results.
ddoNextToken :: Lens' DescribeDHCPOptions (Maybe Text)
ddoNextToken = lens _ddoNextToken (\s a -> s {_ddoNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
ddoDryRun :: Lens' DescribeDHCPOptions (Maybe Bool)
ddoDryRun = lens _ddoDryRun (\s a -> s {_ddoDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
ddoMaxResults :: Lens' DescribeDHCPOptions (Maybe Natural)
ddoMaxResults = lens _ddoMaxResults (\s a -> s {_ddoMaxResults = a}) . mapping _Nat

instance AWSPager DescribeDHCPOptions where
  page rq rs
    | stop (rs ^. ddorsNextToken) = Nothing
    | stop (rs ^. ddorsDHCPOptions) = Nothing
    | otherwise = Just $ rq & ddoNextToken .~ rs ^. ddorsNextToken

instance AWSRequest DescribeDHCPOptions where
  type Rs DescribeDHCPOptions = DescribeDHCPOptionsResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeDHCPOptionsResponse'
            <$> (x .@? "dhcpOptionsSet" .!@ mempty >>= may (parseXMLList "item"))
            <*> (x .@? "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeDHCPOptions

instance NFData DescribeDHCPOptions

instance ToHeaders DescribeDHCPOptions where
  toHeaders = const mempty

instance ToPath DescribeDHCPOptions where
  toPath = const "/"

instance ToQuery DescribeDHCPOptions where
  toQuery DescribeDHCPOptions' {..} =
    mconcat
      [ "Action" =: ("DescribeDhcpOptions" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "Filter" <$> _ddoFilters),
        toQuery (toQueryList "DhcpOptionsId" <$> _ddoDHCPOptionsIds),
        "NextToken" =: _ddoNextToken,
        "DryRun" =: _ddoDryRun,
        "MaxResults" =: _ddoMaxResults
      ]

-- | /See:/ 'describeDHCPOptionsResponse' smart constructor.
data DescribeDHCPOptionsResponse = DescribeDHCPOptionsResponse'
  { _ddorsDHCPOptions ::
      !(Maybe [DHCPOptions]),
    _ddorsNextToken :: !(Maybe Text),
    _ddorsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeDHCPOptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddorsDHCPOptions' - Information about one or more DHCP options sets.
--
-- * 'ddorsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'ddorsResponseStatus' - -- | The response status code.
describeDHCPOptionsResponse ::
  -- | 'ddorsResponseStatus'
  Int ->
  DescribeDHCPOptionsResponse
describeDHCPOptionsResponse pResponseStatus_ =
  DescribeDHCPOptionsResponse'
    { _ddorsDHCPOptions = Nothing,
      _ddorsNextToken = Nothing,
      _ddorsResponseStatus = pResponseStatus_
    }

-- | Information about one or more DHCP options sets.
ddorsDHCPOptions :: Lens' DescribeDHCPOptionsResponse [DHCPOptions]
ddorsDHCPOptions = lens _ddorsDHCPOptions (\s a -> s {_ddorsDHCPOptions = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
ddorsNextToken :: Lens' DescribeDHCPOptionsResponse (Maybe Text)
ddorsNextToken = lens _ddorsNextToken (\s a -> s {_ddorsNextToken = a})

-- | -- | The response status code.
ddorsResponseStatus :: Lens' DescribeDHCPOptionsResponse Int
ddorsResponseStatus = lens _ddorsResponseStatus (\s a -> s {_ddorsResponseStatus = a})

instance NFData DescribeDHCPOptionsResponse
