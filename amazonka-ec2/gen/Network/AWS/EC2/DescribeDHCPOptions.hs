{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.DescribeDHCPOptions
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes one or more of your DHCP options sets.
--
-- For more information about DHCP options sets, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_DHCP_Options.html DHCP Options Sets>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeDHCPOptions.html>
module Network.AWS.EC2.DescribeDHCPOptions
    (
    -- * Request
      DescribeDHCPOptions
    -- ** Request constructor
    , describeDHCPOptions
    -- ** Request lenses
    , ddoFilters
    , ddoDHCPOptionsIds
    , ddoDryRun

    -- * Response
    , DescribeDHCPOptionsResponse
    -- ** Response constructor
    , describeDHCPOptionsResponse
    -- ** Response lenses
    , ddorDHCPOptions
    , ddorStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeDHCPOptions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddoFilters'
--
-- * 'ddoDHCPOptionsIds'
--
-- * 'ddoDryRun'
data DescribeDHCPOptions = DescribeDHCPOptions'
    { _ddoFilters        :: !(Maybe [Filter])
    , _ddoDHCPOptionsIds :: !(Maybe [Text])
    , _ddoDryRun         :: !(Maybe Bool)
    } deriving (Eq,Read,Show)

-- | 'DescribeDHCPOptions' smart constructor.
describeDHCPOptions :: DescribeDHCPOptions
describeDHCPOptions =
    DescribeDHCPOptions'
    { _ddoFilters = Nothing
    , _ddoDHCPOptionsIds = Nothing
    , _ddoDryRun = Nothing
    }

-- | One or more filters.
--
-- -   @dhcp-options-id@ - The ID of a set of DHCP options.
--
-- -   @key@ - The key for one of the options (for example, @domain-name@).
--
-- -   @value@ - The value for one of the options.
--
-- -   @tag@:/key/=/value/ - The key\/value combination of a tag assigned
--     to the resource.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. This filter
--     is independent of the @tag-value@ filter. For example, if you use
--     both the filter \"tag-key=Purpose\" and the filter \"tag-value=X\",
--     you get any resources assigned both the tag key Purpose (regardless
--     of what the tag\'s value is), and the tag value X (regardless of
--     what the tag\'s key is). If you want to list only resources where
--     Purpose is X, see the @tag@:/key/=/value/ filter.
--
-- -   @tag-value@ - The value of a tag assigned to the resource. This
--     filter is independent of the @tag-key@ filter.
--
ddoFilters :: Lens' DescribeDHCPOptions [Filter]
ddoFilters = lens _ddoFilters (\ s a -> s{_ddoFilters = a}) . _Default;

-- | The IDs of one or more DHCP options sets.
--
-- Default: Describes all your DHCP options sets.
ddoDHCPOptionsIds :: Lens' DescribeDHCPOptions [Text]
ddoDHCPOptionsIds = lens _ddoDHCPOptionsIds (\ s a -> s{_ddoDHCPOptionsIds = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
ddoDryRun :: Lens' DescribeDHCPOptions (Maybe Bool)
ddoDryRun = lens _ddoDryRun (\ s a -> s{_ddoDryRun = a});

instance AWSRequest DescribeDHCPOptions where
        type Sv DescribeDHCPOptions = EC2
        type Rs DescribeDHCPOptions =
             DescribeDHCPOptionsResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeDHCPOptionsResponse' <$>
                   (may (parseXMLList "item") x) <*> (pure s))

instance ToHeaders DescribeDHCPOptions where
        toHeaders = const mempty

instance ToPath DescribeDHCPOptions where
        toPath = const "/"

instance ToQuery DescribeDHCPOptions where
        toQuery DescribeDHCPOptions'{..}
          = mconcat
              ["Action" =: ("DescribeDHCPOptions" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _ddoFilters),
               toQuery
                 (toQueryList "DhcpOptionsId" <$> _ddoDHCPOptionsIds),
               "DryRun" =: _ddoDryRun]

-- | /See:/ 'describeDHCPOptionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddorDHCPOptions'
--
-- * 'ddorStatus'
data DescribeDHCPOptionsResponse = DescribeDHCPOptionsResponse'
    { _ddorDHCPOptions :: !(Maybe [DHCPOptions])
    , _ddorStatus      :: !Status
    } deriving (Eq,Read,Show)

-- | 'DescribeDHCPOptionsResponse' smart constructor.
describeDHCPOptionsResponse :: Status -> DescribeDHCPOptionsResponse
describeDHCPOptionsResponse pStatus =
    DescribeDHCPOptionsResponse'
    { _ddorDHCPOptions = Nothing
    , _ddorStatus = pStatus
    }

-- | Information about one or more DHCP options sets.
ddorDHCPOptions :: Lens' DescribeDHCPOptionsResponse [DHCPOptions]
ddorDHCPOptions = lens _ddorDHCPOptions (\ s a -> s{_ddorDHCPOptions = a}) . _Default;

-- | FIXME: Undocumented member.
ddorStatus :: Lens' DescribeDHCPOptionsResponse Status
ddorStatus = lens _ddorStatus (\ s a -> s{_ddorStatus = a});
