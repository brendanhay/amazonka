{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeDHCPOptions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your DHCP options sets.
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
    , ddorqFilters
    , ddorqDHCPOptionsIds
    , ddorqDryRun

    -- * Response
    , DescribeDHCPOptionsResponse
    -- ** Response constructor
    , describeDHCPOptionsResponse
    -- ** Response lenses
    , ddorsDHCPOptions
    , ddorsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeDHCPOptions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddorqFilters'
--
-- * 'ddorqDHCPOptionsIds'
--
-- * 'ddorqDryRun'
data DescribeDHCPOptions = DescribeDHCPOptions'
    { _ddorqFilters        :: !(Maybe [Filter])
    , _ddorqDHCPOptionsIds :: !(Maybe [Text])
    , _ddorqDryRun         :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDHCPOptions' smart constructor.
describeDHCPOptions :: DescribeDHCPOptions
describeDHCPOptions =
    DescribeDHCPOptions'
    { _ddorqFilters = Nothing
    , _ddorqDHCPOptionsIds = Nothing
    , _ddorqDryRun = Nothing
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
ddorqFilters :: Lens' DescribeDHCPOptions [Filter]
ddorqFilters = lens _ddorqFilters (\ s a -> s{_ddorqFilters = a}) . _Default;

-- | The IDs of one or more DHCP options sets.
--
-- Default: Describes all your DHCP options sets.
ddorqDHCPOptionsIds :: Lens' DescribeDHCPOptions [Text]
ddorqDHCPOptionsIds = lens _ddorqDHCPOptionsIds (\ s a -> s{_ddorqDHCPOptionsIds = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
ddorqDryRun :: Lens' DescribeDHCPOptions (Maybe Bool)
ddorqDryRun = lens _ddorqDryRun (\ s a -> s{_ddorqDryRun = a});

instance AWSRequest DescribeDHCPOptions where
        type Sv DescribeDHCPOptions = EC2
        type Rs DescribeDHCPOptions =
             DescribeDHCPOptionsResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeDHCPOptionsResponse' <$>
                   (x .@? "dhcpOptionsSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeDHCPOptions where
        toHeaders = const mempty

instance ToPath DescribeDHCPOptions where
        toPath = const "/"

instance ToQuery DescribeDHCPOptions where
        toQuery DescribeDHCPOptions'{..}
          = mconcat
              ["Action" =: ("DescribeDHCPOptions" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _ddorqFilters),
               toQuery
                 (toQueryList "DhcpOptionsId" <$>
                    _ddorqDHCPOptionsIds),
               "DryRun" =: _ddorqDryRun]

-- | /See:/ 'describeDHCPOptionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddorsDHCPOptions'
--
-- * 'ddorsStatus'
data DescribeDHCPOptionsResponse = DescribeDHCPOptionsResponse'
    { _ddorsDHCPOptions :: !(Maybe [DHCPOptions])
    , _ddorsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDHCPOptionsResponse' smart constructor.
describeDHCPOptionsResponse :: Int -> DescribeDHCPOptionsResponse
describeDHCPOptionsResponse pStatus =
    DescribeDHCPOptionsResponse'
    { _ddorsDHCPOptions = Nothing
    , _ddorsStatus = pStatus
    }

-- | Information about one or more DHCP options sets.
ddorsDHCPOptions :: Lens' DescribeDHCPOptionsResponse [DHCPOptions]
ddorsDHCPOptions = lens _ddorsDHCPOptions (\ s a -> s{_ddorsDHCPOptions = a}) . _Default;

-- | FIXME: Undocumented member.
ddorsStatus :: Lens' DescribeDHCPOptionsResponse Int
ddorsStatus = lens _ddorsStatus (\ s a -> s{_ddorsStatus = a});
