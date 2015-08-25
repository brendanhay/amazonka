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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your DHCP options sets.
--
-- For more information about DHCP options sets, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_DHCP_Options.html DHCP Options Sets>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeDHCPOptions.html AWS API Reference> for DescribeDHCPOptions.
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
    , ddorsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeDHCPOptions' smart constructor.
data DescribeDHCPOptions = DescribeDHCPOptions'
    { _ddoFilters        :: !(Maybe [Filter])
    , _ddoDHCPOptionsIds :: !(Maybe [Text])
    , _ddoDryRun         :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeDHCPOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddoFilters'
--
-- * 'ddoDHCPOptionsIds'
--
-- * 'ddoDryRun'
describeDHCPOptions
    :: DescribeDHCPOptions
describeDHCPOptions =
    DescribeDHCPOptions'
    { _ddoFilters = Nothing
    , _ddoDHCPOptionsIds = Nothing
    , _ddoDryRun = Nothing
    }

-- | One or more filters.
--
-- -   'dhcp-options-id' - The ID of a set of DHCP options.
--
-- -   'key' - The key for one of the options (for example, 'domain-name').
--
-- -   'value' - The value for one of the options.
--
-- -   'tag':/key/=/value/ - The key\/value combination of a tag assigned
--     to the resource.
--
-- -   'tag-key' - The key of a tag assigned to the resource. This filter
--     is independent of the 'tag-value' filter. For example, if you use
--     both the filter \"tag-key=Purpose\" and the filter \"tag-value=X\",
--     you get any resources assigned both the tag key Purpose (regardless
--     of what the tag\'s value is), and the tag value X (regardless of
--     what the tag\'s key is). If you want to list only resources where
--     Purpose is X, see the 'tag':/key/=/value/ filter.
--
-- -   'tag-value' - The value of a tag assigned to the resource. This
--     filter is independent of the 'tag-key' filter.
--
ddoFilters :: Lens' DescribeDHCPOptions [Filter]
ddoFilters = lens _ddoFilters (\ s a -> s{_ddoFilters = a}) . _Default . _Coerce;

-- | The IDs of one or more DHCP options sets.
--
-- Default: Describes all your DHCP options sets.
ddoDHCPOptionsIds :: Lens' DescribeDHCPOptions [Text]
ddoDHCPOptionsIds = lens _ddoDHCPOptionsIds (\ s a -> s{_ddoDHCPOptionsIds = a}) . _Default . _Coerce;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
ddoDryRun :: Lens' DescribeDHCPOptions (Maybe Bool)
ddoDryRun = lens _ddoDryRun (\ s a -> s{_ddoDryRun = a});

instance AWSRequest DescribeDHCPOptions where
        type Rs DescribeDHCPOptions =
             DescribeDHCPOptionsResponse
        request = postQuery eC2
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
              ["Action" =: ("DescribeDhcpOptions" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _ddoFilters),
               toQuery
                 (toQueryList "DhcpOptionsId" <$> _ddoDHCPOptionsIds),
               "DryRun" =: _ddoDryRun]

-- | /See:/ 'describeDHCPOptionsResponse' smart constructor.
data DescribeDHCPOptionsResponse = DescribeDHCPOptionsResponse'
    { _ddorsDHCPOptions :: !(Maybe [DHCPOptions])
    , _ddorsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeDHCPOptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddorsDHCPOptions'
--
-- * 'ddorsStatus'
describeDHCPOptionsResponse
    :: Int -- ^ 'ddorsStatus'
    -> DescribeDHCPOptionsResponse
describeDHCPOptionsResponse pStatus_ =
    DescribeDHCPOptionsResponse'
    { _ddorsDHCPOptions = Nothing
    , _ddorsStatus = pStatus_
    }

-- | Information about one or more DHCP options sets.
ddorsDHCPOptions :: Lens' DescribeDHCPOptionsResponse [DHCPOptions]
ddorsDHCPOptions = lens _ddorsDHCPOptions (\ s a -> s{_ddorsDHCPOptions = a}) . _Default . _Coerce;

-- | The response status code.
ddorsStatus :: Lens' DescribeDHCPOptionsResponse Int
ddorsStatus = lens _ddorsStatus (\ s a -> s{_ddorsStatus = a});
