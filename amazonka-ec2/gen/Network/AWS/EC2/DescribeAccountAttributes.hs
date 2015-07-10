{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeAccountAttributes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes attributes of your AWS account. The following are the
-- supported account attributes:
--
-- -   @supported-platforms@: Indicates whether your account can launch
--     instances into EC2-Classic and EC2-VPC, or only into EC2-VPC.
--
-- -   @default-vpc@: The ID of the default VPC for your account, or
--     @none@.
--
-- -   @max-instances@: The maximum number of On-Demand instances that you
--     can run.
--
-- -   @vpc-max-security-groups-per-interface@: The maximum number of
--     security groups that you can assign to a network interface.
--
-- -   @max-elastic-ips@: The maximum number of Elastic IP addresses that
--     you can allocate for use with EC2-Classic.
--
-- -   @vpc-max-elastic-ips@: The maximum number of Elastic IP addresses
--     that you can allocate for use with EC2-VPC.
--
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeAccountAttributes.html>
module Network.AWS.EC2.DescribeAccountAttributes
    (
    -- * Request
      DescribeAccountAttributes
    -- ** Request constructor
    , describeAccountAttributes
    -- ** Request lenses
    , daaAttributeNames
    , daaDryRun

    -- * Response
    , DescribeAccountAttributesResponse
    -- ** Response constructor
    , describeAccountAttributesResponse
    -- ** Response lenses
    , daarAccountAttributes
    , daarStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeAccountAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daaAttributeNames'
--
-- * 'daaDryRun'
data DescribeAccountAttributes = DescribeAccountAttributes'
    { _daaAttributeNames :: !(Maybe [AccountAttributeName])
    , _daaDryRun         :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAccountAttributes' smart constructor.
describeAccountAttributes :: DescribeAccountAttributes
describeAccountAttributes =
    DescribeAccountAttributes'
    { _daaAttributeNames = Nothing
    , _daaDryRun = Nothing
    }

-- | One or more account attribute names.
daaAttributeNames :: Lens' DescribeAccountAttributes [AccountAttributeName]
daaAttributeNames = lens _daaAttributeNames (\ s a -> s{_daaAttributeNames = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
daaDryRun :: Lens' DescribeAccountAttributes (Maybe Bool)
daaDryRun = lens _daaDryRun (\ s a -> s{_daaDryRun = a});

instance AWSRequest DescribeAccountAttributes where
        type Sv DescribeAccountAttributes = EC2
        type Rs DescribeAccountAttributes =
             DescribeAccountAttributesResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeAccountAttributesResponse' <$>
                   (x .@? "accountAttributeSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeAccountAttributes where
        toHeaders = const mempty

instance ToPath DescribeAccountAttributes where
        toPath = const "/"

instance ToQuery DescribeAccountAttributes where
        toQuery DescribeAccountAttributes'{..}
          = mconcat
              ["Action" =:
                 ("DescribeAccountAttributes" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery
                 (toQueryList "attributeName" <$> _daaAttributeNames),
               "DryRun" =: _daaDryRun]

-- | /See:/ 'describeAccountAttributesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daarAccountAttributes'
--
-- * 'daarStatus'
data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse'
    { _daarAccountAttributes :: !(Maybe [AccountAttribute])
    , _daarStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAccountAttributesResponse' smart constructor.
describeAccountAttributesResponse :: Int -> DescribeAccountAttributesResponse
describeAccountAttributesResponse pStatus =
    DescribeAccountAttributesResponse'
    { _daarAccountAttributes = Nothing
    , _daarStatus = pStatus
    }

-- | Information about one or more account attributes.
daarAccountAttributes :: Lens' DescribeAccountAttributesResponse [AccountAttribute]
daarAccountAttributes = lens _daarAccountAttributes (\ s a -> s{_daarAccountAttributes = a}) . _Default;

-- | FIXME: Undocumented member.
daarStatus :: Lens' DescribeAccountAttributesResponse Int
daarStatus = lens _daarStatus (\ s a -> s{_daarStatus = a});
