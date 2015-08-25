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
-- Module      : Network.AWS.EC2.DescribeAccountAttributes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes attributes of your AWS account. The following are the
-- supported account attributes:
--
-- -   'supported-platforms': Indicates whether your account can launch
--     instances into EC2-Classic and EC2-VPC, or only into EC2-VPC.
--
-- -   'default-vpc': The ID of the default VPC for your account, or
--     'none'.
--
-- -   'max-instances': The maximum number of On-Demand instances that you
--     can run.
--
-- -   'vpc-max-security-groups-per-interface': The maximum number of
--     security groups that you can assign to a network interface.
--
-- -   'max-elastic-ips': The maximum number of Elastic IP addresses that
--     you can allocate for use with EC2-Classic.
--
-- -   'vpc-max-elastic-ips': The maximum number of Elastic IP addresses
--     that you can allocate for use with EC2-VPC.
--
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeAccountAttributes.html AWS API Reference> for DescribeAccountAttributes.
module Network.AWS.EC2.DescribeAccountAttributes
    (
    -- * Creating a Request
      describeAccountAttributes
    , DescribeAccountAttributes
    -- * Request Lenses
    , daaAttributeNames
    , daaDryRun

    -- * Destructuring the Response
    , describeAccountAttributesResponse
    , DescribeAccountAttributesResponse
    -- * Response Lenses
    , daarsAccountAttributes
    , daarsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeAccountAttributes' smart constructor.
data DescribeAccountAttributes = DescribeAccountAttributes'
    { _daaAttributeNames :: !(Maybe [AccountAttributeName])
    , _daaDryRun         :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeAccountAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daaAttributeNames'
--
-- * 'daaDryRun'
describeAccountAttributes
    :: DescribeAccountAttributes
describeAccountAttributes =
    DescribeAccountAttributes'
    { _daaAttributeNames = Nothing
    , _daaDryRun = Nothing
    }

-- | One or more account attribute names.
daaAttributeNames :: Lens' DescribeAccountAttributes [AccountAttributeName]
daaAttributeNames = lens _daaAttributeNames (\ s a -> s{_daaAttributeNames = a}) . _Default . _Coerce;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
daaDryRun :: Lens' DescribeAccountAttributes (Maybe Bool)
daaDryRun = lens _daaDryRun (\ s a -> s{_daaDryRun = a});

instance AWSRequest DescribeAccountAttributes where
        type Rs DescribeAccountAttributes =
             DescribeAccountAttributesResponse
        request = postQuery eC2
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
                 (toQueryList "AttributeName" <$> _daaAttributeNames),
               "DryRun" =: _daaDryRun]

-- | /See:/ 'describeAccountAttributesResponse' smart constructor.
data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse'
    { _daarsAccountAttributes :: !(Maybe [AccountAttribute])
    , _daarsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeAccountAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daarsAccountAttributes'
--
-- * 'daarsStatus'
describeAccountAttributesResponse
    :: Int -- ^ 'daarsStatus'
    -> DescribeAccountAttributesResponse
describeAccountAttributesResponse pStatus_ =
    DescribeAccountAttributesResponse'
    { _daarsAccountAttributes = Nothing
    , _daarsStatus = pStatus_
    }

-- | Information about one or more account attributes.
daarsAccountAttributes :: Lens' DescribeAccountAttributesResponse [AccountAttribute]
daarsAccountAttributes = lens _daarsAccountAttributes (\ s a -> s{_daarsAccountAttributes = a}) . _Default . _Coerce;

-- | The response status code.
daarsStatus :: Lens' DescribeAccountAttributesResponse Int
daarsStatus = lens _daarsStatus (\ s a -> s{_daarsStatus = a});
