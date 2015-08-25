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
-- Module      : Network.AWS.EC2.DescribeKeyPairs
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your key pairs.
--
-- For more information about key pairs, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Key Pairs>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeKeyPairs.html AWS API Reference> for DescribeKeyPairs.
module Network.AWS.EC2.DescribeKeyPairs
    (
    -- * Creating a Request
      describeKeyPairs
    , DescribeKeyPairs
    -- * Request Lenses
    , dkpsFilters
    , dkpsKeyNames
    , dkpsDryRun

    -- * Destructuring the Response
    , describeKeyPairsResponse
    , DescribeKeyPairsResponse
    -- * Response Lenses
    , dkprsKeyPairs
    , dkprsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeKeyPairs' smart constructor.
data DescribeKeyPairs = DescribeKeyPairs'
    { _dkpsFilters  :: !(Maybe [Filter])
    , _dkpsKeyNames :: !(Maybe [Text])
    , _dkpsDryRun   :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeKeyPairs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dkpsFilters'
--
-- * 'dkpsKeyNames'
--
-- * 'dkpsDryRun'
describeKeyPairs
    :: DescribeKeyPairs
describeKeyPairs =
    DescribeKeyPairs'
    { _dkpsFilters = Nothing
    , _dkpsKeyNames = Nothing
    , _dkpsDryRun = Nothing
    }

-- | One or more filters.
--
-- -   'fingerprint' - The fingerprint of the key pair.
--
-- -   'key-name' - The name of the key pair.
--
dkpsFilters :: Lens' DescribeKeyPairs [Filter]
dkpsFilters = lens _dkpsFilters (\ s a -> s{_dkpsFilters = a}) . _Default . _Coerce;

-- | One or more key pair names.
--
-- Default: Describes all your key pairs.
dkpsKeyNames :: Lens' DescribeKeyPairs [Text]
dkpsKeyNames = lens _dkpsKeyNames (\ s a -> s{_dkpsKeyNames = a}) . _Default . _Coerce;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
dkpsDryRun :: Lens' DescribeKeyPairs (Maybe Bool)
dkpsDryRun = lens _dkpsDryRun (\ s a -> s{_dkpsDryRun = a});

instance AWSRequest DescribeKeyPairs where
        type Rs DescribeKeyPairs = DescribeKeyPairsResponse
        request = postQuery eC2
        response
          = receiveXML
              (\ s h x ->
                 DescribeKeyPairsResponse' <$>
                   (x .@? "keySet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeKeyPairs where
        toHeaders = const mempty

instance ToPath DescribeKeyPairs where
        toPath = const "/"

instance ToQuery DescribeKeyPairs where
        toQuery DescribeKeyPairs'{..}
          = mconcat
              ["Action" =: ("DescribeKeyPairs" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dkpsFilters),
               toQuery (toQueryList "KeyName" <$> _dkpsKeyNames),
               "DryRun" =: _dkpsDryRun]

-- | /See:/ 'describeKeyPairsResponse' smart constructor.
data DescribeKeyPairsResponse = DescribeKeyPairsResponse'
    { _dkprsKeyPairs :: !(Maybe [KeyPairInfo])
    , _dkprsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeKeyPairsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dkprsKeyPairs'
--
-- * 'dkprsStatus'
describeKeyPairsResponse
    :: Int -- ^ 'dkprsStatus'
    -> DescribeKeyPairsResponse
describeKeyPairsResponse pStatus_ =
    DescribeKeyPairsResponse'
    { _dkprsKeyPairs = Nothing
    , _dkprsStatus = pStatus_
    }

-- | Information about one or more key pairs.
dkprsKeyPairs :: Lens' DescribeKeyPairsResponse [KeyPairInfo]
dkprsKeyPairs = lens _dkprsKeyPairs (\ s a -> s{_dkprsKeyPairs = a}) . _Default . _Coerce;

-- | The response status code.
dkprsStatus :: Lens' DescribeKeyPairsResponse Int
dkprsStatus = lens _dkprsStatus (\ s a -> s{_dkprsStatus = a});
