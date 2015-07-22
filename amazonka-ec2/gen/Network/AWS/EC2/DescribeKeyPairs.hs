{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeKeyPairs
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your key pairs.
--
-- For more information about key pairs, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Key Pairs>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeKeyPairs.html>
module Network.AWS.EC2.DescribeKeyPairs
    (
    -- * Request
      DescribeKeyPairs
    -- ** Request constructor
    , describeKeyPairs
    -- ** Request lenses
    , dkpsrqFilters
    , dkpsrqKeyNames
    , dkpsrqDryRun

    -- * Response
    , DescribeKeyPairsResponse
    -- ** Response constructor
    , describeKeyPairsResponse
    -- ** Response lenses
    , dkprsKeyPairs
    , dkprsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeKeyPairs' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dkpsrqFilters'
--
-- * 'dkpsrqKeyNames'
--
-- * 'dkpsrqDryRun'
data DescribeKeyPairs = DescribeKeyPairs'
    { _dkpsrqFilters  :: !(Maybe [Filter])
    , _dkpsrqKeyNames :: !(Maybe [Text])
    , _dkpsrqDryRun   :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeKeyPairs' smart constructor.
describeKeyPairs :: DescribeKeyPairs
describeKeyPairs =
    DescribeKeyPairs'
    { _dkpsrqFilters = Nothing
    , _dkpsrqKeyNames = Nothing
    , _dkpsrqDryRun = Nothing
    }

-- | One or more filters.
--
-- -   @fingerprint@ - The fingerprint of the key pair.
--
-- -   @key-name@ - The name of the key pair.
--
dkpsrqFilters :: Lens' DescribeKeyPairs [Filter]
dkpsrqFilters = lens _dkpsrqFilters (\ s a -> s{_dkpsrqFilters = a}) . _Default;

-- | One or more key pair names.
--
-- Default: Describes all your key pairs.
dkpsrqKeyNames :: Lens' DescribeKeyPairs [Text]
dkpsrqKeyNames = lens _dkpsrqKeyNames (\ s a -> s{_dkpsrqKeyNames = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dkpsrqDryRun :: Lens' DescribeKeyPairs (Maybe Bool)
dkpsrqDryRun = lens _dkpsrqDryRun (\ s a -> s{_dkpsrqDryRun = a});

instance AWSRequest DescribeKeyPairs where
        type Sv DescribeKeyPairs = EC2
        type Rs DescribeKeyPairs = DescribeKeyPairsResponse
        request = post
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
               toQuery (toQueryList "Filter" <$> _dkpsrqFilters),
               toQuery (toQueryList "KeyName" <$> _dkpsrqKeyNames),
               "DryRun" =: _dkpsrqDryRun]

-- | /See:/ 'describeKeyPairsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dkprsKeyPairs'
--
-- * 'dkprsStatus'
data DescribeKeyPairsResponse = DescribeKeyPairsResponse'
    { _dkprsKeyPairs :: !(Maybe [KeyPairInfo])
    , _dkprsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeKeyPairsResponse' smart constructor.
describeKeyPairsResponse :: Int -> DescribeKeyPairsResponse
describeKeyPairsResponse pStatus =
    DescribeKeyPairsResponse'
    { _dkprsKeyPairs = Nothing
    , _dkprsStatus = pStatus
    }

-- | Information about one or more key pairs.
dkprsKeyPairs :: Lens' DescribeKeyPairsResponse [KeyPairInfo]
dkprsKeyPairs = lens _dkprsKeyPairs (\ s a -> s{_dkprsKeyPairs = a}) . _Default;

-- | FIXME: Undocumented member.
dkprsStatus :: Lens' DescribeKeyPairsResponse Int
dkprsStatus = lens _dkprsStatus (\ s a -> s{_dkprsStatus = a});
