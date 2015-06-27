{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.DescribeKeyPairs
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

-- | Describes one or more of your key pairs.
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
    , dkp1Filters
    , dkp1KeyNames
    , dkp1DryRun

    -- * Response
    , DescribeKeyPairsResponse
    -- ** Response constructor
    , describeKeyPairsResponse
    -- ** Response lenses
    , dkprKeyPairs
    , dkprStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeKeyPairs' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dkp1Filters'
--
-- * 'dkp1KeyNames'
--
-- * 'dkp1DryRun'
data DescribeKeyPairs = DescribeKeyPairs'
    { _dkp1Filters  :: Maybe [Filter]
    , _dkp1KeyNames :: Maybe [Text]
    , _dkp1DryRun   :: Maybe Bool
    } deriving (Eq,Read,Show)

-- | 'DescribeKeyPairs' smart constructor.
describeKeyPairs :: DescribeKeyPairs
describeKeyPairs =
    DescribeKeyPairs'
    { _dkp1Filters = Nothing
    , _dkp1KeyNames = Nothing
    , _dkp1DryRun = Nothing
    }

-- | One or more filters.
--
-- -   @fingerprint@ - The fingerprint of the key pair.
--
-- -   @key-name@ - The name of the key pair.
--
dkp1Filters :: Lens' DescribeKeyPairs [Filter]
dkp1Filters = lens _dkp1Filters (\ s a -> s{_dkp1Filters = a}) . _Default;

-- | One or more key pair names.
--
-- Default: Describes all your key pairs.
dkp1KeyNames :: Lens' DescribeKeyPairs [Text]
dkp1KeyNames = lens _dkp1KeyNames (\ s a -> s{_dkp1KeyNames = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dkp1DryRun :: Lens' DescribeKeyPairs (Maybe Bool)
dkp1DryRun = lens _dkp1DryRun (\ s a -> s{_dkp1DryRun = a});

instance AWSRequest DescribeKeyPairs where
        type Sv DescribeKeyPairs = EC2
        type Rs DescribeKeyPairs = DescribeKeyPairsResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeKeyPairsResponse' <$>
                   (may (parseXMLList "item") x) <*>
                     (pure (fromEnum s)))

instance ToHeaders DescribeKeyPairs where
        toHeaders = const mempty

instance ToPath DescribeKeyPairs where
        toPath = const "/"

instance ToQuery DescribeKeyPairs where
        toQuery DescribeKeyPairs'{..}
          = mconcat
              ["Action" =: ("DescribeKeyPairs" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dkp1Filters),
               toQuery (toQueryList "KeyName" <$> _dkp1KeyNames),
               "DryRun" =: _dkp1DryRun]

-- | /See:/ 'describeKeyPairsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dkprKeyPairs'
--
-- * 'dkprStatus'
data DescribeKeyPairsResponse = DescribeKeyPairsResponse'
    { _dkprKeyPairs :: Maybe [KeyPairInfo]
    , _dkprStatus   :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeKeyPairsResponse' smart constructor.
describeKeyPairsResponse :: Int -> DescribeKeyPairsResponse
describeKeyPairsResponse pStatus =
    DescribeKeyPairsResponse'
    { _dkprKeyPairs = Nothing
    , _dkprStatus = pStatus
    }

-- | Information about one or more key pairs.
dkprKeyPairs :: Lens' DescribeKeyPairsResponse [KeyPairInfo]
dkprKeyPairs = lens _dkprKeyPairs (\ s a -> s{_dkprKeyPairs = a}) . _Default;

-- | FIXME: Undocumented member.
dkprStatus :: Lens' DescribeKeyPairsResponse Int
dkprStatus = lens _dkprStatus (\ s a -> s{_dkprStatus = a});
