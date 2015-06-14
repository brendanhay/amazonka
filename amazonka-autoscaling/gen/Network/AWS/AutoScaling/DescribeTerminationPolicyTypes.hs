{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.AutoScaling.DescribeTerminationPolicyTypes
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

-- | Lists the termination policies supported by Auto Scaling.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeTerminationPolicyTypes.html>
module Network.AWS.AutoScaling.DescribeTerminationPolicyTypes
    (
    -- * Request
      DescribeTerminationPolicyTypes
    -- ** Request constructor
    , describeTerminationPolicyTypes

    -- * Response
    , DescribeTerminationPolicyTypesResponse
    -- ** Response constructor
    , describeTerminationPolicyTypesResponse
    -- ** Response lenses
    , dtptrTerminationPolicyTypes
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.AutoScaling.Types

-- | /See:/ 'describeTerminationPolicyTypes' smart constructor.
data DescribeTerminationPolicyTypes = DescribeTerminationPolicyTypes' deriving (Eq, Read, Show)

-- | 'DescribeTerminationPolicyTypes' smart constructor.
describeTerminationPolicyTypes :: DescribeTerminationPolicyTypes
describeTerminationPolicyTypes = DescribeTerminationPolicyTypes';

instance AWSRequest DescribeTerminationPolicyTypes
         where
        type Sv DescribeTerminationPolicyTypes = AutoScaling
        type Rs DescribeTerminationPolicyTypes =
             DescribeTerminationPolicyTypesResponse
        request = post
        response
          = receiveXMLWrapper
              "DescribeTerminationPolicyTypesResult"
              (\ s h x ->
                 DescribeTerminationPolicyTypesResponse' <$>
                   (x .@? "TerminationPolicyTypes" .!@ mempty >>=
                      parseXMLList "member"))

instance ToHeaders DescribeTerminationPolicyTypes
         where
        toHeaders = const mempty

instance ToPath DescribeTerminationPolicyTypes where
        toPath = const "/"

instance ToQuery DescribeTerminationPolicyTypes where
        toQuery
          = const
              (mconcat
                 ["Action" =:
                    ("DescribeTerminationPolicyTypes" :: ByteString),
                  "Version" =: ("2011-01-01" :: ByteString)])

-- | /See:/ 'describeTerminationPolicyTypesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtptrTerminationPolicyTypes'
newtype DescribeTerminationPolicyTypesResponse = DescribeTerminationPolicyTypesResponse'{_dtptrTerminationPolicyTypes :: Maybe [Text]} deriving (Eq, Read, Show)

-- | 'DescribeTerminationPolicyTypesResponse' smart constructor.
describeTerminationPolicyTypesResponse :: DescribeTerminationPolicyTypesResponse
describeTerminationPolicyTypesResponse = DescribeTerminationPolicyTypesResponse'{_dtptrTerminationPolicyTypes = Nothing};

-- | The Termination policies supported by Auto Scaling. They are:
-- @OldestInstance@, @OldestLaunchConfiguration@, @NewestInstance@,
-- @ClosestToNextInstanceHour@, and @Default@.
dtptrTerminationPolicyTypes :: Lens' DescribeTerminationPolicyTypesResponse (Maybe [Text])
dtptrTerminationPolicyTypes = lens _dtptrTerminationPolicyTypes (\ s a -> s{_dtptrTerminationPolicyTypes = a});
