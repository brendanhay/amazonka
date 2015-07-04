{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.AutoScaling.DescribeTerminationPolicyTypes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes the termination policies supported by Auto Scaling.
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
    , dtptrStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeTerminationPolicyTypes' smart constructor.
data DescribeTerminationPolicyTypes =
    DescribeTerminationPolicyTypes'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTerminationPolicyTypes' smart constructor.
describeTerminationPolicyTypes :: DescribeTerminationPolicyTypes
describeTerminationPolicyTypes = DescribeTerminationPolicyTypes'

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
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

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
--
-- * 'dtptrStatus'
data DescribeTerminationPolicyTypesResponse = DescribeTerminationPolicyTypesResponse'
    { _dtptrTerminationPolicyTypes :: !(Maybe [Text])
    , _dtptrStatus                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTerminationPolicyTypesResponse' smart constructor.
describeTerminationPolicyTypesResponse :: Int -> DescribeTerminationPolicyTypesResponse
describeTerminationPolicyTypesResponse pStatus =
    DescribeTerminationPolicyTypesResponse'
    { _dtptrTerminationPolicyTypes = Nothing
    , _dtptrStatus = pStatus
    }

-- | The termination policies supported by Auto Scaling (@OldestInstance@,
-- @OldestLaunchConfiguration@, @NewestInstance@,
-- @ClosestToNextInstanceHour@, and @Default@).
dtptrTerminationPolicyTypes :: Lens' DescribeTerminationPolicyTypesResponse [Text]
dtptrTerminationPolicyTypes = lens _dtptrTerminationPolicyTypes (\ s a -> s{_dtptrTerminationPolicyTypes = a}) . _Default;

-- | FIXME: Undocumented member.
dtptrStatus :: Lens' DescribeTerminationPolicyTypesResponse Int
dtptrStatus = lens _dtptrStatus (\ s a -> s{_dtptrStatus = a});
