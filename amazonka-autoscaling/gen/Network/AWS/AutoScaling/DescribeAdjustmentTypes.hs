{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeAdjustmentTypes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Describes the policy adjustment types for use with PutScalingPolicy.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeAdjustmentTypes.html>
module Network.AWS.AutoScaling.DescribeAdjustmentTypes
    (
    -- * Request
      DescribeAdjustmentTypes
    -- ** Request constructor
    , describeAdjustmentTypes

    -- * Response
    , DescribeAdjustmentTypesResponse
    -- ** Response constructor
    , describeAdjustmentTypesResponse
    -- ** Response lenses
    , datrAdjustmentTypes
    , datrStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeAdjustmentTypes' smart constructor.
data DescribeAdjustmentTypes =
    DescribeAdjustmentTypes'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAdjustmentTypes' smart constructor.
describeAdjustmentTypes :: DescribeAdjustmentTypes
describeAdjustmentTypes = DescribeAdjustmentTypes'

instance AWSRequest DescribeAdjustmentTypes where
        type Sv DescribeAdjustmentTypes = AutoScaling
        type Rs DescribeAdjustmentTypes =
             DescribeAdjustmentTypesResponse
        request = post
        response
          = receiveXMLWrapper "DescribeAdjustmentTypesResult"
              (\ s h x ->
                 DescribeAdjustmentTypesResponse' <$>
                   (x .@? "AdjustmentTypes" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeAdjustmentTypes where
        toHeaders = const mempty

instance ToPath DescribeAdjustmentTypes where
        toPath = const "/"

instance ToQuery DescribeAdjustmentTypes where
        toQuery
          = const
              (mconcat
                 ["Action" =:
                    ("DescribeAdjustmentTypes" :: ByteString),
                  "Version" =: ("2011-01-01" :: ByteString)])

-- | /See:/ 'describeAdjustmentTypesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'datrAdjustmentTypes'
--
-- * 'datrStatus'
data DescribeAdjustmentTypesResponse = DescribeAdjustmentTypesResponse'
    { _datrAdjustmentTypes :: !(Maybe [AdjustmentType])
    , _datrStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAdjustmentTypesResponse' smart constructor.
describeAdjustmentTypesResponse :: Int -> DescribeAdjustmentTypesResponse
describeAdjustmentTypesResponse pStatus =
    DescribeAdjustmentTypesResponse'
    { _datrAdjustmentTypes = Nothing
    , _datrStatus = pStatus
    }

-- | The policy adjustment types.
datrAdjustmentTypes :: Lens' DescribeAdjustmentTypesResponse [AdjustmentType]
datrAdjustmentTypes = lens _datrAdjustmentTypes (\ s a -> s{_datrAdjustmentTypes = a}) . _Default;

-- | FIXME: Undocumented member.
datrStatus :: Lens' DescribeAdjustmentTypesResponse Int
datrStatus = lens _datrStatus (\ s a -> s{_datrStatus = a});
