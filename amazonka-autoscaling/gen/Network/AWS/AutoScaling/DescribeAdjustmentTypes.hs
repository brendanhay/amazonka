{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeAdjustmentTypes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes the policy adjustment types for use with PutScalingPolicy.
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
    , datrsAdjustmentTypes
    , datrsStatus
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
        request = postQuery
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
-- * 'datrsAdjustmentTypes'
--
-- * 'datrsStatus'
data DescribeAdjustmentTypesResponse = DescribeAdjustmentTypesResponse'
    { _datrsAdjustmentTypes :: !(Maybe [AdjustmentType])
    , _datrsStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAdjustmentTypesResponse' smart constructor.
describeAdjustmentTypesResponse :: Int -> DescribeAdjustmentTypesResponse
describeAdjustmentTypesResponse pStatus_ =
    DescribeAdjustmentTypesResponse'
    { _datrsAdjustmentTypes = Nothing
    , _datrsStatus = pStatus_
    }

-- | The policy adjustment types.
datrsAdjustmentTypes :: Lens' DescribeAdjustmentTypesResponse [AdjustmentType]
datrsAdjustmentTypes = lens _datrsAdjustmentTypes (\ s a -> s{_datrsAdjustmentTypes = a}) . _Default;

-- | FIXME: Undocumented member.
datrsStatus :: Lens' DescribeAdjustmentTypesResponse Int
datrsStatus = lens _datrsStatus (\ s a -> s{_datrsStatus = a});
