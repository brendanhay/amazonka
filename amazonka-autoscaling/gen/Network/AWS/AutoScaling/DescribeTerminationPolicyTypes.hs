{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeTerminationPolicyTypes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the termination policies supported by Auto Scaling.
--
-- /See:/ <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeTerminationPolicyTypes.html AWS API Reference> for DescribeTerminationPolicyTypes.
module Network.AWS.AutoScaling.DescribeTerminationPolicyTypes
    (
    -- * Creating a Request
      DescribeTerminationPolicyTypes
    , describeTerminationPolicyTypes

    -- * Destructuring the Response
    , DescribeTerminationPolicyTypesResponse
    , describeTerminationPolicyTypesResponse
    -- * Response Lenses
    , dtptrsTerminationPolicyTypes
    , dtptrsStatus
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
        request = postQuery
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
-- * 'dtptrsTerminationPolicyTypes'
--
-- * 'dtptrsStatus'
data DescribeTerminationPolicyTypesResponse = DescribeTerminationPolicyTypesResponse'
    { _dtptrsTerminationPolicyTypes :: !(Maybe [Text])
    , _dtptrsStatus                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTerminationPolicyTypesResponse' smart constructor.
describeTerminationPolicyTypesResponse :: Int -> DescribeTerminationPolicyTypesResponse
describeTerminationPolicyTypesResponse pStatus_ =
    DescribeTerminationPolicyTypesResponse'
    { _dtptrsTerminationPolicyTypes = Nothing
    , _dtptrsStatus = pStatus_
    }

-- | The termination policies supported by Auto Scaling (@OldestInstance@,
-- @OldestLaunchConfiguration@, @NewestInstance@,
-- @ClosestToNextInstanceHour@, and @Default@).
dtptrsTerminationPolicyTypes :: Lens' DescribeTerminationPolicyTypesResponse [Text]
dtptrsTerminationPolicyTypes = lens _dtptrsTerminationPolicyTypes (\ s a -> s{_dtptrsTerminationPolicyTypes = a}) . _Default . _Coerce;

-- | Undocumented member.
dtptrsStatus :: Lens' DescribeTerminationPolicyTypesResponse Int
dtptrsStatus = lens _dtptrsStatus (\ s a -> s{_dtptrsStatus = a});
