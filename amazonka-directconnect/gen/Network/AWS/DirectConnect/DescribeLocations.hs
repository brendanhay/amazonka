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
-- Module      : Network.AWS.DirectConnect.DescribeLocations
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of AWS Direct Connect locations in the current AWS
-- region. These are the locations that may be selected when calling
-- CreateConnection or CreateInterconnect.
--
-- /See:/ <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_DescribeLocations.html AWS API Reference> for DescribeLocations.
module Network.AWS.DirectConnect.DescribeLocations
    (
    -- * Creating a Request
      DescribeLocations
    , describeLocations

    -- * Destructuring the Response
    , DescribeLocationsResponse
    , describeLocationsResponse
    -- * Response Lenses
    , dlrsLocations
    , dlrsStatus
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.DirectConnect.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeLocations' smart constructor.
data DescribeLocations =
    DescribeLocations' 
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLocations' smart constructor.
describeLocations :: DescribeLocations
describeLocations = DescribeLocations'

instance AWSRequest DescribeLocations where
        type Sv DescribeLocations = DirectConnect
        type Rs DescribeLocations = DescribeLocationsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeLocationsResponse' <$>
                   (x .?> "locations" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance ToHeaders DescribeLocations where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.DescribeLocations" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeLocations where
        toJSON = const (Object mempty)

instance ToPath DescribeLocations where
        toPath = const "/"

instance ToQuery DescribeLocations where
        toQuery = const mempty

-- | /See:/ 'describeLocationsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlrsLocations'
--
-- * 'dlrsStatus'
data DescribeLocationsResponse = DescribeLocationsResponse'
    { _dlrsLocations :: !(Maybe [Location])
    , _dlrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLocationsResponse' smart constructor.
describeLocationsResponse :: Int -> DescribeLocationsResponse
describeLocationsResponse pStatus_ = 
    DescribeLocationsResponse'
    { _dlrsLocations = Nothing
    , _dlrsStatus = pStatus_
    }

-- | Undocumented member.
dlrsLocations :: Lens' DescribeLocationsResponse [Location]
dlrsLocations = lens _dlrsLocations (\ s a -> s{_dlrsLocations = a}) . _Default . _Coerce;

-- | Undocumented member.
dlrsStatus :: Lens' DescribeLocationsResponse Int
dlrsStatus = lens _dlrsStatus (\ s a -> s{_dlrsStatus = a});
