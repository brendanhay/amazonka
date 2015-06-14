{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.DirectConnect.DescribeLocations
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

-- | Returns the list of AWS Direct Connect locations in the current AWS
-- region. These are the locations that may be selected when calling
-- CreateConnection or CreateInterconnect.
--
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_DescribeLocations.html>
module Network.AWS.DirectConnect.DescribeLocations
    (
    -- * Request
      DescribeLocations
    -- ** Request constructor
    , describeLocations

    -- * Response
    , DescribeLocationsResponse
    -- ** Response constructor
    , describeLocationsResponse
    -- ** Response lenses
    , dlrDescribeLocationsResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.DirectConnect.Types

-- | /See:/ 'describeLocations' smart constructor.
data DescribeLocations = DescribeLocations' deriving (Eq, Read, Show)

-- | 'DescribeLocations' smart constructor.
describeLocations :: DescribeLocations
describeLocations = DescribeLocations';

instance AWSRequest DescribeLocations where
        type Sv DescribeLocations = DirectConnect
        type Rs DescribeLocations = DescribeLocationsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeLocationsResponse' <$>
                   x .?> "DescribeLocationsResponse" .!@ mempty)

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
-- * 'dlrDescribeLocationsResponse'
newtype DescribeLocationsResponse = DescribeLocationsResponse'{_dlrDescribeLocationsResponse :: Maybe [Location]} deriving (Eq, Read, Show)

-- | 'DescribeLocationsResponse' smart constructor.
describeLocationsResponse :: DescribeLocationsResponse
describeLocationsResponse = DescribeLocationsResponse'{_dlrDescribeLocationsResponse = Nothing};

-- | FIXME: Undocumented member.
dlrDescribeLocationsResponse :: Lens' DescribeLocationsResponse (Maybe [Location])
dlrDescribeLocationsResponse = lens _dlrDescribeLocationsResponse (\ s a -> s{_dlrDescribeLocationsResponse = a});
