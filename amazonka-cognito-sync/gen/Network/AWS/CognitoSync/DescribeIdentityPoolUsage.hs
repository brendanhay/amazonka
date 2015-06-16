{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CognitoSync.DescribeIdentityPoolUsage
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

-- | Gets usage details (for example, data storage) about a particular
-- identity pool.
--
-- DescribeIdentityPoolUsage can only be called with developer credentials.
-- You cannot make this API call with the temporary user credentials
-- provided by Cognito Identity.
--
-- <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_DescribeIdentityPoolUsage.html>
module Network.AWS.CognitoSync.DescribeIdentityPoolUsage
    (
    -- * Request
      DescribeIdentityPoolUsage
    -- ** Request constructor
    , describeIdentityPoolUsage
    -- ** Request lenses
    , dipuIdentityPoolId

    -- * Response
    , DescribeIdentityPoolUsageResponse
    -- ** Response constructor
    , describeIdentityPoolUsageResponse
    -- ** Response lenses
    , dipurIdentityPoolUsage
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CognitoSync.Types

-- | /See:/ 'describeIdentityPoolUsage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dipuIdentityPoolId'
newtype DescribeIdentityPoolUsage = DescribeIdentityPoolUsage'{_dipuIdentityPoolId :: Text} deriving (Eq, Read, Show)

-- | 'DescribeIdentityPoolUsage' smart constructor.
describeIdentityPoolUsage :: Text -> DescribeIdentityPoolUsage
describeIdentityPoolUsage pIdentityPoolId = DescribeIdentityPoolUsage'{_dipuIdentityPoolId = pIdentityPoolId};

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
dipuIdentityPoolId :: Lens' DescribeIdentityPoolUsage Text
dipuIdentityPoolId = lens _dipuIdentityPoolId (\ s a -> s{_dipuIdentityPoolId = a});

instance AWSRequest DescribeIdentityPoolUsage where
        type Sv DescribeIdentityPoolUsage = CognitoSync
        type Rs DescribeIdentityPoolUsage =
             DescribeIdentityPoolUsageResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 DescribeIdentityPoolUsageResponse' <$>
                   (x .?> "IdentityPoolUsage"))

instance ToHeaders DescribeIdentityPoolUsage where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeIdentityPoolUsage where
        toPath DescribeIdentityPoolUsage'{..}
          = mconcat
              ["/identitypools/", toText _dipuIdentityPoolId]

instance ToQuery DescribeIdentityPoolUsage where
        toQuery = const mempty

-- | /See:/ 'describeIdentityPoolUsageResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dipurIdentityPoolUsage'
newtype DescribeIdentityPoolUsageResponse = DescribeIdentityPoolUsageResponse'{_dipurIdentityPoolUsage :: Maybe IdentityPoolUsage} deriving (Eq, Read, Show)

-- | 'DescribeIdentityPoolUsageResponse' smart constructor.
describeIdentityPoolUsageResponse :: DescribeIdentityPoolUsageResponse
describeIdentityPoolUsageResponse = DescribeIdentityPoolUsageResponse'{_dipurIdentityPoolUsage = Nothing};

-- | Information about the usage of the identity pool.
dipurIdentityPoolUsage :: Lens' DescribeIdentityPoolUsageResponse (Maybe IdentityPoolUsage)
dipurIdentityPoolUsage = lens _dipurIdentityPoolUsage (\ s a -> s{_dipurIdentityPoolUsage = a});
