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
-- Module      : Network.AWS.CognitoSync.DescribeIdentityPoolUsage
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets usage details (for example, data storage) about a particular
-- identity pool.
--
-- This API can only be called with developer credentials. You cannot call
-- this API with the temporary user credentials provided by Cognito
-- Identity.
--
-- /See:/ <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_DescribeIdentityPoolUsage.html AWS API Reference> for DescribeIdentityPoolUsage.
module Network.AWS.CognitoSync.DescribeIdentityPoolUsage
    (
    -- * Creating a Request
      describeIdentityPoolUsage
    , DescribeIdentityPoolUsage
    -- * Request Lenses
    , dipuIdentityPoolId

    -- * Destructuring the Response
    , describeIdentityPoolUsageResponse
    , DescribeIdentityPoolUsageResponse
    -- * Response Lenses
    , dipursIdentityPoolUsage
    , dipursStatus
    ) where

import           Network.AWS.CognitoSync.Types
import           Network.AWS.CognitoSync.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | A request for usage information about the identity pool.
--
-- /See:/ 'describeIdentityPoolUsage' smart constructor.
newtype DescribeIdentityPoolUsage = DescribeIdentityPoolUsage'
    { _dipuIdentityPoolId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeIdentityPoolUsage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dipuIdentityPoolId'
describeIdentityPoolUsage
    :: Text -- ^ 'dipuIdentityPoolId'
    -> DescribeIdentityPoolUsage
describeIdentityPoolUsage pIdentityPoolId_ =
    DescribeIdentityPoolUsage'
    { _dipuIdentityPoolId = pIdentityPoolId_
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
dipuIdentityPoolId :: Lens' DescribeIdentityPoolUsage Text
dipuIdentityPoolId = lens _dipuIdentityPoolId (\ s a -> s{_dipuIdentityPoolId = a});

instance AWSRequest DescribeIdentityPoolUsage where
        type Rs DescribeIdentityPoolUsage =
             DescribeIdentityPoolUsageResponse
        request = get cognitoSync
        response
          = receiveJSON
              (\ s h x ->
                 DescribeIdentityPoolUsageResponse' <$>
                   (x .?> "IdentityPoolUsage") <*> (pure (fromEnum s)))

instance ToHeaders DescribeIdentityPoolUsage where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeIdentityPoolUsage where
        toPath DescribeIdentityPoolUsage'{..}
          = mconcat
              ["/identitypools/", toBS _dipuIdentityPoolId]

instance ToQuery DescribeIdentityPoolUsage where
        toQuery = const mempty

-- | Response to a successful DescribeIdentityPoolUsage request.
--
-- /See:/ 'describeIdentityPoolUsageResponse' smart constructor.
data DescribeIdentityPoolUsageResponse = DescribeIdentityPoolUsageResponse'
    { _dipursIdentityPoolUsage :: !(Maybe IdentityPoolUsage)
    , _dipursStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeIdentityPoolUsageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dipursIdentityPoolUsage'
--
-- * 'dipursStatus'
describeIdentityPoolUsageResponse
    :: Int -- ^ 'dipursStatus'
    -> DescribeIdentityPoolUsageResponse
describeIdentityPoolUsageResponse pStatus_ =
    DescribeIdentityPoolUsageResponse'
    { _dipursIdentityPoolUsage = Nothing
    , _dipursStatus = pStatus_
    }

-- | Information about the usage of the identity pool.
dipursIdentityPoolUsage :: Lens' DescribeIdentityPoolUsageResponse (Maybe IdentityPoolUsage)
dipursIdentityPoolUsage = lens _dipursIdentityPoolUsage (\ s a -> s{_dipursIdentityPoolUsage = a});

-- | The response status code.
dipursStatus :: Lens' DescribeIdentityPoolUsageResponse Int
dipursStatus = lens _dipursStatus (\ s a -> s{_dipursStatus = a});
