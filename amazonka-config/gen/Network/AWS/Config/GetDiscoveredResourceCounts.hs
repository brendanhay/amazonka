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
-- Module      : Network.AWS.Config.GetDiscoveredResourceCounts
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the resource types, the number of each resource type, and the total number of resources that AWS Config is recording in this region for your AWS account.
--
--
-- __Example__
--
--     * AWS Config is recording three resource types in the US East (Ohio) Region for your account: 25 EC2 instances, 20 IAM users, and 15 S3 buckets.
--
--     * You make a call to the @GetDiscoveredResourceCounts@ action and specify that you want all resource types.
--
--     * AWS Config returns the following:
--
--     * The resource types (EC2 instances, IAM users, and S3 buckets).
--
--     * The number of each resource type (25, 20, and 15).
--
--     * The total number of all resources (60).
--
--
--
--
--
-- The response is paginated. By default, AWS Config lists 100 'ResourceCount' objects on each page. You can customize this number with the @limit@ parameter. The response includes a @nextToken@ string. To get the next page of results, run the request again and specify the string for the @nextToken@ parameter.
--
module Network.AWS.Config.GetDiscoveredResourceCounts
    (
    -- * Creating a Request
      getDiscoveredResourceCounts
    , GetDiscoveredResourceCounts
    -- * Request Lenses
    , gdrcNextToken
    , gdrcLimit
    , gdrcResourceTypes

    -- * Destructuring the Response
    , getDiscoveredResourceCountsResponse
    , GetDiscoveredResourceCountsResponse
    -- * Response Lenses
    , gdrcrsTotalDiscoveredResources
    , gdrcrsNextToken
    , gdrcrsResourceCounts
    , gdrcrsResponseStatus
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDiscoveredResourceCounts' smart constructor.
data GetDiscoveredResourceCounts = GetDiscoveredResourceCounts'
  { _gdrcNextToken     :: !(Maybe Text)
  , _gdrcLimit         :: !(Maybe Nat)
  , _gdrcResourceTypes :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDiscoveredResourceCounts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdrcNextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'gdrcLimit' - The maximum number of 'ResourceCount' objects returned on each page. The default is 100. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
--
-- * 'gdrcResourceTypes' - The comma-separated list that specifies the resource types that you want AWS Config to return (for example, @"AWS::EC2::Instance"@ , @"AWS::IAM::User"@ ). If a value for @resourceTypes@ is not specified, AWS Config returns all resource types that AWS Config is recording in the region for your account.
getDiscoveredResourceCounts
    :: GetDiscoveredResourceCounts
getDiscoveredResourceCounts =
  GetDiscoveredResourceCounts'
    { _gdrcNextToken = Nothing
    , _gdrcLimit = Nothing
    , _gdrcResourceTypes = Nothing
    }


-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
gdrcNextToken :: Lens' GetDiscoveredResourceCounts (Maybe Text)
gdrcNextToken = lens _gdrcNextToken (\ s a -> s{_gdrcNextToken = a})

-- | The maximum number of 'ResourceCount' objects returned on each page. The default is 100. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
gdrcLimit :: Lens' GetDiscoveredResourceCounts (Maybe Natural)
gdrcLimit = lens _gdrcLimit (\ s a -> s{_gdrcLimit = a}) . mapping _Nat

-- | The comma-separated list that specifies the resource types that you want AWS Config to return (for example, @"AWS::EC2::Instance"@ , @"AWS::IAM::User"@ ). If a value for @resourceTypes@ is not specified, AWS Config returns all resource types that AWS Config is recording in the region for your account.
gdrcResourceTypes :: Lens' GetDiscoveredResourceCounts [Text]
gdrcResourceTypes = lens _gdrcResourceTypes (\ s a -> s{_gdrcResourceTypes = a}) . _Default . _Coerce

instance AWSRequest GetDiscoveredResourceCounts where
        type Rs GetDiscoveredResourceCounts =
             GetDiscoveredResourceCountsResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 GetDiscoveredResourceCountsResponse' <$>
                   (x .?> "totalDiscoveredResources") <*>
                     (x .?> "nextToken")
                     <*> (x .?> "resourceCounts" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetDiscoveredResourceCounts where

instance NFData GetDiscoveredResourceCounts where

instance ToHeaders GetDiscoveredResourceCounts where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.GetDiscoveredResourceCounts" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDiscoveredResourceCounts where
        toJSON GetDiscoveredResourceCounts'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _gdrcNextToken,
                  ("limit" .=) <$> _gdrcLimit,
                  ("resourceTypes" .=) <$> _gdrcResourceTypes])

instance ToPath GetDiscoveredResourceCounts where
        toPath = const "/"

instance ToQuery GetDiscoveredResourceCounts where
        toQuery = const mempty

-- | /See:/ 'getDiscoveredResourceCountsResponse' smart constructor.
data GetDiscoveredResourceCountsResponse = GetDiscoveredResourceCountsResponse'
  { _gdrcrsTotalDiscoveredResources :: !(Maybe Integer)
  , _gdrcrsNextToken                :: !(Maybe Text)
  , _gdrcrsResourceCounts           :: !(Maybe [ResourceCount])
  , _gdrcrsResponseStatus           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDiscoveredResourceCountsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdrcrsTotalDiscoveredResources' - The total number of resources that AWS Config is recording in the region for your account. If you specify resource types in the request, AWS Config returns only the total number of resources for those resource types. __Example__      * AWS Config is recording three resource types in the US East (Ohio) Region for your account: 25 EC2 instances, 20 IAM users, and 15 S3 buckets, for a total of 60 resources.     * You make a call to the @GetDiscoveredResourceCounts@ action and specify the resource type, @"AWS::EC2::Instances"@ , in the request.     * AWS Config returns 25 for @totalDiscoveredResources@ .
--
-- * 'gdrcrsNextToken' - The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- * 'gdrcrsResourceCounts' - The list of @ResourceCount@ objects. Each object is listed in descending order by the number of resources.
--
-- * 'gdrcrsResponseStatus' - -- | The response status code.
getDiscoveredResourceCountsResponse
    :: Int -- ^ 'gdrcrsResponseStatus'
    -> GetDiscoveredResourceCountsResponse
getDiscoveredResourceCountsResponse pResponseStatus_ =
  GetDiscoveredResourceCountsResponse'
    { _gdrcrsTotalDiscoveredResources = Nothing
    , _gdrcrsNextToken = Nothing
    , _gdrcrsResourceCounts = Nothing
    , _gdrcrsResponseStatus = pResponseStatus_
    }


-- | The total number of resources that AWS Config is recording in the region for your account. If you specify resource types in the request, AWS Config returns only the total number of resources for those resource types. __Example__      * AWS Config is recording three resource types in the US East (Ohio) Region for your account: 25 EC2 instances, 20 IAM users, and 15 S3 buckets, for a total of 60 resources.     * You make a call to the @GetDiscoveredResourceCounts@ action and specify the resource type, @"AWS::EC2::Instances"@ , in the request.     * AWS Config returns 25 for @totalDiscoveredResources@ .
gdrcrsTotalDiscoveredResources :: Lens' GetDiscoveredResourceCountsResponse (Maybe Integer)
gdrcrsTotalDiscoveredResources = lens _gdrcrsTotalDiscoveredResources (\ s a -> s{_gdrcrsTotalDiscoveredResources = a})

-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
gdrcrsNextToken :: Lens' GetDiscoveredResourceCountsResponse (Maybe Text)
gdrcrsNextToken = lens _gdrcrsNextToken (\ s a -> s{_gdrcrsNextToken = a})

-- | The list of @ResourceCount@ objects. Each object is listed in descending order by the number of resources.
gdrcrsResourceCounts :: Lens' GetDiscoveredResourceCountsResponse [ResourceCount]
gdrcrsResourceCounts = lens _gdrcrsResourceCounts (\ s a -> s{_gdrcrsResourceCounts = a}) . _Default . _Coerce

-- | -- | The response status code.
gdrcrsResponseStatus :: Lens' GetDiscoveredResourceCountsResponse Int
gdrcrsResponseStatus = lens _gdrcrsResponseStatus (\ s a -> s{_gdrcrsResponseStatus = a})

instance NFData GetDiscoveredResourceCountsResponse
         where
