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
-- Module      : Network.AWS.AutoScalingPlans.DescribeScalingPlanResources
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the scalable resources in the specified scaling plan.
--
--
module Network.AWS.AutoScalingPlans.DescribeScalingPlanResources
    (
    -- * Creating a Request
      describeScalingPlanResources
    , DescribeScalingPlanResources
    -- * Request Lenses
    , dsprNextToken
    , dsprMaxResults
    , dsprScalingPlanName
    , dsprScalingPlanVersion

    -- * Destructuring the Response
    , describeScalingPlanResourcesResponse
    , DescribeScalingPlanResourcesResponse
    -- * Response Lenses
    , dsprrsNextToken
    , dsprrsScalingPlanResources
    , dsprrsResponseStatus
    ) where

import Network.AWS.AutoScalingPlans.Types
import Network.AWS.AutoScalingPlans.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeScalingPlanResources' smart constructor.
data DescribeScalingPlanResources = DescribeScalingPlanResources'
  { _dsprNextToken          :: !(Maybe Text)
  , _dsprMaxResults         :: !(Maybe Int)
  , _dsprScalingPlanName    :: !Text
  , _dsprScalingPlanVersion :: !Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeScalingPlanResources' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsprNextToken' - The token for the next set of results.
--
-- * 'dsprMaxResults' - The maximum number of scalable resources to return. This value can be between 1 and 50. The default value is 50.
--
-- * 'dsprScalingPlanName' - The name of the scaling plan.
--
-- * 'dsprScalingPlanVersion' - The version of the scaling plan.
describeScalingPlanResources
    :: Text -- ^ 'dsprScalingPlanName'
    -> Integer -- ^ 'dsprScalingPlanVersion'
    -> DescribeScalingPlanResources
describeScalingPlanResources pScalingPlanName_ pScalingPlanVersion_ =
  DescribeScalingPlanResources'
    { _dsprNextToken = Nothing
    , _dsprMaxResults = Nothing
    , _dsprScalingPlanName = pScalingPlanName_
    , _dsprScalingPlanVersion = pScalingPlanVersion_
    }


-- | The token for the next set of results.
dsprNextToken :: Lens' DescribeScalingPlanResources (Maybe Text)
dsprNextToken = lens _dsprNextToken (\ s a -> s{_dsprNextToken = a})

-- | The maximum number of scalable resources to return. This value can be between 1 and 50. The default value is 50.
dsprMaxResults :: Lens' DescribeScalingPlanResources (Maybe Int)
dsprMaxResults = lens _dsprMaxResults (\ s a -> s{_dsprMaxResults = a})

-- | The name of the scaling plan.
dsprScalingPlanName :: Lens' DescribeScalingPlanResources Text
dsprScalingPlanName = lens _dsprScalingPlanName (\ s a -> s{_dsprScalingPlanName = a})

-- | The version of the scaling plan.
dsprScalingPlanVersion :: Lens' DescribeScalingPlanResources Integer
dsprScalingPlanVersion = lens _dsprScalingPlanVersion (\ s a -> s{_dsprScalingPlanVersion = a})

instance AWSRequest DescribeScalingPlanResources
         where
        type Rs DescribeScalingPlanResources =
             DescribeScalingPlanResourcesResponse
        request = postJSON autoScalingPlans
        response
          = receiveJSON
              (\ s h x ->
                 DescribeScalingPlanResourcesResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "ScalingPlanResources" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeScalingPlanResources where

instance NFData DescribeScalingPlanResources where

instance ToHeaders DescribeScalingPlanResources where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AnyScaleScalingPlannerFrontendService.DescribeScalingPlanResources"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeScalingPlanResources where
        toJSON DescribeScalingPlanResources'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _dsprNextToken,
                  ("MaxResults" .=) <$> _dsprMaxResults,
                  Just ("ScalingPlanName" .= _dsprScalingPlanName),
                  Just
                    ("ScalingPlanVersion" .= _dsprScalingPlanVersion)])

instance ToPath DescribeScalingPlanResources where
        toPath = const "/"

instance ToQuery DescribeScalingPlanResources where
        toQuery = const mempty

-- | /See:/ 'describeScalingPlanResourcesResponse' smart constructor.
data DescribeScalingPlanResourcesResponse = DescribeScalingPlanResourcesResponse'
  { _dsprrsNextToken            :: !(Maybe Text)
  , _dsprrsScalingPlanResources :: !(Maybe [ScalingPlanResource])
  , _dsprrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeScalingPlanResourcesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsprrsNextToken' - The token required to get the next set of results. This value is @null@ if there are no more results to return.
--
-- * 'dsprrsScalingPlanResources' - Information about the scalable resources.
--
-- * 'dsprrsResponseStatus' - -- | The response status code.
describeScalingPlanResourcesResponse
    :: Int -- ^ 'dsprrsResponseStatus'
    -> DescribeScalingPlanResourcesResponse
describeScalingPlanResourcesResponse pResponseStatus_ =
  DescribeScalingPlanResourcesResponse'
    { _dsprrsNextToken = Nothing
    , _dsprrsScalingPlanResources = Nothing
    , _dsprrsResponseStatus = pResponseStatus_
    }


-- | The token required to get the next set of results. This value is @null@ if there are no more results to return.
dsprrsNextToken :: Lens' DescribeScalingPlanResourcesResponse (Maybe Text)
dsprrsNextToken = lens _dsprrsNextToken (\ s a -> s{_dsprrsNextToken = a})

-- | Information about the scalable resources.
dsprrsScalingPlanResources :: Lens' DescribeScalingPlanResourcesResponse [ScalingPlanResource]
dsprrsScalingPlanResources = lens _dsprrsScalingPlanResources (\ s a -> s{_dsprrsScalingPlanResources = a}) . _Default . _Coerce

-- | -- | The response status code.
dsprrsResponseStatus :: Lens' DescribeScalingPlanResourcesResponse Int
dsprrsResponseStatus = lens _dsprrsResponseStatus (\ s a -> s{_dsprrsResponseStatus = a})

instance NFData DescribeScalingPlanResourcesResponse
         where
