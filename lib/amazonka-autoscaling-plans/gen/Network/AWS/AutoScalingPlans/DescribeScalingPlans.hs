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
-- Module      : Network.AWS.AutoScalingPlans.DescribeScalingPlans
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified scaling plans or all of your scaling plans.
--
--
module Network.AWS.AutoScalingPlans.DescribeScalingPlans
    (
    -- * Creating a Request
      describeScalingPlans
    , DescribeScalingPlans
    -- * Request Lenses
    , dScalingPlanVersion
    , dScalingPlanNames
    , dNextToken
    , dApplicationSources
    , dMaxResults

    -- * Destructuring the Response
    , describeScalingPlansResponse
    , DescribeScalingPlansResponse
    -- * Response Lenses
    , drsScalingPlans
    , drsNextToken
    , drsResponseStatus
    ) where

import Network.AWS.AutoScalingPlans.Types
import Network.AWS.AutoScalingPlans.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeScalingPlans' smart constructor.
data DescribeScalingPlans = DescribeScalingPlans'
  { _dScalingPlanVersion :: !(Maybe Integer)
  , _dScalingPlanNames   :: !(Maybe [Text])
  , _dNextToken          :: !(Maybe Text)
  , _dApplicationSources :: !(Maybe [ApplicationSource])
  , _dMaxResults         :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeScalingPlans' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dScalingPlanVersion' - The version of the scaling plan. If you specify a scaling plan version, you must also specify a scaling plan name.
--
-- * 'dScalingPlanNames' - The names of the scaling plans (up to 10). If you specify application sources, you cannot specify scaling plan names.
--
-- * 'dNextToken' - The token for the next set of results.
--
-- * 'dApplicationSources' - The sources for the applications (up to 10). If you specify scaling plan names, you cannot specify application sources.
--
-- * 'dMaxResults' - The maximum number of scalable resources to return. This value can be between 1 and 50. The default value is 50.
describeScalingPlans
    :: DescribeScalingPlans
describeScalingPlans =
  DescribeScalingPlans'
    { _dScalingPlanVersion = Nothing
    , _dScalingPlanNames = Nothing
    , _dNextToken = Nothing
    , _dApplicationSources = Nothing
    , _dMaxResults = Nothing
    }


-- | The version of the scaling plan. If you specify a scaling plan version, you must also specify a scaling plan name.
dScalingPlanVersion :: Lens' DescribeScalingPlans (Maybe Integer)
dScalingPlanVersion = lens _dScalingPlanVersion (\ s a -> s{_dScalingPlanVersion = a})

-- | The names of the scaling plans (up to 10). If you specify application sources, you cannot specify scaling plan names.
dScalingPlanNames :: Lens' DescribeScalingPlans [Text]
dScalingPlanNames = lens _dScalingPlanNames (\ s a -> s{_dScalingPlanNames = a}) . _Default . _Coerce

-- | The token for the next set of results.
dNextToken :: Lens' DescribeScalingPlans (Maybe Text)
dNextToken = lens _dNextToken (\ s a -> s{_dNextToken = a})

-- | The sources for the applications (up to 10). If you specify scaling plan names, you cannot specify application sources.
dApplicationSources :: Lens' DescribeScalingPlans [ApplicationSource]
dApplicationSources = lens _dApplicationSources (\ s a -> s{_dApplicationSources = a}) . _Default . _Coerce

-- | The maximum number of scalable resources to return. This value can be between 1 and 50. The default value is 50.
dMaxResults :: Lens' DescribeScalingPlans (Maybe Int)
dMaxResults = lens _dMaxResults (\ s a -> s{_dMaxResults = a})

instance AWSRequest DescribeScalingPlans where
        type Rs DescribeScalingPlans =
             DescribeScalingPlansResponse
        request = postJSON autoScalingPlans
        response
          = receiveJSON
              (\ s h x ->
                 DescribeScalingPlansResponse' <$>
                   (x .?> "ScalingPlans" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeScalingPlans where

instance NFData DescribeScalingPlans where

instance ToHeaders DescribeScalingPlans where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AnyScaleScalingPlannerFrontendService.DescribeScalingPlans"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeScalingPlans where
        toJSON DescribeScalingPlans'{..}
          = object
              (catMaybes
                 [("ScalingPlanVersion" .=) <$> _dScalingPlanVersion,
                  ("ScalingPlanNames" .=) <$> _dScalingPlanNames,
                  ("NextToken" .=) <$> _dNextToken,
                  ("ApplicationSources" .=) <$> _dApplicationSources,
                  ("MaxResults" .=) <$> _dMaxResults])

instance ToPath DescribeScalingPlans where
        toPath = const "/"

instance ToQuery DescribeScalingPlans where
        toQuery = const mempty

-- | /See:/ 'describeScalingPlansResponse' smart constructor.
data DescribeScalingPlansResponse = DescribeScalingPlansResponse'
  { _drsScalingPlans   :: !(Maybe [ScalingPlan])
  , _drsNextToken      :: !(Maybe Text)
  , _drsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeScalingPlansResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsScalingPlans' - Information about the scaling plans.
--
-- * 'drsNextToken' - The token required to get the next set of results. This value is @null@ if there are no more results to return.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeScalingPlansResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeScalingPlansResponse
describeScalingPlansResponse pResponseStatus_ =
  DescribeScalingPlansResponse'
    { _drsScalingPlans = Nothing
    , _drsNextToken = Nothing
    , _drsResponseStatus = pResponseStatus_
    }


-- | Information about the scaling plans.
drsScalingPlans :: Lens' DescribeScalingPlansResponse [ScalingPlan]
drsScalingPlans = lens _drsScalingPlans (\ s a -> s{_drsScalingPlans = a}) . _Default . _Coerce

-- | The token required to get the next set of results. This value is @null@ if there are no more results to return.
drsNextToken :: Lens' DescribeScalingPlansResponse (Maybe Text)
drsNextToken = lens _drsNextToken (\ s a -> s{_drsNextToken = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeScalingPlansResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DescribeScalingPlansResponse where
