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
-- Module      : Network.AWS.Config.DescribeComplianceByResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Indicates whether the specified AWS resources are compliant. If a resource is noncompliant, this action returns the number of AWS Config rules that the resource does not comply with.
--
--
-- A resource is compliant if it complies with all the AWS Config rules that evaluate it. It is noncompliant if it does not comply with one or more of these rules.
--
-- If AWS Config has no current evaluation results for the resource, it returns @INSUFFICIENT_DATA@ . This result might indicate one of the following conditions about the rules that evaluate the resource:
--
--     * AWS Config has never invoked an evaluation for the rule. To check whether it has, use the @DescribeConfigRuleEvaluationStatus@ action to get the @LastSuccessfulInvocationTime@ and @LastFailedInvocationTime@ .
--
--     * The rule's AWS Lambda function is failing to send evaluation results to AWS Config. Verify that the role that you assigned to your configuration recorder includes the @config:PutEvaluations@ permission. If the rule is a custom rule, verify that the AWS Lambda execution role includes the @config:PutEvaluations@ permission.
--
--     * The rule's AWS Lambda function has returned @NOT_APPLICABLE@ for all evaluation results. This can occur if the resources were deleted or removed from the rule's scope.
--
--
--
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeComplianceByResource
    (
    -- * Creating a Request
      describeComplianceByResource
    , DescribeComplianceByResource
    -- * Request Lenses
    , dcbrResourceId
    , dcbrResourceType
    , dcbrComplianceTypes
    , dcbrNextToken
    , dcbrLimit

    -- * Destructuring the Response
    , describeComplianceByResourceResponse
    , DescribeComplianceByResourceResponse
    -- * Response Lenses
    , dcbrrsComplianceByResources
    , dcbrrsNextToken
    , dcbrrsResponseStatus
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeComplianceByResource' smart constructor.
data DescribeComplianceByResource = DescribeComplianceByResource'
  { _dcbrResourceId      :: !(Maybe Text)
  , _dcbrResourceType    :: !(Maybe Text)
  , _dcbrComplianceTypes :: !(Maybe [ComplianceType])
  , _dcbrNextToken       :: !(Maybe Text)
  , _dcbrLimit           :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeComplianceByResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcbrResourceId' - The ID of the AWS resource for which you want compliance information. You can specify only one resource ID. If you specify a resource ID, you must also specify a type for @ResourceType@ .
--
-- * 'dcbrResourceType' - The types of AWS resources for which you want compliance information (for example, @AWS::EC2::Instance@ ). For this action, you can specify that the resource type is an AWS account by specifying @AWS::::Account@ .
--
-- * 'dcbrComplianceTypes' - Filters the results by compliance. The allowed values are @COMPLIANT@ and @NON_COMPLIANT@ .
--
-- * 'dcbrNextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'dcbrLimit' - The maximum number of evaluation results returned on each page. The default is 10. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
describeComplianceByResource
    :: DescribeComplianceByResource
describeComplianceByResource =
  DescribeComplianceByResource'
    { _dcbrResourceId = Nothing
    , _dcbrResourceType = Nothing
    , _dcbrComplianceTypes = Nothing
    , _dcbrNextToken = Nothing
    , _dcbrLimit = Nothing
    }


-- | The ID of the AWS resource for which you want compliance information. You can specify only one resource ID. If you specify a resource ID, you must also specify a type for @ResourceType@ .
dcbrResourceId :: Lens' DescribeComplianceByResource (Maybe Text)
dcbrResourceId = lens _dcbrResourceId (\ s a -> s{_dcbrResourceId = a})

-- | The types of AWS resources for which you want compliance information (for example, @AWS::EC2::Instance@ ). For this action, you can specify that the resource type is an AWS account by specifying @AWS::::Account@ .
dcbrResourceType :: Lens' DescribeComplianceByResource (Maybe Text)
dcbrResourceType = lens _dcbrResourceType (\ s a -> s{_dcbrResourceType = a})

-- | Filters the results by compliance. The allowed values are @COMPLIANT@ and @NON_COMPLIANT@ .
dcbrComplianceTypes :: Lens' DescribeComplianceByResource [ComplianceType]
dcbrComplianceTypes = lens _dcbrComplianceTypes (\ s a -> s{_dcbrComplianceTypes = a}) . _Default . _Coerce

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
dcbrNextToken :: Lens' DescribeComplianceByResource (Maybe Text)
dcbrNextToken = lens _dcbrNextToken (\ s a -> s{_dcbrNextToken = a})

-- | The maximum number of evaluation results returned on each page. The default is 10. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
dcbrLimit :: Lens' DescribeComplianceByResource (Maybe Natural)
dcbrLimit = lens _dcbrLimit (\ s a -> s{_dcbrLimit = a}) . mapping _Nat

instance AWSPager DescribeComplianceByResource where
        page rq rs
          | stop (rs ^. dcbrrsNextToken) = Nothing
          | stop (rs ^. dcbrrsComplianceByResources) = Nothing
          | otherwise =
            Just $ rq & dcbrNextToken .~ rs ^. dcbrrsNextToken

instance AWSRequest DescribeComplianceByResource
         where
        type Rs DescribeComplianceByResource =
             DescribeComplianceByResourceResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 DescribeComplianceByResourceResponse' <$>
                   (x .?> "ComplianceByResources" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeComplianceByResource where

instance NFData DescribeComplianceByResource where

instance ToHeaders DescribeComplianceByResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.DescribeComplianceByResource"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeComplianceByResource where
        toJSON DescribeComplianceByResource'{..}
          = object
              (catMaybes
                 [("ResourceId" .=) <$> _dcbrResourceId,
                  ("ResourceType" .=) <$> _dcbrResourceType,
                  ("ComplianceTypes" .=) <$> _dcbrComplianceTypes,
                  ("NextToken" .=) <$> _dcbrNextToken,
                  ("Limit" .=) <$> _dcbrLimit])

instance ToPath DescribeComplianceByResource where
        toPath = const "/"

instance ToQuery DescribeComplianceByResource where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'describeComplianceByResourceResponse' smart constructor.
data DescribeComplianceByResourceResponse = DescribeComplianceByResourceResponse'
  { _dcbrrsComplianceByResources :: !(Maybe [ComplianceByResource])
  , _dcbrrsNextToken             :: !(Maybe Text)
  , _dcbrrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeComplianceByResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcbrrsComplianceByResources' - Indicates whether the specified AWS resource complies with all of the AWS Config rules that evaluate it.
--
-- * 'dcbrrsNextToken' - The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- * 'dcbrrsResponseStatus' - -- | The response status code.
describeComplianceByResourceResponse
    :: Int -- ^ 'dcbrrsResponseStatus'
    -> DescribeComplianceByResourceResponse
describeComplianceByResourceResponse pResponseStatus_ =
  DescribeComplianceByResourceResponse'
    { _dcbrrsComplianceByResources = Nothing
    , _dcbrrsNextToken = Nothing
    , _dcbrrsResponseStatus = pResponseStatus_
    }


-- | Indicates whether the specified AWS resource complies with all of the AWS Config rules that evaluate it.
dcbrrsComplianceByResources :: Lens' DescribeComplianceByResourceResponse [ComplianceByResource]
dcbrrsComplianceByResources = lens _dcbrrsComplianceByResources (\ s a -> s{_dcbrrsComplianceByResources = a}) . _Default . _Coerce

-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
dcbrrsNextToken :: Lens' DescribeComplianceByResourceResponse (Maybe Text)
dcbrrsNextToken = lens _dcbrrsNextToken (\ s a -> s{_dcbrrsNextToken = a})

-- | -- | The response status code.
dcbrrsResponseStatus :: Lens' DescribeComplianceByResourceResponse Int
dcbrrsResponseStatus = lens _dcbrrsResponseStatus (\ s a -> s{_dcbrrsResponseStatus = a})

instance NFData DescribeComplianceByResourceResponse
         where
