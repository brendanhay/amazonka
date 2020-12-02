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
-- Module      : Network.AWS.CostExplorer.GetDimensionValues
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all available filter values for a specified filter over a period of time. You can search the dimension values for an arbitrary string.
--
--
module Network.AWS.CostExplorer.GetDimensionValues
    (
    -- * Creating a Request
      getDimensionValues
    , GetDimensionValues
    -- * Request Lenses
    , gdvNextPageToken
    , gdvContext
    , gdvSearchString
    , gdvTimePeriod
    , gdvDimension

    -- * Destructuring the Response
    , getDimensionValuesResponse
    , GetDimensionValuesResponse
    -- * Response Lenses
    , gdvrsNextPageToken
    , gdvrsResponseStatus
    , gdvrsDimensionValues
    , gdvrsReturnSize
    , gdvrsTotalSize
    ) where

import Network.AWS.CostExplorer.Types
import Network.AWS.CostExplorer.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDimensionValues' smart constructor.
data GetDimensionValues = GetDimensionValues'
  { _gdvNextPageToken :: !(Maybe Text)
  , _gdvContext       :: !(Maybe Context)
  , _gdvSearchString  :: !(Maybe Text)
  , _gdvTimePeriod    :: !DateInterval
  , _gdvDimension     :: !Dimension
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDimensionValues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdvNextPageToken' - The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- * 'gdvContext' - The context for the call to @GetDimensionValues@ . This can be @RESERVATIONS@ or @COST_AND_USAGE@ . The default value is @COST_AND_USAGE@ . If the context is set to @RESERVATIONS@ , the resulting dimension values can be used in the @GetReservationUtilization@ operation. If the context is set to @COST_AND_USAGE@ the resulting dimension values can be used in the @GetCostAndUsage@ operation. If you set the context to @COST_AND_USAGE@ , you can use the following dimensions for searching:     * AZ - The Availability Zone. An example is @us-east-1a@ .     * DATABASE_ENGINE - The Amazon Relational Database Service database. Examples are Aurora or MySQL.     * INSTANCE_TYPE - The type of EC2 instance. An example is @m4.xlarge@ .     * LEGAL_ENTITY_NAME - The name of the organization that sells you AWS services, such as Amazon Web Services.     * LINKED_ACCOUNT - The description in the attribute map that includes the full name of the member account. The value field contains the AWS ID of the member account.     * OPERATING_SYSTEM - The operating system. Examples are Windows or Linux.     * OPERATION - The action performed. Examples include @RunInstance@ and @CreateBucket@ .     * PLATFORM - The EC2 operating system. Examples are Windows or Linux.     * PURCHASE_TYPE - The reservation type of the purchase to which this usage is related. Examples include On-Demand Instances and Standard Reserved Instances.     * SERVICE - The AWS service such as Amazon DynamoDB.     * USAGE_TYPE - The type of usage. An example is DataTransfer-In-Bytes. The response for the @GetDimensionValues@ operation includes a unit attribute. Examples include GB and Hrs.     * USAGE_TYPE_GROUP - The grouping of common usage types. An example is EC2: CloudWatch – Alarms. The response for this operation includes a unit attribute.     * RECORD_TYPE - The different types of charges such as RI fees, usage costs, tax refunds, and credits. If you set the context to @RESERVATIONS@ , you can use the following dimensions for searching:     * AZ - The Availability Zone. An example is @us-east-1a@ .     * CACHE_ENGINE - The Amazon ElastiCache operating system. Examples are Windows or Linux.     * DEPLOYMENT_OPTION - The scope of Amazon Relational Database Service deployments. Valid values are @SingleAZ@ and @MultiAZ@ .     * INSTANCE_TYPE - The type of EC2 instance. An example is @m4.xlarge@ .     * LINKED_ACCOUNT - The description in the attribute map that includes the full name of the member account. The value field contains the AWS ID of the member account.     * PLATFORM - The EC2 operating system. Examples are Windows or Linux.     * REGION - The AWS Region.     * SCOPE (Utilization only) - The scope of a Reserved Instance (RI). Values are regional or a single Availability Zone.     * TAG (Coverage only) - The tags that are associated with a Reserved Instance (RI).     * TENANCY - The tenancy of a resource. Examples are shared or dedicated.
--
-- * 'gdvSearchString' - The value that you want to search the filter values for.
--
-- * 'gdvTimePeriod' - The start and end dates for retrieving the dimension values. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
--
-- * 'gdvDimension' - The name of the dimension. Each @Dimension@ is available for different a @Context@ . For more information, see @Context@ .
getDimensionValues
    :: DateInterval -- ^ 'gdvTimePeriod'
    -> Dimension -- ^ 'gdvDimension'
    -> GetDimensionValues
getDimensionValues pTimePeriod_ pDimension_ =
  GetDimensionValues'
    { _gdvNextPageToken = Nothing
    , _gdvContext = Nothing
    , _gdvSearchString = Nothing
    , _gdvTimePeriod = pTimePeriod_
    , _gdvDimension = pDimension_
    }


-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
gdvNextPageToken :: Lens' GetDimensionValues (Maybe Text)
gdvNextPageToken = lens _gdvNextPageToken (\ s a -> s{_gdvNextPageToken = a})

-- | The context for the call to @GetDimensionValues@ . This can be @RESERVATIONS@ or @COST_AND_USAGE@ . The default value is @COST_AND_USAGE@ . If the context is set to @RESERVATIONS@ , the resulting dimension values can be used in the @GetReservationUtilization@ operation. If the context is set to @COST_AND_USAGE@ the resulting dimension values can be used in the @GetCostAndUsage@ operation. If you set the context to @COST_AND_USAGE@ , you can use the following dimensions for searching:     * AZ - The Availability Zone. An example is @us-east-1a@ .     * DATABASE_ENGINE - The Amazon Relational Database Service database. Examples are Aurora or MySQL.     * INSTANCE_TYPE - The type of EC2 instance. An example is @m4.xlarge@ .     * LEGAL_ENTITY_NAME - The name of the organization that sells you AWS services, such as Amazon Web Services.     * LINKED_ACCOUNT - The description in the attribute map that includes the full name of the member account. The value field contains the AWS ID of the member account.     * OPERATING_SYSTEM - The operating system. Examples are Windows or Linux.     * OPERATION - The action performed. Examples include @RunInstance@ and @CreateBucket@ .     * PLATFORM - The EC2 operating system. Examples are Windows or Linux.     * PURCHASE_TYPE - The reservation type of the purchase to which this usage is related. Examples include On-Demand Instances and Standard Reserved Instances.     * SERVICE - The AWS service such as Amazon DynamoDB.     * USAGE_TYPE - The type of usage. An example is DataTransfer-In-Bytes. The response for the @GetDimensionValues@ operation includes a unit attribute. Examples include GB and Hrs.     * USAGE_TYPE_GROUP - The grouping of common usage types. An example is EC2: CloudWatch – Alarms. The response for this operation includes a unit attribute.     * RECORD_TYPE - The different types of charges such as RI fees, usage costs, tax refunds, and credits. If you set the context to @RESERVATIONS@ , you can use the following dimensions for searching:     * AZ - The Availability Zone. An example is @us-east-1a@ .     * CACHE_ENGINE - The Amazon ElastiCache operating system. Examples are Windows or Linux.     * DEPLOYMENT_OPTION - The scope of Amazon Relational Database Service deployments. Valid values are @SingleAZ@ and @MultiAZ@ .     * INSTANCE_TYPE - The type of EC2 instance. An example is @m4.xlarge@ .     * LINKED_ACCOUNT - The description in the attribute map that includes the full name of the member account. The value field contains the AWS ID of the member account.     * PLATFORM - The EC2 operating system. Examples are Windows or Linux.     * REGION - The AWS Region.     * SCOPE (Utilization only) - The scope of a Reserved Instance (RI). Values are regional or a single Availability Zone.     * TAG (Coverage only) - The tags that are associated with a Reserved Instance (RI).     * TENANCY - The tenancy of a resource. Examples are shared or dedicated.
gdvContext :: Lens' GetDimensionValues (Maybe Context)
gdvContext = lens _gdvContext (\ s a -> s{_gdvContext = a})

-- | The value that you want to search the filter values for.
gdvSearchString :: Lens' GetDimensionValues (Maybe Text)
gdvSearchString = lens _gdvSearchString (\ s a -> s{_gdvSearchString = a})

-- | The start and end dates for retrieving the dimension values. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
gdvTimePeriod :: Lens' GetDimensionValues DateInterval
gdvTimePeriod = lens _gdvTimePeriod (\ s a -> s{_gdvTimePeriod = a})

-- | The name of the dimension. Each @Dimension@ is available for different a @Context@ . For more information, see @Context@ .
gdvDimension :: Lens' GetDimensionValues Dimension
gdvDimension = lens _gdvDimension (\ s a -> s{_gdvDimension = a})

instance AWSRequest GetDimensionValues where
        type Rs GetDimensionValues =
             GetDimensionValuesResponse
        request = postJSON costExplorer
        response
          = receiveJSON
              (\ s h x ->
                 GetDimensionValuesResponse' <$>
                   (x .?> "NextPageToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "DimensionValues" .!@ mempty)
                     <*> (x .:> "ReturnSize")
                     <*> (x .:> "TotalSize"))

instance Hashable GetDimensionValues where

instance NFData GetDimensionValues where

instance ToHeaders GetDimensionValues where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSInsightsIndexService.GetDimensionValues" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDimensionValues where
        toJSON GetDimensionValues'{..}
          = object
              (catMaybes
                 [("NextPageToken" .=) <$> _gdvNextPageToken,
                  ("Context" .=) <$> _gdvContext,
                  ("SearchString" .=) <$> _gdvSearchString,
                  Just ("TimePeriod" .= _gdvTimePeriod),
                  Just ("Dimension" .= _gdvDimension)])

instance ToPath GetDimensionValues where
        toPath = const "/"

instance ToQuery GetDimensionValues where
        toQuery = const mempty

-- | /See:/ 'getDimensionValuesResponse' smart constructor.
data GetDimensionValuesResponse = GetDimensionValuesResponse'
  { _gdvrsNextPageToken   :: !(Maybe Text)
  , _gdvrsResponseStatus  :: !Int
  , _gdvrsDimensionValues :: ![DimensionValuesWithAttributes]
  , _gdvrsReturnSize      :: !Int
  , _gdvrsTotalSize       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDimensionValuesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdvrsNextPageToken' - The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- * 'gdvrsResponseStatus' - -- | The response status code.
--
-- * 'gdvrsDimensionValues' - The filters that you used to filter your request. Some dimensions are available only for a specific context: If you set the context to @COST_AND_USAGE@ , you can use the following dimensions for searching:     * AZ - The Availability Zone. An example is @us-east-1a@ .     * DATABASE_ENGINE - The Amazon Relational Database Service database. Examples are Aurora or MySQL.     * INSTANCE_TYPE - The type of EC2 instance. An example is @m4.xlarge@ .     * LEGAL_ENTITY_NAME - The name of the organization that sells you AWS services, such as Amazon Web Services.     * LINKED_ACCOUNT - The description in the attribute map that includes the full name of the member account. The value field contains the AWS ID of the member account.     * OPERATING_SYSTEM - The operating system. Examples are Windows or Linux.     * OPERATION - The action performed. Examples include @RunInstance@ and @CreateBucket@ .     * PLATFORM - The EC2 operating system. Examples are Windows or Linux.     * PURCHASE_TYPE - The reservation type of the purchase to which this usage is related. Examples include On-Demand Instances and Standard Reserved Instances.     * SERVICE - The AWS service such as Amazon DynamoDB.     * USAGE_TYPE - The type of usage. An example is DataTransfer-In-Bytes. The response for the @GetDimensionValues@ operation includes a unit attribute. Examples include GB and Hrs.     * USAGE_TYPE_GROUP - The grouping of common usage types. An example is EC2: CloudWatch – Alarms. The response for this operation includes a unit attribute.     * RECORD_TYPE - The different types of charges such as RI fees, usage costs, tax refunds, and credits. If you set the context to @RESERVATIONS@ , you can use the following dimensions for searching:     * AZ - The Availability Zone. An example is @us-east-1a@ .     * CACHE_ENGINE - The Amazon ElastiCache operating system. Examples are Windows or Linux.     * DEPLOYMENT_OPTION - The scope of Amazon Relational Database Service deployments. Valid values are @SingleAZ@ and @MultiAZ@ .     * INSTANCE_TYPE - The type of EC2 instance. An example is @m4.xlarge@ .     * LINKED_ACCOUNT - The description in the attribute map that includes the full name of the member account. The value field contains the AWS ID of the member account.     * PLATFORM - The EC2 operating system. Examples are Windows or Linux.     * REGION - The AWS Region.     * SCOPE (Utilization only) - The scope of a Reserved Instance (RI). Values are regional or a single Availability Zone.     * TAG (Coverage only) - The tags that are associated with a Reserved Instance (RI).     * TENANCY - The tenancy of a resource. Examples are shared or dedicated.
--
-- * 'gdvrsReturnSize' - The number of results that AWS returned at one time.
--
-- * 'gdvrsTotalSize' - The total number of search results.
getDimensionValuesResponse
    :: Int -- ^ 'gdvrsResponseStatus'
    -> Int -- ^ 'gdvrsReturnSize'
    -> Int -- ^ 'gdvrsTotalSize'
    -> GetDimensionValuesResponse
getDimensionValuesResponse pResponseStatus_ pReturnSize_ pTotalSize_ =
  GetDimensionValuesResponse'
    { _gdvrsNextPageToken = Nothing
    , _gdvrsResponseStatus = pResponseStatus_
    , _gdvrsDimensionValues = mempty
    , _gdvrsReturnSize = pReturnSize_
    , _gdvrsTotalSize = pTotalSize_
    }


-- | The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
gdvrsNextPageToken :: Lens' GetDimensionValuesResponse (Maybe Text)
gdvrsNextPageToken = lens _gdvrsNextPageToken (\ s a -> s{_gdvrsNextPageToken = a})

-- | -- | The response status code.
gdvrsResponseStatus :: Lens' GetDimensionValuesResponse Int
gdvrsResponseStatus = lens _gdvrsResponseStatus (\ s a -> s{_gdvrsResponseStatus = a})

-- | The filters that you used to filter your request. Some dimensions are available only for a specific context: If you set the context to @COST_AND_USAGE@ , you can use the following dimensions for searching:     * AZ - The Availability Zone. An example is @us-east-1a@ .     * DATABASE_ENGINE - The Amazon Relational Database Service database. Examples are Aurora or MySQL.     * INSTANCE_TYPE - The type of EC2 instance. An example is @m4.xlarge@ .     * LEGAL_ENTITY_NAME - The name of the organization that sells you AWS services, such as Amazon Web Services.     * LINKED_ACCOUNT - The description in the attribute map that includes the full name of the member account. The value field contains the AWS ID of the member account.     * OPERATING_SYSTEM - The operating system. Examples are Windows or Linux.     * OPERATION - The action performed. Examples include @RunInstance@ and @CreateBucket@ .     * PLATFORM - The EC2 operating system. Examples are Windows or Linux.     * PURCHASE_TYPE - The reservation type of the purchase to which this usage is related. Examples include On-Demand Instances and Standard Reserved Instances.     * SERVICE - The AWS service such as Amazon DynamoDB.     * USAGE_TYPE - The type of usage. An example is DataTransfer-In-Bytes. The response for the @GetDimensionValues@ operation includes a unit attribute. Examples include GB and Hrs.     * USAGE_TYPE_GROUP - The grouping of common usage types. An example is EC2: CloudWatch – Alarms. The response for this operation includes a unit attribute.     * RECORD_TYPE - The different types of charges such as RI fees, usage costs, tax refunds, and credits. If you set the context to @RESERVATIONS@ , you can use the following dimensions for searching:     * AZ - The Availability Zone. An example is @us-east-1a@ .     * CACHE_ENGINE - The Amazon ElastiCache operating system. Examples are Windows or Linux.     * DEPLOYMENT_OPTION - The scope of Amazon Relational Database Service deployments. Valid values are @SingleAZ@ and @MultiAZ@ .     * INSTANCE_TYPE - The type of EC2 instance. An example is @m4.xlarge@ .     * LINKED_ACCOUNT - The description in the attribute map that includes the full name of the member account. The value field contains the AWS ID of the member account.     * PLATFORM - The EC2 operating system. Examples are Windows or Linux.     * REGION - The AWS Region.     * SCOPE (Utilization only) - The scope of a Reserved Instance (RI). Values are regional or a single Availability Zone.     * TAG (Coverage only) - The tags that are associated with a Reserved Instance (RI).     * TENANCY - The tenancy of a resource. Examples are shared or dedicated.
gdvrsDimensionValues :: Lens' GetDimensionValuesResponse [DimensionValuesWithAttributes]
gdvrsDimensionValues = lens _gdvrsDimensionValues (\ s a -> s{_gdvrsDimensionValues = a}) . _Coerce

-- | The number of results that AWS returned at one time.
gdvrsReturnSize :: Lens' GetDimensionValuesResponse Int
gdvrsReturnSize = lens _gdvrsReturnSize (\ s a -> s{_gdvrsReturnSize = a})

-- | The total number of search results.
gdvrsTotalSize :: Lens' GetDimensionValuesResponse Int
gdvrsTotalSize = lens _gdvrsTotalSize (\ s a -> s{_gdvrsTotalSize = a})

instance NFData GetDimensionValuesResponse where
