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
-- * 'gdvContext' - The context for the call to @GetDimensionValues@ . This can be @RESERVATIONS@ or @COST_AND_USAGE@ . The default value is @COST_AND_USAGE@ . If the context is set to @RESERVATIONS@ , the resulting dimension values can be used in the @GetReservationUtilization@ operation. If the context is set to @COST_AND_USAGE@ , the resulting dimension values can be used in the @GetCostAndUsage@ operation. If you set the context to @COST_AND_USAGE@ , you can use the following dimensions for searching:     * AZ - The Availability Zone. An example is @us-east-1a@ .     * DATABASE_ENGINE - The Amazon Relational Database Service database. Examples are Aurora or MySQL.     * INSTANCE_TYPE - The type of Amazon EC2 instance. An example is @m4.xlarge@ .     * LEGAL_ENTITY_NAME - The name of the organization that sells you AWS services, such as Amazon Web Services.     * LINKED_ACCOUNT - The description in the attribute map that includes the full name of the member account. The value field contains the AWS ID of the member account.     * OPERATING_SYSTEM - The operating system. Examples are Windows or Linux.     * OPERATION - The action performed. Examples include @RunInstance@ and @CreateBucket@ .     * PLATFORM - The Amazon EC2 operating system. Examples are Windows or Linux.     * PURCHASE_TYPE - The reservation type of the purchase to which this usage is related. Examples include On-Demand Instances and Standard Reserved Instances.     * SERVICE - The AWS service such as Amazon DynamoDB.     * USAGE_TYPE - The type of usage. An example is DataTransfer-In-Bytes. The response for the @GetDimensionValues@ operation includes a unit attribute. Examples include GB and Hrs.     * USAGE_TYPE_GROUP - The grouping of common usage types. An example is Amazon EC2: CloudWatch
