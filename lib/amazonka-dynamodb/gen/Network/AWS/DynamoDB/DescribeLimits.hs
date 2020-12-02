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
-- Module      : Network.AWS.DynamoDB.DescribeLimits
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current provisioned-capacity limits for your AWS account in a region, both for the region as a whole and for any one DynamoDB table that you create there.
--
--
-- When you establish an AWS account, the account has initial limits on the maximum read capacity units and write capacity units that you can provision across all of your DynamoDB tables in a given region. Also, there are per-table limits that apply when you create a table there. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Limits> page in the /Amazon DynamoDB Developer Guide/ .
--
-- Although you can increase these limits by filing a case at <https://console.aws.amazon.com/support/home#/ AWS Support Center> , obtaining the increase is not instantaneous. The @DescribeLimits@ action lets you write code to compare the capacity you are currently using to those limits imposed by your account so that you have enough time to apply for an increase before you hit a limit.
--
-- For example, you could use one of the AWS SDKs to do the following:
--
--     * Call @DescribeLimits@ for a particular region to obtain your current account limits on provisioned capacity there.
--
--     * Create a variable to hold the aggregate read capacity units provisioned for all your tables in that region, and one to hold the aggregate write capacity units. Zero them both.
--
--     * Call @ListTables@ to obtain a list of all your DynamoDB tables.
--
--     * For each table name listed by @ListTables@ , do the following:
--
--     * Call @DescribeTable@ with the table name.
--
--     * Use the data returned by @DescribeTable@ to add the read capacity units and write capacity units provisioned for the table itself to your variables.
--
--     * If the table has one or more global secondary indexes (GSIs), loop over these GSIs and add their provisioned capacity values to your variables as well.
--
--
--
--     * Report the account limits for that region returned by @DescribeLimits@ , along with the total current provisioned capacity levels you have calculated.
--
--
--
-- This will let you see whether you are getting close to your account-level limits.
--
-- The per-table limits apply only when you are creating a new table. They restrict the sum of the provisioned capacity of the new table itself and all its global secondary indexes.
--
-- For existing tables and their GSIs, DynamoDB will not let you increase provisioned capacity extremely rapidly, but the only upper limit that applies is that the aggregate provisioned capacity over all your tables and GSIs cannot exceed either of the per-account limits.
--
-- The @DescribeLimits@ Request element has no content.
--
module Network.AWS.DynamoDB.DescribeLimits
    (
    -- * Creating a Request
      describeLimits
    , DescribeLimits

    -- * Destructuring the Response
    , describeLimitsResponse
    , DescribeLimitsResponse
    -- * Response Lenses
    , dlrsTableMaxWriteCapacityUnits
    , dlrsTableMaxReadCapacityUnits
    , dlrsAccountMaxWriteCapacityUnits
    , dlrsAccountMaxReadCapacityUnits
    , dlrsResponseStatus
    ) where

import Network.AWS.DynamoDB.Types
import Network.AWS.DynamoDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @DescribeLimits@ operation. Has no content.
--
--
--
-- /See:/ 'describeLimits' smart constructor.
data DescribeLimits =
  DescribeLimits'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLimits' with the minimum fields required to make a request.
--
describeLimits
    :: DescribeLimits
describeLimits = DescribeLimits'


instance AWSRequest DescribeLimits where
        type Rs DescribeLimits = DescribeLimitsResponse
        request = postJSON dynamoDB
        response
          = receiveJSON
              (\ s h x ->
                 DescribeLimitsResponse' <$>
                   (x .?> "TableMaxWriteCapacityUnits") <*>
                     (x .?> "TableMaxReadCapacityUnits")
                     <*> (x .?> "AccountMaxWriteCapacityUnits")
                     <*> (x .?> "AccountMaxReadCapacityUnits")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeLimits where

instance NFData DescribeLimits where

instance ToHeaders DescribeLimits where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDB_20120810.DescribeLimits" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON DescribeLimits where
        toJSON = const (Object mempty)

instance ToPath DescribeLimits where
        toPath = const "/"

instance ToQuery DescribeLimits where
        toQuery = const mempty

-- | Represents the output of a @DescribeLimits@ operation.
--
--
--
-- /See:/ 'describeLimitsResponse' smart constructor.
data DescribeLimitsResponse = DescribeLimitsResponse'
  { _dlrsTableMaxWriteCapacityUnits   :: !(Maybe Nat)
  , _dlrsTableMaxReadCapacityUnits    :: !(Maybe Nat)
  , _dlrsAccountMaxWriteCapacityUnits :: !(Maybe Nat)
  , _dlrsAccountMaxReadCapacityUnits  :: !(Maybe Nat)
  , _dlrsResponseStatus               :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLimitsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlrsTableMaxWriteCapacityUnits' - The maximum write capacity units that your account allows you to provision for a new table that you are creating in this region, including the write capacity units provisioned for its global secondary indexes (GSIs).
--
-- * 'dlrsTableMaxReadCapacityUnits' - The maximum read capacity units that your account allows you to provision for a new table that you are creating in this region, including the read capacity units provisioned for its global secondary indexes (GSIs).
--
-- * 'dlrsAccountMaxWriteCapacityUnits' - The maximum total write capacity units that your account allows you to provision across all of your tables in this region.
--
-- * 'dlrsAccountMaxReadCapacityUnits' - The maximum total read capacity units that your account allows you to provision across all of your tables in this region.
--
-- * 'dlrsResponseStatus' - -- | The response status code.
describeLimitsResponse
    :: Int -- ^ 'dlrsResponseStatus'
    -> DescribeLimitsResponse
describeLimitsResponse pResponseStatus_ =
  DescribeLimitsResponse'
    { _dlrsTableMaxWriteCapacityUnits = Nothing
    , _dlrsTableMaxReadCapacityUnits = Nothing
    , _dlrsAccountMaxWriteCapacityUnits = Nothing
    , _dlrsAccountMaxReadCapacityUnits = Nothing
    , _dlrsResponseStatus = pResponseStatus_
    }


-- | The maximum write capacity units that your account allows you to provision for a new table that you are creating in this region, including the write capacity units provisioned for its global secondary indexes (GSIs).
dlrsTableMaxWriteCapacityUnits :: Lens' DescribeLimitsResponse (Maybe Natural)
dlrsTableMaxWriteCapacityUnits = lens _dlrsTableMaxWriteCapacityUnits (\ s a -> s{_dlrsTableMaxWriteCapacityUnits = a}) . mapping _Nat

-- | The maximum read capacity units that your account allows you to provision for a new table that you are creating in this region, including the read capacity units provisioned for its global secondary indexes (GSIs).
dlrsTableMaxReadCapacityUnits :: Lens' DescribeLimitsResponse (Maybe Natural)
dlrsTableMaxReadCapacityUnits = lens _dlrsTableMaxReadCapacityUnits (\ s a -> s{_dlrsTableMaxReadCapacityUnits = a}) . mapping _Nat

-- | The maximum total write capacity units that your account allows you to provision across all of your tables in this region.
dlrsAccountMaxWriteCapacityUnits :: Lens' DescribeLimitsResponse (Maybe Natural)
dlrsAccountMaxWriteCapacityUnits = lens _dlrsAccountMaxWriteCapacityUnits (\ s a -> s{_dlrsAccountMaxWriteCapacityUnits = a}) . mapping _Nat

-- | The maximum total read capacity units that your account allows you to provision across all of your tables in this region.
dlrsAccountMaxReadCapacityUnits :: Lens' DescribeLimitsResponse (Maybe Natural)
dlrsAccountMaxReadCapacityUnits = lens _dlrsAccountMaxReadCapacityUnits (\ s a -> s{_dlrsAccountMaxReadCapacityUnits = a}) . mapping _Nat

-- | -- | The response status code.
dlrsResponseStatus :: Lens' DescribeLimitsResponse Int
dlrsResponseStatus = lens _dlrsResponseStatus (\ s a -> s{_dlrsResponseStatus = a})

instance NFData DescribeLimitsResponse where
