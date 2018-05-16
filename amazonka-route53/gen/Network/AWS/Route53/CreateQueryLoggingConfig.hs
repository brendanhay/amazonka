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
-- Module      : Network.AWS.Route53.CreateQueryLoggingConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a configuration for DNS query logging. After you create a query logging configuration, Amazon Route 53 begins to publish log data to an Amazon CloudWatch Logs log group.
--
--
-- DNS query logs contain information about the queries that Amazon Route 53 receives for a specified public hosted zone, such as the following:
--
--     * Amazon Route 53 edge location that responded to the DNS query
--
--     * Domain or subdomain that was requested
--
--     * DNS record type, such as A or AAAA
--
--     * DNS response code, such as @NoError@ or @ServFail@
--
--
--
--     * Log Group and Resource Policy    * Before you create a query logging configuration, perform the following operations.
--
--     * Create a CloudWatch Logs log group, and make note of the ARN, which you specify when you create a query logging configuration. Note the following:
--
--     * You must create the log group in the us-east-1 region.
--
--     * You must use the same AWS account to create the log group and the hosted zone that you want to configure query logging for.
--
--     * When you create log groups for query logging, we recommend that you use a consistent prefix, for example:
--
-- @/aws/route53//hosted zone name/ @
--
-- In the next step, you'll create a resource policy, which controls access to one or more log groups and the associated AWS resources, such as Amazon Route 53 hosted zones. There's a limit on the number of resource policies that you can create, so we recommend that you use a consistent prefix so you can use the same resource policy for all the log groups that you create for query logging.
--
--
--
--     * Create a CloudWatch Logs resource policy, and give it the permissions that Amazon Route 53 needs to create log streams and to send query logs to log streams. For the value of @Resource@ , specify the ARN for the log group that you created in the previous step. To use the same resource policy for all the CloudWatch Logs log groups that you created for query logging configurations, replace the hosted zone name with @*@ , for example:
--
-- @arn:aws:logs:us-east-1:123412341234:log-group:/aws/route53/*@
--
--
--
--     * Log Streams and Edge Locations    * When Amazon Route 53 finishes creating the configuration for DNS query logging, it does the following:
--
--     * Creates a log stream for an edge location the first time that the edge location responds to DNS queries for the specified hosted zone. That log stream is used to log all queries that Amazon Route 53 responds to for that edge location.
--
--     * Begins to send query logs to the applicable log stream.
--
--
--
-- The name of each log stream is in the following format:
--
-- @/hosted zone ID/ //edge location code/ @
--
-- The edge location code is a three-letter code and an arbitrarily assigned number, for example, DFW3. The three-letter code typically corresponds with the International Air Transport Association airport code for an airport near the edge location. (These abbreviations might change in the future.) For a list of edge locations, see "The Amazon Route 53 Global Network" on the <http://aws.amazon.com/route53/details/ Amazon Route 53 Product Details> page.
--
--     * Queries That Are Logged    * Query logs contain only the queries that DNS resolvers forward to Amazon Route 53. If a DNS resolver has already cached the response to a query (such as the IP address for a load balancer for example.com), the resolver will continue to return the cached response. It doesn't forward another query to Amazon Route 53 until the TTL for the corresponding resource record set expires. Depending on how many DNS queries are submitted for a resource record set, and depending on the TTL for that resource record set, query logs might contain information about only one query out of every several thousand queries that are submitted to DNS. For more information about how DNS works, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/welcome-dns-service.html Routing Internet Traffic to Your Website or Web Application> in the /Amazon Route 53 Developer Guide/ .
--
--     * Log File Format    * For a list of the values in each query log and the format of each value, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/query-logs.html Logging DNS Queries> in the /Amazon Route 53 Developer Guide/ .
--
--     * Pricing    * For information about charges for query logs, see <http://aws.amazon.com/cloudwatch/pricing/ Amazon CloudWatch Pricing> .
--
--     * How to Stop Logging    * If you want Amazon Route 53 to stop sending query logs to CloudWatch Logs, delete the query logging configuration. For more information, see 'DeleteQueryLoggingConfig' .
--
--
--
module Network.AWS.Route53.CreateQueryLoggingConfig
    (
    -- * Creating a Request
      createQueryLoggingConfig
    , CreateQueryLoggingConfig
    -- * Request Lenses
    , cqlcHostedZoneId
    , cqlcCloudWatchLogsLogGroupARN

    -- * Destructuring the Response
    , createQueryLoggingConfigResponse
    , CreateQueryLoggingConfigResponse
    -- * Response Lenses
    , cqlcrsResponseStatus
    , cqlcrsQueryLoggingConfig
    , cqlcrsLocation
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | /See:/ 'createQueryLoggingConfig' smart constructor.
data CreateQueryLoggingConfig = CreateQueryLoggingConfig'
  { _cqlcHostedZoneId              :: !ResourceId
  , _cqlcCloudWatchLogsLogGroupARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateQueryLoggingConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cqlcHostedZoneId' - The ID of the hosted zone that you want to log queries for. You can log queries only for public hosted zones.
--
-- * 'cqlcCloudWatchLogsLogGroupARN' - The Amazon Resource Name (ARN) for the log group that you want to Amazon Route 53 to send query logs to. This is the format of the ARN: arn:aws:logs:/region/ :/account-id/ :log-group:/log_group_name/  To get the ARN for a log group, you can use the CloudWatch console, the <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeLogGroups.html DescribeLogGroups> API action, the <http://docs.aws.amazon.com/cli/latest/reference/logs/describe-log-groups.html describe-log-groups> command, or the applicable command in one of the AWS SDKs.
createQueryLoggingConfig
    :: ResourceId -- ^ 'cqlcHostedZoneId'
    -> Text -- ^ 'cqlcCloudWatchLogsLogGroupARN'
    -> CreateQueryLoggingConfig
createQueryLoggingConfig pHostedZoneId_ pCloudWatchLogsLogGroupARN_ =
  CreateQueryLoggingConfig'
    { _cqlcHostedZoneId = pHostedZoneId_
    , _cqlcCloudWatchLogsLogGroupARN = pCloudWatchLogsLogGroupARN_
    }


-- | The ID of the hosted zone that you want to log queries for. You can log queries only for public hosted zones.
cqlcHostedZoneId :: Lens' CreateQueryLoggingConfig ResourceId
cqlcHostedZoneId = lens _cqlcHostedZoneId (\ s a -> s{_cqlcHostedZoneId = a})

-- | The Amazon Resource Name (ARN) for the log group that you want to Amazon Route 53 to send query logs to. This is the format of the ARN: arn:aws:logs:/region/ :/account-id/ :log-group:/log_group_name/  To get the ARN for a log group, you can use the CloudWatch console, the <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeLogGroups.html DescribeLogGroups> API action, the <http://docs.aws.amazon.com/cli/latest/reference/logs/describe-log-groups.html describe-log-groups> command, or the applicable command in one of the AWS SDKs.
cqlcCloudWatchLogsLogGroupARN :: Lens' CreateQueryLoggingConfig Text
cqlcCloudWatchLogsLogGroupARN = lens _cqlcCloudWatchLogsLogGroupARN (\ s a -> s{_cqlcCloudWatchLogsLogGroupARN = a})

instance AWSRequest CreateQueryLoggingConfig where
        type Rs CreateQueryLoggingConfig =
             CreateQueryLoggingConfigResponse
        request = postXML route53
        response
          = receiveXML
              (\ s h x ->
                 CreateQueryLoggingConfigResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "QueryLoggingConfig")
                     <*> (h .# "Location"))

instance Hashable CreateQueryLoggingConfig where

instance NFData CreateQueryLoggingConfig where

instance ToElement CreateQueryLoggingConfig where
        toElement
          = mkElement
              "{https://route53.amazonaws.com/doc/2013-04-01/}CreateQueryLoggingConfigRequest"

instance ToHeaders CreateQueryLoggingConfig where
        toHeaders = const mempty

instance ToPath CreateQueryLoggingConfig where
        toPath = const "/2013-04-01/queryloggingconfig"

instance ToQuery CreateQueryLoggingConfig where
        toQuery = const mempty

instance ToXML CreateQueryLoggingConfig where
        toXML CreateQueryLoggingConfig'{..}
          = mconcat
              ["HostedZoneId" @= _cqlcHostedZoneId,
               "CloudWatchLogsLogGroupArn" @=
                 _cqlcCloudWatchLogsLogGroupARN]

-- | /See:/ 'createQueryLoggingConfigResponse' smart constructor.
data CreateQueryLoggingConfigResponse = CreateQueryLoggingConfigResponse'
  { _cqlcrsResponseStatus     :: !Int
  , _cqlcrsQueryLoggingConfig :: !QueryLoggingConfig
  , _cqlcrsLocation           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateQueryLoggingConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cqlcrsResponseStatus' - -- | The response status code.
--
-- * 'cqlcrsQueryLoggingConfig' - A complex type that contains the ID for a query logging configuration, the ID of the hosted zone that you want to log queries for, and the ARN for the log group that you want Amazon Route 53 to send query logs to.
--
-- * 'cqlcrsLocation' - The unique URL representing the new query logging configuration.
createQueryLoggingConfigResponse
    :: Int -- ^ 'cqlcrsResponseStatus'
    -> QueryLoggingConfig -- ^ 'cqlcrsQueryLoggingConfig'
    -> Text -- ^ 'cqlcrsLocation'
    -> CreateQueryLoggingConfigResponse
createQueryLoggingConfigResponse pResponseStatus_ pQueryLoggingConfig_ pLocation_ =
  CreateQueryLoggingConfigResponse'
    { _cqlcrsResponseStatus = pResponseStatus_
    , _cqlcrsQueryLoggingConfig = pQueryLoggingConfig_
    , _cqlcrsLocation = pLocation_
    }


-- | -- | The response status code.
cqlcrsResponseStatus :: Lens' CreateQueryLoggingConfigResponse Int
cqlcrsResponseStatus = lens _cqlcrsResponseStatus (\ s a -> s{_cqlcrsResponseStatus = a})

-- | A complex type that contains the ID for a query logging configuration, the ID of the hosted zone that you want to log queries for, and the ARN for the log group that you want Amazon Route 53 to send query logs to.
cqlcrsQueryLoggingConfig :: Lens' CreateQueryLoggingConfigResponse QueryLoggingConfig
cqlcrsQueryLoggingConfig = lens _cqlcrsQueryLoggingConfig (\ s a -> s{_cqlcrsQueryLoggingConfig = a})

-- | The unique URL representing the new query logging configuration.
cqlcrsLocation :: Lens' CreateQueryLoggingConfigResponse Text
cqlcrsLocation = lens _cqlcrsLocation (\ s a -> s{_cqlcrsLocation = a})

instance NFData CreateQueryLoggingConfigResponse
         where
