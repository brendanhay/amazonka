{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Route53.CreateQueryLoggingConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a configuration for DNS query logging. After you create a query
-- logging configuration, Amazon Route 53 begins to publish log data to an
-- Amazon CloudWatch Logs log group.
--
-- DNS query logs contain information about the queries that Route 53
-- receives for a specified public hosted zone, such as the following:
--
-- -   Route 53 edge location that responded to the DNS query
--
-- -   Domain or subdomain that was requested
--
-- -   DNS record type, such as A or AAAA
--
-- -   DNS response code, such as @NoError@ or @ServFail@
--
-- [Log Group and Resource Policy]
--     Before you create a query logging configuration, perform the
--     following operations.
--
--     If you create a query logging configuration using the Route 53
--     console, Route 53 performs these operations automatically.
--
--     1.  Create a CloudWatch Logs log group, and make note of the ARN,
--         which you specify when you create a query logging configuration.
--         Note the following:
--
--         -   You must create the log group in the us-east-1 region.
--
--         -   You must use the same Amazon Web Services account to create
--             the log group and the hosted zone that you want to configure
--             query logging for.
--
--         -   When you create log groups for query logging, we recommend
--             that you use a consistent prefix, for example:
--
--             @\/aws\/route53\/hosted zone name @
--
--             In the next step, you\'ll create a resource policy, which
--             controls access to one or more log groups and the associated
--             Amazon Web Services resources, such as Route 53 hosted
--             zones. There\'s a limit on the number of resource policies
--             that you can create, so we recommend that you use a
--             consistent prefix so you can use the same resource policy
--             for all the log groups that you create for query logging.
--
--     2.  Create a CloudWatch Logs resource policy, and give it the
--         permissions that Route 53 needs to create log streams and to
--         send query logs to log streams. For the value of @Resource@,
--         specify the ARN for the log group that you created in the
--         previous step. To use the same resource policy for all the
--         CloudWatch Logs log groups that you created for query logging
--         configurations, replace the hosted zone name with @*@, for
--         example:
--
--         @arn:aws:logs:us-east-1:123412341234:log-group:\/aws\/route53\/*@
--
--         To avoid the confused deputy problem, a security issue where an
--         entity without a permission for an action can coerce a
--         more-privileged entity to perform it, you can optionally limit
--         the permissions that a service has to a resource in a
--         resource-based policy by supplying the following values:
--
--         -   For @aws:SourceArn@, supply the hosted zone ARN used in
--             creating the query logging configuration. For example,
--             @aws:SourceArn: arn:aws:route53:::hostedzone\/hosted zone ID@.
--
--         -   For @aws:SourceAccount@, supply the account ID for the
--             account that creates the query logging configuration. For
--             example, @aws:SourceAccount:111111111111@.
--
--         For more information, see
--         <https://docs.aws.amazon.com/IAM/latest/UserGuide/confused-deputy.html The confused deputy problem>
--         in the /Amazon Web Services IAM User Guide/.
--
--         You can\'t use the CloudWatch console to create or edit a
--         resource policy. You must use the CloudWatch API, one of the
--         Amazon Web Services SDKs, or the CLI.
--
-- [Log Streams and Edge Locations]
--     When Route 53 finishes creating the configuration for DNS query
--     logging, it does the following:
--
--     -   Creates a log stream for an edge location the first time that
--         the edge location responds to DNS queries for the specified
--         hosted zone. That log stream is used to log all queries that
--         Route 53 responds to for that edge location.
--
--     -   Begins to send query logs to the applicable log stream.
--
--     The name of each log stream is in the following format:
--
--     @ hosted zone ID\/edge location code @
--
--     The edge location code is a three-letter code and an arbitrarily
--     assigned number, for example, DFW3. The three-letter code typically
--     corresponds with the International Air Transport Association airport
--     code for an airport near the edge location. (These abbreviations
--     might change in the future.) For a list of edge locations, see \"The
--     Route 53 Global Network\" on the
--     <http://aws.amazon.com/route53/details/ Route 53 Product Details>
--     page.
--
-- [Queries That Are Logged]
--     Query logs contain only the queries that DNS resolvers forward to
--     Route 53. If a DNS resolver has already cached the response to a
--     query (such as the IP address for a load balancer for example.com),
--     the resolver will continue to return the cached response. It
--     doesn\'t forward another query to Route 53 until the TTL for the
--     corresponding resource record set expires. Depending on how many DNS
--     queries are submitted for a resource record set, and depending on
--     the TTL for that resource record set, query logs might contain
--     information about only one query out of every several thousand
--     queries that are submitted to DNS. For more information about how
--     DNS works, see
--     <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/welcome-dns-service.html Routing Internet Traffic to Your Website or Web Application>
--     in the /Amazon Route 53 Developer Guide/.
--
-- [Log File Format]
--     For a list of the values in each query log and the format of each
--     value, see
--     <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/query-logs.html Logging DNS Queries>
--     in the /Amazon Route 53 Developer Guide/.
--
-- [Pricing]
--     For information about charges for query logs, see
--     <http://aws.amazon.com/cloudwatch/pricing/ Amazon CloudWatch Pricing>.
--
-- [How to Stop Logging]
--     If you want Route 53 to stop sending query logs to CloudWatch Logs,
--     delete the query logging configuration. For more information, see
--     <https://docs.aws.amazon.com/Route53/latest/APIReference/API_DeleteQueryLoggingConfig.html DeleteQueryLoggingConfig>.
module Amazonka.Route53.CreateQueryLoggingConfig
  ( -- * Creating a Request
    CreateQueryLoggingConfig (..),
    newCreateQueryLoggingConfig,

    -- * Request Lenses
    createQueryLoggingConfig_hostedZoneId,
    createQueryLoggingConfig_cloudWatchLogsLogGroupArn,

    -- * Destructuring the Response
    CreateQueryLoggingConfigResponse (..),
    newCreateQueryLoggingConfigResponse,

    -- * Response Lenses
    createQueryLoggingConfigResponse_httpStatus,
    createQueryLoggingConfigResponse_queryLoggingConfig,
    createQueryLoggingConfigResponse_location,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | /See:/ 'newCreateQueryLoggingConfig' smart constructor.
data CreateQueryLoggingConfig = CreateQueryLoggingConfig'
  { -- | The ID of the hosted zone that you want to log queries for. You can log
    -- queries only for public hosted zones.
    hostedZoneId :: ResourceId,
    -- | The Amazon Resource Name (ARN) for the log group that you want to Amazon
    -- Route 53 to send query logs to. This is the format of the ARN:
    --
    -- arn:aws:logs:/region/:/account-id/:log-group:/log_group_name/
    --
    -- To get the ARN for a log group, you can use the CloudWatch console, the
    -- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeLogGroups.html DescribeLogGroups>
    -- API action, the
    -- <https://docs.aws.amazon.com/cli/latest/reference/logs/describe-log-groups.html describe-log-groups>
    -- command, or the applicable command in one of the Amazon Web Services
    -- SDKs.
    cloudWatchLogsLogGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateQueryLoggingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostedZoneId', 'createQueryLoggingConfig_hostedZoneId' - The ID of the hosted zone that you want to log queries for. You can log
-- queries only for public hosted zones.
--
-- 'cloudWatchLogsLogGroupArn', 'createQueryLoggingConfig_cloudWatchLogsLogGroupArn' - The Amazon Resource Name (ARN) for the log group that you want to Amazon
-- Route 53 to send query logs to. This is the format of the ARN:
--
-- arn:aws:logs:/region/:/account-id/:log-group:/log_group_name/
--
-- To get the ARN for a log group, you can use the CloudWatch console, the
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeLogGroups.html DescribeLogGroups>
-- API action, the
-- <https://docs.aws.amazon.com/cli/latest/reference/logs/describe-log-groups.html describe-log-groups>
-- command, or the applicable command in one of the Amazon Web Services
-- SDKs.
newCreateQueryLoggingConfig ::
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'cloudWatchLogsLogGroupArn'
  Prelude.Text ->
  CreateQueryLoggingConfig
newCreateQueryLoggingConfig
  pHostedZoneId_
  pCloudWatchLogsLogGroupArn_ =
    CreateQueryLoggingConfig'
      { hostedZoneId =
          pHostedZoneId_,
        cloudWatchLogsLogGroupArn =
          pCloudWatchLogsLogGroupArn_
      }

-- | The ID of the hosted zone that you want to log queries for. You can log
-- queries only for public hosted zones.
createQueryLoggingConfig_hostedZoneId :: Lens.Lens' CreateQueryLoggingConfig ResourceId
createQueryLoggingConfig_hostedZoneId = Lens.lens (\CreateQueryLoggingConfig' {hostedZoneId} -> hostedZoneId) (\s@CreateQueryLoggingConfig' {} a -> s {hostedZoneId = a} :: CreateQueryLoggingConfig)

-- | The Amazon Resource Name (ARN) for the log group that you want to Amazon
-- Route 53 to send query logs to. This is the format of the ARN:
--
-- arn:aws:logs:/region/:/account-id/:log-group:/log_group_name/
--
-- To get the ARN for a log group, you can use the CloudWatch console, the
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeLogGroups.html DescribeLogGroups>
-- API action, the
-- <https://docs.aws.amazon.com/cli/latest/reference/logs/describe-log-groups.html describe-log-groups>
-- command, or the applicable command in one of the Amazon Web Services
-- SDKs.
createQueryLoggingConfig_cloudWatchLogsLogGroupArn :: Lens.Lens' CreateQueryLoggingConfig Prelude.Text
createQueryLoggingConfig_cloudWatchLogsLogGroupArn = Lens.lens (\CreateQueryLoggingConfig' {cloudWatchLogsLogGroupArn} -> cloudWatchLogsLogGroupArn) (\s@CreateQueryLoggingConfig' {} a -> s {cloudWatchLogsLogGroupArn = a} :: CreateQueryLoggingConfig)

instance Core.AWSRequest CreateQueryLoggingConfig where
  type
    AWSResponse CreateQueryLoggingConfig =
      CreateQueryLoggingConfigResponse
  request overrides =
    Request.postXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateQueryLoggingConfigResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..@ "QueryLoggingConfig")
            Prelude.<*> (h Core..# "Location")
      )

instance Prelude.Hashable CreateQueryLoggingConfig where
  hashWithSalt _salt CreateQueryLoggingConfig' {..} =
    _salt `Prelude.hashWithSalt` hostedZoneId
      `Prelude.hashWithSalt` cloudWatchLogsLogGroupArn

instance Prelude.NFData CreateQueryLoggingConfig where
  rnf CreateQueryLoggingConfig' {..} =
    Prelude.rnf hostedZoneId
      `Prelude.seq` Prelude.rnf cloudWatchLogsLogGroupArn

instance Core.ToElement CreateQueryLoggingConfig where
  toElement =
    Core.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}CreateQueryLoggingConfigRequest"

instance Core.ToHeaders CreateQueryLoggingConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateQueryLoggingConfig where
  toPath =
    Prelude.const "/2013-04-01/queryloggingconfig"

instance Core.ToQuery CreateQueryLoggingConfig where
  toQuery = Prelude.const Prelude.mempty

instance Core.ToXML CreateQueryLoggingConfig where
  toXML CreateQueryLoggingConfig' {..} =
    Prelude.mconcat
      [ "HostedZoneId" Core.@= hostedZoneId,
        "CloudWatchLogsLogGroupArn"
          Core.@= cloudWatchLogsLogGroupArn
      ]

-- | /See:/ 'newCreateQueryLoggingConfigResponse' smart constructor.
data CreateQueryLoggingConfigResponse = CreateQueryLoggingConfigResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A complex type that contains the ID for a query logging configuration,
    -- the ID of the hosted zone that you want to log queries for, and the ARN
    -- for the log group that you want Amazon Route 53 to send query logs to.
    queryLoggingConfig :: QueryLoggingConfig,
    -- | The unique URL representing the new query logging configuration.
    location :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateQueryLoggingConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createQueryLoggingConfigResponse_httpStatus' - The response's http status code.
--
-- 'queryLoggingConfig', 'createQueryLoggingConfigResponse_queryLoggingConfig' - A complex type that contains the ID for a query logging configuration,
-- the ID of the hosted zone that you want to log queries for, and the ARN
-- for the log group that you want Amazon Route 53 to send query logs to.
--
-- 'location', 'createQueryLoggingConfigResponse_location' - The unique URL representing the new query logging configuration.
newCreateQueryLoggingConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'queryLoggingConfig'
  QueryLoggingConfig ->
  -- | 'location'
  Prelude.Text ->
  CreateQueryLoggingConfigResponse
newCreateQueryLoggingConfigResponse
  pHttpStatus_
  pQueryLoggingConfig_
  pLocation_ =
    CreateQueryLoggingConfigResponse'
      { httpStatus =
          pHttpStatus_,
        queryLoggingConfig = pQueryLoggingConfig_,
        location = pLocation_
      }

-- | The response's http status code.
createQueryLoggingConfigResponse_httpStatus :: Lens.Lens' CreateQueryLoggingConfigResponse Prelude.Int
createQueryLoggingConfigResponse_httpStatus = Lens.lens (\CreateQueryLoggingConfigResponse' {httpStatus} -> httpStatus) (\s@CreateQueryLoggingConfigResponse' {} a -> s {httpStatus = a} :: CreateQueryLoggingConfigResponse)

-- | A complex type that contains the ID for a query logging configuration,
-- the ID of the hosted zone that you want to log queries for, and the ARN
-- for the log group that you want Amazon Route 53 to send query logs to.
createQueryLoggingConfigResponse_queryLoggingConfig :: Lens.Lens' CreateQueryLoggingConfigResponse QueryLoggingConfig
createQueryLoggingConfigResponse_queryLoggingConfig = Lens.lens (\CreateQueryLoggingConfigResponse' {queryLoggingConfig} -> queryLoggingConfig) (\s@CreateQueryLoggingConfigResponse' {} a -> s {queryLoggingConfig = a} :: CreateQueryLoggingConfigResponse)

-- | The unique URL representing the new query logging configuration.
createQueryLoggingConfigResponse_location :: Lens.Lens' CreateQueryLoggingConfigResponse Prelude.Text
createQueryLoggingConfigResponse_location = Lens.lens (\CreateQueryLoggingConfigResponse' {location} -> location) (\s@CreateQueryLoggingConfigResponse' {} a -> s {location = a} :: CreateQueryLoggingConfigResponse)

instance
  Prelude.NFData
    CreateQueryLoggingConfigResponse
  where
  rnf CreateQueryLoggingConfigResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf queryLoggingConfig
      `Prelude.seq` Prelude.rnf location
