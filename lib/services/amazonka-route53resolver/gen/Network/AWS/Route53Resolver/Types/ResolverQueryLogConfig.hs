{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Resolver.Types.ResolverQueryLogConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Resolver.Types.ResolverQueryLogConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53Resolver.Types.ResolverQueryLogConfigStatus
import Network.AWS.Route53Resolver.Types.ShareStatus

-- | In the response to a
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_CreateResolverQueryLogConfig.html CreateResolverQueryLogConfig>,
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_DeleteResolverQueryLogConfig.html DeleteResolverQueryLogConfig>,
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_GetResolverQueryLogConfig.html GetResolverQueryLogConfig>,
-- or
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_ListResolverQueryLogConfigs.html ListResolverQueryLogConfigs>
-- request, a complex type that contains settings for one query logging
-- configuration.
--
-- /See:/ 'newResolverQueryLogConfig' smart constructor.
data ResolverQueryLogConfig = ResolverQueryLogConfig'
  { -- | The date and time that the query logging configuration was created, in
    -- Unix time format and Coordinated Universal Time (UTC).
    creationTime :: Prelude.Maybe Prelude.Text,
    -- | The status of the specified query logging configuration. Valid values
    -- include the following:
    --
    -- -   @CREATING@: Resolver is creating the query logging configuration.
    --
    -- -   @CREATED@: The query logging configuration was successfully created.
    --     Resolver is logging queries that originate in the specified VPC.
    --
    -- -   @DELETING@: Resolver is deleting this query logging configuration.
    --
    -- -   @FAILED@: Resolver can\'t deliver logs to the location that is
    --     specified in the query logging configuration. Here are two common
    --     causes:
    --
    --     -   The specified destination (for example, an Amazon S3 bucket) was
    --         deleted.
    --
    --     -   Permissions don\'t allow sending logs to the destination.
    status :: Prelude.Maybe ResolverQueryLogConfigStatus,
    -- | The number of VPCs that are associated with the query logging
    -- configuration.
    associationCount :: Prelude.Maybe Prelude.Int,
    -- | The ARN for the query logging configuration.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A unique string that identifies the request that created the query
    -- logging configuration. The @CreatorRequestId@ allows failed requests to
    -- be retried without the risk of running the operation twice.
    creatorRequestId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the resource that you want Resolver to send query logs: an
    -- Amazon S3 bucket, a CloudWatch Logs log group, or a Kinesis Data
    -- Firehose delivery stream.
    destinationArn :: Prelude.Maybe Prelude.Text,
    -- | An indication of whether the query logging configuration is shared with
    -- other Amazon Web Services accounts, or was shared with the current
    -- account by another Amazon Web Services account. Sharing is configured
    -- through Resource Access Manager (RAM).
    shareStatus :: Prelude.Maybe ShareStatus,
    -- | The Amazon Web Services account ID for the account that created the
    -- query logging configuration.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The name of the query logging configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID for the query logging configuration.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResolverQueryLogConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'resolverQueryLogConfig_creationTime' - The date and time that the query logging configuration was created, in
-- Unix time format and Coordinated Universal Time (UTC).
--
-- 'status', 'resolverQueryLogConfig_status' - The status of the specified query logging configuration. Valid values
-- include the following:
--
-- -   @CREATING@: Resolver is creating the query logging configuration.
--
-- -   @CREATED@: The query logging configuration was successfully created.
--     Resolver is logging queries that originate in the specified VPC.
--
-- -   @DELETING@: Resolver is deleting this query logging configuration.
--
-- -   @FAILED@: Resolver can\'t deliver logs to the location that is
--     specified in the query logging configuration. Here are two common
--     causes:
--
--     -   The specified destination (for example, an Amazon S3 bucket) was
--         deleted.
--
--     -   Permissions don\'t allow sending logs to the destination.
--
-- 'associationCount', 'resolverQueryLogConfig_associationCount' - The number of VPCs that are associated with the query logging
-- configuration.
--
-- 'arn', 'resolverQueryLogConfig_arn' - The ARN for the query logging configuration.
--
-- 'creatorRequestId', 'resolverQueryLogConfig_creatorRequestId' - A unique string that identifies the request that created the query
-- logging configuration. The @CreatorRequestId@ allows failed requests to
-- be retried without the risk of running the operation twice.
--
-- 'destinationArn', 'resolverQueryLogConfig_destinationArn' - The ARN of the resource that you want Resolver to send query logs: an
-- Amazon S3 bucket, a CloudWatch Logs log group, or a Kinesis Data
-- Firehose delivery stream.
--
-- 'shareStatus', 'resolverQueryLogConfig_shareStatus' - An indication of whether the query logging configuration is shared with
-- other Amazon Web Services accounts, or was shared with the current
-- account by another Amazon Web Services account. Sharing is configured
-- through Resource Access Manager (RAM).
--
-- 'ownerId', 'resolverQueryLogConfig_ownerId' - The Amazon Web Services account ID for the account that created the
-- query logging configuration.
--
-- 'name', 'resolverQueryLogConfig_name' - The name of the query logging configuration.
--
-- 'id', 'resolverQueryLogConfig_id' - The ID for the query logging configuration.
newResolverQueryLogConfig ::
  ResolverQueryLogConfig
newResolverQueryLogConfig =
  ResolverQueryLogConfig'
    { creationTime =
        Prelude.Nothing,
      status = Prelude.Nothing,
      associationCount = Prelude.Nothing,
      arn = Prelude.Nothing,
      creatorRequestId = Prelude.Nothing,
      destinationArn = Prelude.Nothing,
      shareStatus = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The date and time that the query logging configuration was created, in
-- Unix time format and Coordinated Universal Time (UTC).
resolverQueryLogConfig_creationTime :: Lens.Lens' ResolverQueryLogConfig (Prelude.Maybe Prelude.Text)
resolverQueryLogConfig_creationTime = Lens.lens (\ResolverQueryLogConfig' {creationTime} -> creationTime) (\s@ResolverQueryLogConfig' {} a -> s {creationTime = a} :: ResolverQueryLogConfig)

-- | The status of the specified query logging configuration. Valid values
-- include the following:
--
-- -   @CREATING@: Resolver is creating the query logging configuration.
--
-- -   @CREATED@: The query logging configuration was successfully created.
--     Resolver is logging queries that originate in the specified VPC.
--
-- -   @DELETING@: Resolver is deleting this query logging configuration.
--
-- -   @FAILED@: Resolver can\'t deliver logs to the location that is
--     specified in the query logging configuration. Here are two common
--     causes:
--
--     -   The specified destination (for example, an Amazon S3 bucket) was
--         deleted.
--
--     -   Permissions don\'t allow sending logs to the destination.
resolverQueryLogConfig_status :: Lens.Lens' ResolverQueryLogConfig (Prelude.Maybe ResolverQueryLogConfigStatus)
resolverQueryLogConfig_status = Lens.lens (\ResolverQueryLogConfig' {status} -> status) (\s@ResolverQueryLogConfig' {} a -> s {status = a} :: ResolverQueryLogConfig)

-- | The number of VPCs that are associated with the query logging
-- configuration.
resolverQueryLogConfig_associationCount :: Lens.Lens' ResolverQueryLogConfig (Prelude.Maybe Prelude.Int)
resolverQueryLogConfig_associationCount = Lens.lens (\ResolverQueryLogConfig' {associationCount} -> associationCount) (\s@ResolverQueryLogConfig' {} a -> s {associationCount = a} :: ResolverQueryLogConfig)

-- | The ARN for the query logging configuration.
resolverQueryLogConfig_arn :: Lens.Lens' ResolverQueryLogConfig (Prelude.Maybe Prelude.Text)
resolverQueryLogConfig_arn = Lens.lens (\ResolverQueryLogConfig' {arn} -> arn) (\s@ResolverQueryLogConfig' {} a -> s {arn = a} :: ResolverQueryLogConfig)

-- | A unique string that identifies the request that created the query
-- logging configuration. The @CreatorRequestId@ allows failed requests to
-- be retried without the risk of running the operation twice.
resolverQueryLogConfig_creatorRequestId :: Lens.Lens' ResolverQueryLogConfig (Prelude.Maybe Prelude.Text)
resolverQueryLogConfig_creatorRequestId = Lens.lens (\ResolverQueryLogConfig' {creatorRequestId} -> creatorRequestId) (\s@ResolverQueryLogConfig' {} a -> s {creatorRequestId = a} :: ResolverQueryLogConfig)

-- | The ARN of the resource that you want Resolver to send query logs: an
-- Amazon S3 bucket, a CloudWatch Logs log group, or a Kinesis Data
-- Firehose delivery stream.
resolverQueryLogConfig_destinationArn :: Lens.Lens' ResolverQueryLogConfig (Prelude.Maybe Prelude.Text)
resolverQueryLogConfig_destinationArn = Lens.lens (\ResolverQueryLogConfig' {destinationArn} -> destinationArn) (\s@ResolverQueryLogConfig' {} a -> s {destinationArn = a} :: ResolverQueryLogConfig)

-- | An indication of whether the query logging configuration is shared with
-- other Amazon Web Services accounts, or was shared with the current
-- account by another Amazon Web Services account. Sharing is configured
-- through Resource Access Manager (RAM).
resolverQueryLogConfig_shareStatus :: Lens.Lens' ResolverQueryLogConfig (Prelude.Maybe ShareStatus)
resolverQueryLogConfig_shareStatus = Lens.lens (\ResolverQueryLogConfig' {shareStatus} -> shareStatus) (\s@ResolverQueryLogConfig' {} a -> s {shareStatus = a} :: ResolverQueryLogConfig)

-- | The Amazon Web Services account ID for the account that created the
-- query logging configuration.
resolverQueryLogConfig_ownerId :: Lens.Lens' ResolverQueryLogConfig (Prelude.Maybe Prelude.Text)
resolverQueryLogConfig_ownerId = Lens.lens (\ResolverQueryLogConfig' {ownerId} -> ownerId) (\s@ResolverQueryLogConfig' {} a -> s {ownerId = a} :: ResolverQueryLogConfig)

-- | The name of the query logging configuration.
resolverQueryLogConfig_name :: Lens.Lens' ResolverQueryLogConfig (Prelude.Maybe Prelude.Text)
resolverQueryLogConfig_name = Lens.lens (\ResolverQueryLogConfig' {name} -> name) (\s@ResolverQueryLogConfig' {} a -> s {name = a} :: ResolverQueryLogConfig)

-- | The ID for the query logging configuration.
resolverQueryLogConfig_id :: Lens.Lens' ResolverQueryLogConfig (Prelude.Maybe Prelude.Text)
resolverQueryLogConfig_id = Lens.lens (\ResolverQueryLogConfig' {id} -> id) (\s@ResolverQueryLogConfig' {} a -> s {id = a} :: ResolverQueryLogConfig)

instance Core.FromJSON ResolverQueryLogConfig where
  parseJSON =
    Core.withObject
      "ResolverQueryLogConfig"
      ( \x ->
          ResolverQueryLogConfig'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "AssociationCount")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "CreatorRequestId")
            Prelude.<*> (x Core..:? "DestinationArn")
            Prelude.<*> (x Core..:? "ShareStatus")
            Prelude.<*> (x Core..:? "OwnerId")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Id")
      )

instance Prelude.Hashable ResolverQueryLogConfig

instance Prelude.NFData ResolverQueryLogConfig
