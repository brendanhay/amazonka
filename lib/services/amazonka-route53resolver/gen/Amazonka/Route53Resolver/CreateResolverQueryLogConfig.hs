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
-- Module      : Amazonka.Route53Resolver.CreateResolverQueryLogConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Resolver query logging configuration, which defines where you
-- want Resolver to save DNS query logs that originate in your VPCs.
-- Resolver can log queries only for VPCs that are in the same Region as
-- the query logging configuration.
--
-- To specify which VPCs you want to log queries for, you use
-- @AssociateResolverQueryLogConfig@. For more information, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_AssociateResolverQueryLogConfig.html AssociateResolverQueryLogConfig>.
--
-- You can optionally use Resource Access Manager (RAM) to share a query
-- logging configuration with other Amazon Web Services accounts. The other
-- accounts can then associate VPCs with the configuration. The query logs
-- that Resolver creates for a configuration include all DNS queries that
-- originate in all VPCs that are associated with the configuration.
module Amazonka.Route53Resolver.CreateResolverQueryLogConfig
  ( -- * Creating a Request
    CreateResolverQueryLogConfig (..),
    newCreateResolverQueryLogConfig,

    -- * Request Lenses
    createResolverQueryLogConfig_tags,
    createResolverQueryLogConfig_name,
    createResolverQueryLogConfig_destinationArn,
    createResolverQueryLogConfig_creatorRequestId,

    -- * Destructuring the Response
    CreateResolverQueryLogConfigResponse (..),
    newCreateResolverQueryLogConfigResponse,

    -- * Response Lenses
    createResolverQueryLogConfigResponse_resolverQueryLogConfig,
    createResolverQueryLogConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newCreateResolverQueryLogConfig' smart constructor.
data CreateResolverQueryLogConfig = CreateResolverQueryLogConfig'
  { -- | A list of the tag keys and values that you want to associate with the
    -- query logging configuration.
    tags :: Prelude.Maybe [Tag],
    -- | The name that you want to give the query logging configuration.
    name :: Prelude.Text,
    -- | The ARN of the resource that you want Resolver to send query logs. You
    -- can send query logs to an S3 bucket, a CloudWatch Logs log group, or a
    -- Kinesis Data Firehose delivery stream. Examples of valid values include
    -- the following:
    --
    -- -   __S3 bucket__:
    --
    --     @arn:aws:s3:::examplebucket@
    --
    --     You can optionally append a file prefix to the end of the ARN.
    --
    --     @arn:aws:s3:::examplebucket\/development\/@
    --
    -- -   __CloudWatch Logs log group__:
    --
    --     @arn:aws:logs:us-west-1:123456789012:log-group:\/mystack-testgroup-12ABC1AB12A1:*@
    --
    -- -   __Kinesis Data Firehose delivery stream__:
    --
    --     @arn:aws:kinesis:us-east-2:0123456789:stream\/my_stream_name@
    destinationArn :: Prelude.Text,
    -- | A unique string that identifies the request and that allows failed
    -- requests to be retried without the risk of running the operation twice.
    -- @CreatorRequestId@ can be any unique string, for example, a date\/time
    -- stamp.
    creatorRequestId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateResolverQueryLogConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createResolverQueryLogConfig_tags' - A list of the tag keys and values that you want to associate with the
-- query logging configuration.
--
-- 'name', 'createResolverQueryLogConfig_name' - The name that you want to give the query logging configuration.
--
-- 'destinationArn', 'createResolverQueryLogConfig_destinationArn' - The ARN of the resource that you want Resolver to send query logs. You
-- can send query logs to an S3 bucket, a CloudWatch Logs log group, or a
-- Kinesis Data Firehose delivery stream. Examples of valid values include
-- the following:
--
-- -   __S3 bucket__:
--
--     @arn:aws:s3:::examplebucket@
--
--     You can optionally append a file prefix to the end of the ARN.
--
--     @arn:aws:s3:::examplebucket\/development\/@
--
-- -   __CloudWatch Logs log group__:
--
--     @arn:aws:logs:us-west-1:123456789012:log-group:\/mystack-testgroup-12ABC1AB12A1:*@
--
-- -   __Kinesis Data Firehose delivery stream__:
--
--     @arn:aws:kinesis:us-east-2:0123456789:stream\/my_stream_name@
--
-- 'creatorRequestId', 'createResolverQueryLogConfig_creatorRequestId' - A unique string that identifies the request and that allows failed
-- requests to be retried without the risk of running the operation twice.
-- @CreatorRequestId@ can be any unique string, for example, a date\/time
-- stamp.
newCreateResolverQueryLogConfig ::
  -- | 'name'
  Prelude.Text ->
  -- | 'destinationArn'
  Prelude.Text ->
  -- | 'creatorRequestId'
  Prelude.Text ->
  CreateResolverQueryLogConfig
newCreateResolverQueryLogConfig
  pName_
  pDestinationArn_
  pCreatorRequestId_ =
    CreateResolverQueryLogConfig'
      { tags =
          Prelude.Nothing,
        name = pName_,
        destinationArn = pDestinationArn_,
        creatorRequestId = pCreatorRequestId_
      }

-- | A list of the tag keys and values that you want to associate with the
-- query logging configuration.
createResolverQueryLogConfig_tags :: Lens.Lens' CreateResolverQueryLogConfig (Prelude.Maybe [Tag])
createResolverQueryLogConfig_tags = Lens.lens (\CreateResolverQueryLogConfig' {tags} -> tags) (\s@CreateResolverQueryLogConfig' {} a -> s {tags = a} :: CreateResolverQueryLogConfig) Prelude.. Lens.mapping Lens.coerced

-- | The name that you want to give the query logging configuration.
createResolverQueryLogConfig_name :: Lens.Lens' CreateResolverQueryLogConfig Prelude.Text
createResolverQueryLogConfig_name = Lens.lens (\CreateResolverQueryLogConfig' {name} -> name) (\s@CreateResolverQueryLogConfig' {} a -> s {name = a} :: CreateResolverQueryLogConfig)

-- | The ARN of the resource that you want Resolver to send query logs. You
-- can send query logs to an S3 bucket, a CloudWatch Logs log group, or a
-- Kinesis Data Firehose delivery stream. Examples of valid values include
-- the following:
--
-- -   __S3 bucket__:
--
--     @arn:aws:s3:::examplebucket@
--
--     You can optionally append a file prefix to the end of the ARN.
--
--     @arn:aws:s3:::examplebucket\/development\/@
--
-- -   __CloudWatch Logs log group__:
--
--     @arn:aws:logs:us-west-1:123456789012:log-group:\/mystack-testgroup-12ABC1AB12A1:*@
--
-- -   __Kinesis Data Firehose delivery stream__:
--
--     @arn:aws:kinesis:us-east-2:0123456789:stream\/my_stream_name@
createResolverQueryLogConfig_destinationArn :: Lens.Lens' CreateResolverQueryLogConfig Prelude.Text
createResolverQueryLogConfig_destinationArn = Lens.lens (\CreateResolverQueryLogConfig' {destinationArn} -> destinationArn) (\s@CreateResolverQueryLogConfig' {} a -> s {destinationArn = a} :: CreateResolverQueryLogConfig)

-- | A unique string that identifies the request and that allows failed
-- requests to be retried without the risk of running the operation twice.
-- @CreatorRequestId@ can be any unique string, for example, a date\/time
-- stamp.
createResolverQueryLogConfig_creatorRequestId :: Lens.Lens' CreateResolverQueryLogConfig Prelude.Text
createResolverQueryLogConfig_creatorRequestId = Lens.lens (\CreateResolverQueryLogConfig' {creatorRequestId} -> creatorRequestId) (\s@CreateResolverQueryLogConfig' {} a -> s {creatorRequestId = a} :: CreateResolverQueryLogConfig)

instance Core.AWSRequest CreateResolverQueryLogConfig where
  type
    AWSResponse CreateResolverQueryLogConfig =
      CreateResolverQueryLogConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateResolverQueryLogConfigResponse'
            Prelude.<$> (x Data..?> "ResolverQueryLogConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateResolverQueryLogConfig
  where
  hashWithSalt _salt CreateResolverQueryLogConfig' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` destinationArn
      `Prelude.hashWithSalt` creatorRequestId

instance Prelude.NFData CreateResolverQueryLogConfig where
  rnf CreateResolverQueryLogConfig' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf destinationArn
      `Prelude.seq` Prelude.rnf creatorRequestId

instance Data.ToHeaders CreateResolverQueryLogConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.CreateResolverQueryLogConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateResolverQueryLogConfig where
  toJSON CreateResolverQueryLogConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("DestinationArn" Data..= destinationArn),
            Prelude.Just
              ("CreatorRequestId" Data..= creatorRequestId)
          ]
      )

instance Data.ToPath CreateResolverQueryLogConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateResolverQueryLogConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateResolverQueryLogConfigResponse' smart constructor.
data CreateResolverQueryLogConfigResponse = CreateResolverQueryLogConfigResponse'
  { -- | Information about the @CreateResolverQueryLogConfig@ request, including
    -- the status of the request.
    resolverQueryLogConfig :: Prelude.Maybe ResolverQueryLogConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateResolverQueryLogConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverQueryLogConfig', 'createResolverQueryLogConfigResponse_resolverQueryLogConfig' - Information about the @CreateResolverQueryLogConfig@ request, including
-- the status of the request.
--
-- 'httpStatus', 'createResolverQueryLogConfigResponse_httpStatus' - The response's http status code.
newCreateResolverQueryLogConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateResolverQueryLogConfigResponse
newCreateResolverQueryLogConfigResponse pHttpStatus_ =
  CreateResolverQueryLogConfigResponse'
    { resolverQueryLogConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the @CreateResolverQueryLogConfig@ request, including
-- the status of the request.
createResolverQueryLogConfigResponse_resolverQueryLogConfig :: Lens.Lens' CreateResolverQueryLogConfigResponse (Prelude.Maybe ResolverQueryLogConfig)
createResolverQueryLogConfigResponse_resolverQueryLogConfig = Lens.lens (\CreateResolverQueryLogConfigResponse' {resolverQueryLogConfig} -> resolverQueryLogConfig) (\s@CreateResolverQueryLogConfigResponse' {} a -> s {resolverQueryLogConfig = a} :: CreateResolverQueryLogConfigResponse)

-- | The response's http status code.
createResolverQueryLogConfigResponse_httpStatus :: Lens.Lens' CreateResolverQueryLogConfigResponse Prelude.Int
createResolverQueryLogConfigResponse_httpStatus = Lens.lens (\CreateResolverQueryLogConfigResponse' {httpStatus} -> httpStatus) (\s@CreateResolverQueryLogConfigResponse' {} a -> s {httpStatus = a} :: CreateResolverQueryLogConfigResponse)

instance
  Prelude.NFData
    CreateResolverQueryLogConfigResponse
  where
  rnf CreateResolverQueryLogConfigResponse' {..} =
    Prelude.rnf resolverQueryLogConfig
      `Prelude.seq` Prelude.rnf httpStatus
