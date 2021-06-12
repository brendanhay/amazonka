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
-- Module      : Network.AWS.MQ.DescribeConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified configuration.
module Network.AWS.MQ.DescribeConfiguration
  ( -- * Creating a Request
    DescribeConfiguration (..),
    newDescribeConfiguration,

    -- * Request Lenses
    describeConfiguration_configurationId,

    -- * Destructuring the Response
    DescribeConfigurationResponse (..),
    newDescribeConfigurationResponse,

    -- * Response Lenses
    describeConfigurationResponse_engineType,
    describeConfigurationResponse_authenticationStrategy,
    describeConfigurationResponse_latestRevision,
    describeConfigurationResponse_arn,
    describeConfigurationResponse_id,
    describeConfigurationResponse_name,
    describeConfigurationResponse_engineVersion,
    describeConfigurationResponse_tags,
    describeConfigurationResponse_description,
    describeConfigurationResponse_created,
    describeConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeConfiguration' smart constructor.
data DescribeConfiguration = DescribeConfiguration'
  { -- | The unique ID that Amazon MQ generates for the configuration.
    configurationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationId', 'describeConfiguration_configurationId' - The unique ID that Amazon MQ generates for the configuration.
newDescribeConfiguration ::
  -- | 'configurationId'
  Core.Text ->
  DescribeConfiguration
newDescribeConfiguration pConfigurationId_ =
  DescribeConfiguration'
    { configurationId =
        pConfigurationId_
    }

-- | The unique ID that Amazon MQ generates for the configuration.
describeConfiguration_configurationId :: Lens.Lens' DescribeConfiguration Core.Text
describeConfiguration_configurationId = Lens.lens (\DescribeConfiguration' {configurationId} -> configurationId) (\s@DescribeConfiguration' {} a -> s {configurationId = a} :: DescribeConfiguration)

instance Core.AWSRequest DescribeConfiguration where
  type
    AWSResponse DescribeConfiguration =
      DescribeConfigurationResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConfigurationResponse'
            Core.<$> (x Core..?> "engineType")
            Core.<*> (x Core..?> "authenticationStrategy")
            Core.<*> (x Core..?> "latestRevision")
            Core.<*> (x Core..?> "arn")
            Core.<*> (x Core..?> "id")
            Core.<*> (x Core..?> "name")
            Core.<*> (x Core..?> "engineVersion")
            Core.<*> (x Core..?> "tags" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "description")
            Core.<*> (x Core..?> "created")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeConfiguration

instance Core.NFData DescribeConfiguration

instance Core.ToHeaders DescribeConfiguration where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DescribeConfiguration where
  toPath DescribeConfiguration' {..} =
    Core.mconcat
      ["/v1/configurations/", Core.toBS configurationId]

instance Core.ToQuery DescribeConfiguration where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeConfigurationResponse' smart constructor.
data DescribeConfigurationResponse = DescribeConfigurationResponse'
  { -- | Required. The type of broker engine. Note: Currently, Amazon MQ supports
    -- ACTIVEMQ and RABBITMQ.
    engineType :: Core.Maybe EngineType,
    -- | The authentication strategy associated with the configuration.
    authenticationStrategy :: Core.Maybe AuthenticationStrategy,
    -- | Required. The latest revision of the configuration.
    latestRevision :: Core.Maybe ConfigurationRevision,
    -- | Required. The ARN of the configuration.
    arn :: Core.Maybe Core.Text,
    -- | Required. The unique ID that Amazon MQ generates for the configuration.
    id :: Core.Maybe Core.Text,
    -- | Required. The name of the configuration. This value can contain only
    -- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
    -- ~). This value must be 1-150 characters long.
    name :: Core.Maybe Core.Text,
    -- | Required. The version of the broker engine. For a list of supported
    -- engine versions, see
    -- https:\/\/docs.aws.amazon.com\/amazon-mq\/latest\/developer-guide\/broker-engine.html
    engineVersion :: Core.Maybe Core.Text,
    -- | The list of all tags associated with this configuration.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Required. The description of the configuration.
    description :: Core.Maybe Core.Text,
    -- | Required. The date and time of the configuration revision.
    created :: Core.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineType', 'describeConfigurationResponse_engineType' - Required. The type of broker engine. Note: Currently, Amazon MQ supports
-- ACTIVEMQ and RABBITMQ.
--
-- 'authenticationStrategy', 'describeConfigurationResponse_authenticationStrategy' - The authentication strategy associated with the configuration.
--
-- 'latestRevision', 'describeConfigurationResponse_latestRevision' - Required. The latest revision of the configuration.
--
-- 'arn', 'describeConfigurationResponse_arn' - Required. The ARN of the configuration.
--
-- 'id', 'describeConfigurationResponse_id' - Required. The unique ID that Amazon MQ generates for the configuration.
--
-- 'name', 'describeConfigurationResponse_name' - Required. The name of the configuration. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 1-150 characters long.
--
-- 'engineVersion', 'describeConfigurationResponse_engineVersion' - Required. The version of the broker engine. For a list of supported
-- engine versions, see
-- https:\/\/docs.aws.amazon.com\/amazon-mq\/latest\/developer-guide\/broker-engine.html
--
-- 'tags', 'describeConfigurationResponse_tags' - The list of all tags associated with this configuration.
--
-- 'description', 'describeConfigurationResponse_description' - Required. The description of the configuration.
--
-- 'created', 'describeConfigurationResponse_created' - Required. The date and time of the configuration revision.
--
-- 'httpStatus', 'describeConfigurationResponse_httpStatus' - The response's http status code.
newDescribeConfigurationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeConfigurationResponse
newDescribeConfigurationResponse pHttpStatus_ =
  DescribeConfigurationResponse'
    { engineType =
        Core.Nothing,
      authenticationStrategy = Core.Nothing,
      latestRevision = Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing,
      engineVersion = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      created = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Required. The type of broker engine. Note: Currently, Amazon MQ supports
-- ACTIVEMQ and RABBITMQ.
describeConfigurationResponse_engineType :: Lens.Lens' DescribeConfigurationResponse (Core.Maybe EngineType)
describeConfigurationResponse_engineType = Lens.lens (\DescribeConfigurationResponse' {engineType} -> engineType) (\s@DescribeConfigurationResponse' {} a -> s {engineType = a} :: DescribeConfigurationResponse)

-- | The authentication strategy associated with the configuration.
describeConfigurationResponse_authenticationStrategy :: Lens.Lens' DescribeConfigurationResponse (Core.Maybe AuthenticationStrategy)
describeConfigurationResponse_authenticationStrategy = Lens.lens (\DescribeConfigurationResponse' {authenticationStrategy} -> authenticationStrategy) (\s@DescribeConfigurationResponse' {} a -> s {authenticationStrategy = a} :: DescribeConfigurationResponse)

-- | Required. The latest revision of the configuration.
describeConfigurationResponse_latestRevision :: Lens.Lens' DescribeConfigurationResponse (Core.Maybe ConfigurationRevision)
describeConfigurationResponse_latestRevision = Lens.lens (\DescribeConfigurationResponse' {latestRevision} -> latestRevision) (\s@DescribeConfigurationResponse' {} a -> s {latestRevision = a} :: DescribeConfigurationResponse)

-- | Required. The ARN of the configuration.
describeConfigurationResponse_arn :: Lens.Lens' DescribeConfigurationResponse (Core.Maybe Core.Text)
describeConfigurationResponse_arn = Lens.lens (\DescribeConfigurationResponse' {arn} -> arn) (\s@DescribeConfigurationResponse' {} a -> s {arn = a} :: DescribeConfigurationResponse)

-- | Required. The unique ID that Amazon MQ generates for the configuration.
describeConfigurationResponse_id :: Lens.Lens' DescribeConfigurationResponse (Core.Maybe Core.Text)
describeConfigurationResponse_id = Lens.lens (\DescribeConfigurationResponse' {id} -> id) (\s@DescribeConfigurationResponse' {} a -> s {id = a} :: DescribeConfigurationResponse)

-- | Required. The name of the configuration. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 1-150 characters long.
describeConfigurationResponse_name :: Lens.Lens' DescribeConfigurationResponse (Core.Maybe Core.Text)
describeConfigurationResponse_name = Lens.lens (\DescribeConfigurationResponse' {name} -> name) (\s@DescribeConfigurationResponse' {} a -> s {name = a} :: DescribeConfigurationResponse)

-- | Required. The version of the broker engine. For a list of supported
-- engine versions, see
-- https:\/\/docs.aws.amazon.com\/amazon-mq\/latest\/developer-guide\/broker-engine.html
describeConfigurationResponse_engineVersion :: Lens.Lens' DescribeConfigurationResponse (Core.Maybe Core.Text)
describeConfigurationResponse_engineVersion = Lens.lens (\DescribeConfigurationResponse' {engineVersion} -> engineVersion) (\s@DescribeConfigurationResponse' {} a -> s {engineVersion = a} :: DescribeConfigurationResponse)

-- | The list of all tags associated with this configuration.
describeConfigurationResponse_tags :: Lens.Lens' DescribeConfigurationResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
describeConfigurationResponse_tags = Lens.lens (\DescribeConfigurationResponse' {tags} -> tags) (\s@DescribeConfigurationResponse' {} a -> s {tags = a} :: DescribeConfigurationResponse) Core.. Lens.mapping Lens._Coerce

-- | Required. The description of the configuration.
describeConfigurationResponse_description :: Lens.Lens' DescribeConfigurationResponse (Core.Maybe Core.Text)
describeConfigurationResponse_description = Lens.lens (\DescribeConfigurationResponse' {description} -> description) (\s@DescribeConfigurationResponse' {} a -> s {description = a} :: DescribeConfigurationResponse)

-- | Required. The date and time of the configuration revision.
describeConfigurationResponse_created :: Lens.Lens' DescribeConfigurationResponse (Core.Maybe Core.UTCTime)
describeConfigurationResponse_created = Lens.lens (\DescribeConfigurationResponse' {created} -> created) (\s@DescribeConfigurationResponse' {} a -> s {created = a} :: DescribeConfigurationResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
describeConfigurationResponse_httpStatus :: Lens.Lens' DescribeConfigurationResponse Core.Int
describeConfigurationResponse_httpStatus = Lens.lens (\DescribeConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeConfigurationResponse)

instance Core.NFData DescribeConfigurationResponse
