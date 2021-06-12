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
-- Module      : Network.AWS.MQ.CreateConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new configuration for the specified configuration name. Amazon
-- MQ uses the default configuration (the engine type and version).
module Network.AWS.MQ.CreateConfiguration
  ( -- * Creating a Request
    CreateConfiguration (..),
    newCreateConfiguration,

    -- * Request Lenses
    createConfiguration_engineType,
    createConfiguration_authenticationStrategy,
    createConfiguration_name,
    createConfiguration_engineVersion,
    createConfiguration_tags,

    -- * Destructuring the Response
    CreateConfigurationResponse (..),
    newCreateConfigurationResponse,

    -- * Response Lenses
    createConfigurationResponse_authenticationStrategy,
    createConfigurationResponse_latestRevision,
    createConfigurationResponse_arn,
    createConfigurationResponse_id,
    createConfigurationResponse_name,
    createConfigurationResponse_created,
    createConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Creates a new configuration for the specified configuration name. Amazon
-- MQ uses the default configuration (the engine type and version).
--
-- /See:/ 'newCreateConfiguration' smart constructor.
data CreateConfiguration = CreateConfiguration'
  { -- | Required. The type of broker engine. Note: Currently, Amazon MQ supports
    -- ACTIVEMQ and RABBITMQ.
    engineType :: Core.Maybe EngineType,
    -- | The authentication strategy associated with the configuration.
    authenticationStrategy :: Core.Maybe AuthenticationStrategy,
    -- | Required. The name of the configuration. This value can contain only
    -- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
    -- ~). This value must be 1-150 characters long.
    name :: Core.Maybe Core.Text,
    -- | Required. The version of the broker engine. For a list of supported
    -- engine versions, see
    -- https:\/\/docs.aws.amazon.com\/amazon-mq\/latest\/developer-guide\/broker-engine.html
    engineVersion :: Core.Maybe Core.Text,
    -- | Create tags when creating the configuration.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineType', 'createConfiguration_engineType' - Required. The type of broker engine. Note: Currently, Amazon MQ supports
-- ACTIVEMQ and RABBITMQ.
--
-- 'authenticationStrategy', 'createConfiguration_authenticationStrategy' - The authentication strategy associated with the configuration.
--
-- 'name', 'createConfiguration_name' - Required. The name of the configuration. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 1-150 characters long.
--
-- 'engineVersion', 'createConfiguration_engineVersion' - Required. The version of the broker engine. For a list of supported
-- engine versions, see
-- https:\/\/docs.aws.amazon.com\/amazon-mq\/latest\/developer-guide\/broker-engine.html
--
-- 'tags', 'createConfiguration_tags' - Create tags when creating the configuration.
newCreateConfiguration ::
  CreateConfiguration
newCreateConfiguration =
  CreateConfiguration'
    { engineType = Core.Nothing,
      authenticationStrategy = Core.Nothing,
      name = Core.Nothing,
      engineVersion = Core.Nothing,
      tags = Core.Nothing
    }

-- | Required. The type of broker engine. Note: Currently, Amazon MQ supports
-- ACTIVEMQ and RABBITMQ.
createConfiguration_engineType :: Lens.Lens' CreateConfiguration (Core.Maybe EngineType)
createConfiguration_engineType = Lens.lens (\CreateConfiguration' {engineType} -> engineType) (\s@CreateConfiguration' {} a -> s {engineType = a} :: CreateConfiguration)

-- | The authentication strategy associated with the configuration.
createConfiguration_authenticationStrategy :: Lens.Lens' CreateConfiguration (Core.Maybe AuthenticationStrategy)
createConfiguration_authenticationStrategy = Lens.lens (\CreateConfiguration' {authenticationStrategy} -> authenticationStrategy) (\s@CreateConfiguration' {} a -> s {authenticationStrategy = a} :: CreateConfiguration)

-- | Required. The name of the configuration. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 1-150 characters long.
createConfiguration_name :: Lens.Lens' CreateConfiguration (Core.Maybe Core.Text)
createConfiguration_name = Lens.lens (\CreateConfiguration' {name} -> name) (\s@CreateConfiguration' {} a -> s {name = a} :: CreateConfiguration)

-- | Required. The version of the broker engine. For a list of supported
-- engine versions, see
-- https:\/\/docs.aws.amazon.com\/amazon-mq\/latest\/developer-guide\/broker-engine.html
createConfiguration_engineVersion :: Lens.Lens' CreateConfiguration (Core.Maybe Core.Text)
createConfiguration_engineVersion = Lens.lens (\CreateConfiguration' {engineVersion} -> engineVersion) (\s@CreateConfiguration' {} a -> s {engineVersion = a} :: CreateConfiguration)

-- | Create tags when creating the configuration.
createConfiguration_tags :: Lens.Lens' CreateConfiguration (Core.Maybe (Core.HashMap Core.Text Core.Text))
createConfiguration_tags = Lens.lens (\CreateConfiguration' {tags} -> tags) (\s@CreateConfiguration' {} a -> s {tags = a} :: CreateConfiguration) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest CreateConfiguration where
  type
    AWSResponse CreateConfiguration =
      CreateConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateConfigurationResponse'
            Core.<$> (x Core..?> "authenticationStrategy")
            Core.<*> (x Core..?> "latestRevision")
            Core.<*> (x Core..?> "arn")
            Core.<*> (x Core..?> "id")
            Core.<*> (x Core..?> "name")
            Core.<*> (x Core..?> "created")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateConfiguration

instance Core.NFData CreateConfiguration

instance Core.ToHeaders CreateConfiguration where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateConfiguration where
  toJSON CreateConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("engineType" Core..=) Core.<$> engineType,
            ("authenticationStrategy" Core..=)
              Core.<$> authenticationStrategy,
            ("name" Core..=) Core.<$> name,
            ("engineVersion" Core..=) Core.<$> engineVersion,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.ToPath CreateConfiguration where
  toPath = Core.const "/v1/configurations"

instance Core.ToQuery CreateConfiguration where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateConfigurationResponse' smart constructor.
data CreateConfigurationResponse = CreateConfigurationResponse'
  { -- | The authentication strategy associated with the configuration.
    authenticationStrategy :: Core.Maybe AuthenticationStrategy,
    -- | The latest revision of the configuration.
    latestRevision :: Core.Maybe ConfigurationRevision,
    -- | Required. The Amazon Resource Name (ARN) of the configuration.
    arn :: Core.Maybe Core.Text,
    -- | Required. The unique ID that Amazon MQ generates for the configuration.
    id :: Core.Maybe Core.Text,
    -- | Required. The name of the configuration. This value can contain only
    -- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
    -- ~). This value must be 1-150 characters long.
    name :: Core.Maybe Core.Text,
    -- | Required. The date and time of the configuration.
    created :: Core.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationStrategy', 'createConfigurationResponse_authenticationStrategy' - The authentication strategy associated with the configuration.
--
-- 'latestRevision', 'createConfigurationResponse_latestRevision' - The latest revision of the configuration.
--
-- 'arn', 'createConfigurationResponse_arn' - Required. The Amazon Resource Name (ARN) of the configuration.
--
-- 'id', 'createConfigurationResponse_id' - Required. The unique ID that Amazon MQ generates for the configuration.
--
-- 'name', 'createConfigurationResponse_name' - Required. The name of the configuration. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 1-150 characters long.
--
-- 'created', 'createConfigurationResponse_created' - Required. The date and time of the configuration.
--
-- 'httpStatus', 'createConfigurationResponse_httpStatus' - The response's http status code.
newCreateConfigurationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateConfigurationResponse
newCreateConfigurationResponse pHttpStatus_ =
  CreateConfigurationResponse'
    { authenticationStrategy =
        Core.Nothing,
      latestRevision = Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing,
      created = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The authentication strategy associated with the configuration.
createConfigurationResponse_authenticationStrategy :: Lens.Lens' CreateConfigurationResponse (Core.Maybe AuthenticationStrategy)
createConfigurationResponse_authenticationStrategy = Lens.lens (\CreateConfigurationResponse' {authenticationStrategy} -> authenticationStrategy) (\s@CreateConfigurationResponse' {} a -> s {authenticationStrategy = a} :: CreateConfigurationResponse)

-- | The latest revision of the configuration.
createConfigurationResponse_latestRevision :: Lens.Lens' CreateConfigurationResponse (Core.Maybe ConfigurationRevision)
createConfigurationResponse_latestRevision = Lens.lens (\CreateConfigurationResponse' {latestRevision} -> latestRevision) (\s@CreateConfigurationResponse' {} a -> s {latestRevision = a} :: CreateConfigurationResponse)

-- | Required. The Amazon Resource Name (ARN) of the configuration.
createConfigurationResponse_arn :: Lens.Lens' CreateConfigurationResponse (Core.Maybe Core.Text)
createConfigurationResponse_arn = Lens.lens (\CreateConfigurationResponse' {arn} -> arn) (\s@CreateConfigurationResponse' {} a -> s {arn = a} :: CreateConfigurationResponse)

-- | Required. The unique ID that Amazon MQ generates for the configuration.
createConfigurationResponse_id :: Lens.Lens' CreateConfigurationResponse (Core.Maybe Core.Text)
createConfigurationResponse_id = Lens.lens (\CreateConfigurationResponse' {id} -> id) (\s@CreateConfigurationResponse' {} a -> s {id = a} :: CreateConfigurationResponse)

-- | Required. The name of the configuration. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 1-150 characters long.
createConfigurationResponse_name :: Lens.Lens' CreateConfigurationResponse (Core.Maybe Core.Text)
createConfigurationResponse_name = Lens.lens (\CreateConfigurationResponse' {name} -> name) (\s@CreateConfigurationResponse' {} a -> s {name = a} :: CreateConfigurationResponse)

-- | Required. The date and time of the configuration.
createConfigurationResponse_created :: Lens.Lens' CreateConfigurationResponse (Core.Maybe Core.UTCTime)
createConfigurationResponse_created = Lens.lens (\CreateConfigurationResponse' {created} -> created) (\s@CreateConfigurationResponse' {} a -> s {created = a} :: CreateConfigurationResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
createConfigurationResponse_httpStatus :: Lens.Lens' CreateConfigurationResponse Core.Int
createConfigurationResponse_httpStatus = Lens.lens (\CreateConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateConfigurationResponse' {} a -> s {httpStatus = a} :: CreateConfigurationResponse)

instance Core.NFData CreateConfigurationResponse
