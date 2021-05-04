{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Creates a new configuration for the specified configuration name. Amazon
-- MQ uses the default configuration (the engine type and version).
--
-- /See:/ 'newCreateConfiguration' smart constructor.
data CreateConfiguration = CreateConfiguration'
  { -- | Required. The type of broker engine. Note: Currently, Amazon MQ supports
    -- ACTIVEMQ and RABBITMQ.
    engineType :: Prelude.Maybe EngineType,
    -- | The authentication strategy associated with the configuration.
    authenticationStrategy :: Prelude.Maybe AuthenticationStrategy,
    -- | Required. The name of the configuration. This value can contain only
    -- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
    -- ~). This value must be 1-150 characters long.
    name :: Prelude.Maybe Prelude.Text,
    -- | Required. The version of the broker engine. For a list of supported
    -- engine versions, see
    -- https:\/\/docs.aws.amazon.com\/amazon-mq\/latest\/developer-guide\/broker-engine.html
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | Create tags when creating the configuration.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { engineType = Prelude.Nothing,
      authenticationStrategy = Prelude.Nothing,
      name = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Required. The type of broker engine. Note: Currently, Amazon MQ supports
-- ACTIVEMQ and RABBITMQ.
createConfiguration_engineType :: Lens.Lens' CreateConfiguration (Prelude.Maybe EngineType)
createConfiguration_engineType = Lens.lens (\CreateConfiguration' {engineType} -> engineType) (\s@CreateConfiguration' {} a -> s {engineType = a} :: CreateConfiguration)

-- | The authentication strategy associated with the configuration.
createConfiguration_authenticationStrategy :: Lens.Lens' CreateConfiguration (Prelude.Maybe AuthenticationStrategy)
createConfiguration_authenticationStrategy = Lens.lens (\CreateConfiguration' {authenticationStrategy} -> authenticationStrategy) (\s@CreateConfiguration' {} a -> s {authenticationStrategy = a} :: CreateConfiguration)

-- | Required. The name of the configuration. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 1-150 characters long.
createConfiguration_name :: Lens.Lens' CreateConfiguration (Prelude.Maybe Prelude.Text)
createConfiguration_name = Lens.lens (\CreateConfiguration' {name} -> name) (\s@CreateConfiguration' {} a -> s {name = a} :: CreateConfiguration)

-- | Required. The version of the broker engine. For a list of supported
-- engine versions, see
-- https:\/\/docs.aws.amazon.com\/amazon-mq\/latest\/developer-guide\/broker-engine.html
createConfiguration_engineVersion :: Lens.Lens' CreateConfiguration (Prelude.Maybe Prelude.Text)
createConfiguration_engineVersion = Lens.lens (\CreateConfiguration' {engineVersion} -> engineVersion) (\s@CreateConfiguration' {} a -> s {engineVersion = a} :: CreateConfiguration)

-- | Create tags when creating the configuration.
createConfiguration_tags :: Lens.Lens' CreateConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createConfiguration_tags = Lens.lens (\CreateConfiguration' {tags} -> tags) (\s@CreateConfiguration' {} a -> s {tags = a} :: CreateConfiguration) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.AWSRequest CreateConfiguration where
  type
    Rs CreateConfiguration =
      CreateConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateConfigurationResponse'
            Prelude.<$> (x Prelude..?> "authenticationStrategy")
            Prelude.<*> (x Prelude..?> "latestRevision")
            Prelude.<*> (x Prelude..?> "arn")
            Prelude.<*> (x Prelude..?> "id")
            Prelude.<*> (x Prelude..?> "name")
            Prelude.<*> (x Prelude..?> "created")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateConfiguration

instance Prelude.NFData CreateConfiguration

instance Prelude.ToHeaders CreateConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateConfiguration where
  toJSON CreateConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("engineType" Prelude..=) Prelude.<$> engineType,
            ("authenticationStrategy" Prelude..=)
              Prelude.<$> authenticationStrategy,
            ("name" Prelude..=) Prelude.<$> name,
            ("engineVersion" Prelude..=)
              Prelude.<$> engineVersion,
            ("tags" Prelude..=) Prelude.<$> tags
          ]
      )

instance Prelude.ToPath CreateConfiguration where
  toPath = Prelude.const "/v1/configurations"

instance Prelude.ToQuery CreateConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateConfigurationResponse' smart constructor.
data CreateConfigurationResponse = CreateConfigurationResponse'
  { -- | The authentication strategy associated with the configuration.
    authenticationStrategy :: Prelude.Maybe AuthenticationStrategy,
    -- | The latest revision of the configuration.
    latestRevision :: Prelude.Maybe ConfigurationRevision,
    -- | Required. The Amazon Resource Name (ARN) of the configuration.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Required. The unique ID that Amazon MQ generates for the configuration.
    id :: Prelude.Maybe Prelude.Text,
    -- | Required. The name of the configuration. This value can contain only
    -- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
    -- ~). This value must be 1-150 characters long.
    name :: Prelude.Maybe Prelude.Text,
    -- | Required. The date and time of the configuration.
    created :: Prelude.Maybe Prelude.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreateConfigurationResponse
newCreateConfigurationResponse pHttpStatus_ =
  CreateConfigurationResponse'
    { authenticationStrategy =
        Prelude.Nothing,
      latestRevision = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      created = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The authentication strategy associated with the configuration.
createConfigurationResponse_authenticationStrategy :: Lens.Lens' CreateConfigurationResponse (Prelude.Maybe AuthenticationStrategy)
createConfigurationResponse_authenticationStrategy = Lens.lens (\CreateConfigurationResponse' {authenticationStrategy} -> authenticationStrategy) (\s@CreateConfigurationResponse' {} a -> s {authenticationStrategy = a} :: CreateConfigurationResponse)

-- | The latest revision of the configuration.
createConfigurationResponse_latestRevision :: Lens.Lens' CreateConfigurationResponse (Prelude.Maybe ConfigurationRevision)
createConfigurationResponse_latestRevision = Lens.lens (\CreateConfigurationResponse' {latestRevision} -> latestRevision) (\s@CreateConfigurationResponse' {} a -> s {latestRevision = a} :: CreateConfigurationResponse)

-- | Required. The Amazon Resource Name (ARN) of the configuration.
createConfigurationResponse_arn :: Lens.Lens' CreateConfigurationResponse (Prelude.Maybe Prelude.Text)
createConfigurationResponse_arn = Lens.lens (\CreateConfigurationResponse' {arn} -> arn) (\s@CreateConfigurationResponse' {} a -> s {arn = a} :: CreateConfigurationResponse)

-- | Required. The unique ID that Amazon MQ generates for the configuration.
createConfigurationResponse_id :: Lens.Lens' CreateConfigurationResponse (Prelude.Maybe Prelude.Text)
createConfigurationResponse_id = Lens.lens (\CreateConfigurationResponse' {id} -> id) (\s@CreateConfigurationResponse' {} a -> s {id = a} :: CreateConfigurationResponse)

-- | Required. The name of the configuration. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 1-150 characters long.
createConfigurationResponse_name :: Lens.Lens' CreateConfigurationResponse (Prelude.Maybe Prelude.Text)
createConfigurationResponse_name = Lens.lens (\CreateConfigurationResponse' {name} -> name) (\s@CreateConfigurationResponse' {} a -> s {name = a} :: CreateConfigurationResponse)

-- | Required. The date and time of the configuration.
createConfigurationResponse_created :: Lens.Lens' CreateConfigurationResponse (Prelude.Maybe Prelude.UTCTime)
createConfigurationResponse_created = Lens.lens (\CreateConfigurationResponse' {created} -> created) (\s@CreateConfigurationResponse' {} a -> s {created = a} :: CreateConfigurationResponse) Prelude.. Lens.mapping Prelude._Time

-- | The response's http status code.
createConfigurationResponse_httpStatus :: Lens.Lens' CreateConfigurationResponse Prelude.Int
createConfigurationResponse_httpStatus = Lens.lens (\CreateConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateConfigurationResponse' {} a -> s {httpStatus = a} :: CreateConfigurationResponse)

instance Prelude.NFData CreateConfigurationResponse
