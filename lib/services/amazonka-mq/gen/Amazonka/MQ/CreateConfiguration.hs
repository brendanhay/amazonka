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
-- Module      : Amazonka.MQ.CreateConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new configuration for the specified configuration name. Amazon
-- MQ uses the default configuration (the engine type and version).
module Amazonka.MQ.CreateConfiguration
  ( -- * Creating a Request
    CreateConfiguration (..),
    newCreateConfiguration,

    -- * Request Lenses
    createConfiguration_authenticationStrategy,
    createConfiguration_tags,
    createConfiguration_engineVersion,
    createConfiguration_engineType,
    createConfiguration_name,

    -- * Destructuring the Response
    CreateConfigurationResponse (..),
    newCreateConfigurationResponse,

    -- * Response Lenses
    createConfigurationResponse_arn,
    createConfigurationResponse_authenticationStrategy,
    createConfigurationResponse_created,
    createConfigurationResponse_id,
    createConfigurationResponse_latestRevision,
    createConfigurationResponse_name,
    createConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MQ.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Creates a new configuration for the specified configuration name. Amazon
-- MQ uses the default configuration (the engine type and version).
--
-- /See:/ 'newCreateConfiguration' smart constructor.
data CreateConfiguration = CreateConfiguration'
  { -- | Optional. The authentication strategy associated with the configuration.
    -- The default is SIMPLE.
    authenticationStrategy :: Prelude.Maybe AuthenticationStrategy,
    -- | Create tags when creating the configuration.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Required. The broker engine\'s version. For a list of supported engine
    -- versions, see
    -- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker-engine.html Supported engines>.
    engineVersion :: Prelude.Text,
    -- | Required. The type of broker engine. Currently, Amazon MQ supports
    -- ACTIVEMQ and RABBITMQ.
    engineType :: EngineType,
    -- | Required. The name of the configuration. This value can contain only
    -- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
    -- ~). This value must be 1-150 characters long.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationStrategy', 'createConfiguration_authenticationStrategy' - Optional. The authentication strategy associated with the configuration.
-- The default is SIMPLE.
--
-- 'tags', 'createConfiguration_tags' - Create tags when creating the configuration.
--
-- 'engineVersion', 'createConfiguration_engineVersion' - Required. The broker engine\'s version. For a list of supported engine
-- versions, see
-- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker-engine.html Supported engines>.
--
-- 'engineType', 'createConfiguration_engineType' - Required. The type of broker engine. Currently, Amazon MQ supports
-- ACTIVEMQ and RABBITMQ.
--
-- 'name', 'createConfiguration_name' - Required. The name of the configuration. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 1-150 characters long.
newCreateConfiguration ::
  -- | 'engineVersion'
  Prelude.Text ->
  -- | 'engineType'
  EngineType ->
  -- | 'name'
  Prelude.Text ->
  CreateConfiguration
newCreateConfiguration
  pEngineVersion_
  pEngineType_
  pName_ =
    CreateConfiguration'
      { authenticationStrategy =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        engineVersion = pEngineVersion_,
        engineType = pEngineType_,
        name = pName_
      }

-- | Optional. The authentication strategy associated with the configuration.
-- The default is SIMPLE.
createConfiguration_authenticationStrategy :: Lens.Lens' CreateConfiguration (Prelude.Maybe AuthenticationStrategy)
createConfiguration_authenticationStrategy = Lens.lens (\CreateConfiguration' {authenticationStrategy} -> authenticationStrategy) (\s@CreateConfiguration' {} a -> s {authenticationStrategy = a} :: CreateConfiguration)

-- | Create tags when creating the configuration.
createConfiguration_tags :: Lens.Lens' CreateConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createConfiguration_tags = Lens.lens (\CreateConfiguration' {tags} -> tags) (\s@CreateConfiguration' {} a -> s {tags = a} :: CreateConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Required. The broker engine\'s version. For a list of supported engine
-- versions, see
-- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker-engine.html Supported engines>.
createConfiguration_engineVersion :: Lens.Lens' CreateConfiguration Prelude.Text
createConfiguration_engineVersion = Lens.lens (\CreateConfiguration' {engineVersion} -> engineVersion) (\s@CreateConfiguration' {} a -> s {engineVersion = a} :: CreateConfiguration)

-- | Required. The type of broker engine. Currently, Amazon MQ supports
-- ACTIVEMQ and RABBITMQ.
createConfiguration_engineType :: Lens.Lens' CreateConfiguration EngineType
createConfiguration_engineType = Lens.lens (\CreateConfiguration' {engineType} -> engineType) (\s@CreateConfiguration' {} a -> s {engineType = a} :: CreateConfiguration)

-- | Required. The name of the configuration. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 1-150 characters long.
createConfiguration_name :: Lens.Lens' CreateConfiguration Prelude.Text
createConfiguration_name = Lens.lens (\CreateConfiguration' {name} -> name) (\s@CreateConfiguration' {} a -> s {name = a} :: CreateConfiguration)

instance Core.AWSRequest CreateConfiguration where
  type
    AWSResponse CreateConfiguration =
      CreateConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateConfigurationResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "authenticationStrategy")
            Prelude.<*> (x Data..?> "created")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "latestRevision")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateConfiguration where
  hashWithSalt _salt CreateConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` authenticationStrategy
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` engineType
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateConfiguration where
  rnf CreateConfiguration' {..} =
    Prelude.rnf authenticationStrategy
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf engineType
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateConfiguration where
  toJSON CreateConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("authenticationStrategy" Data..=)
              Prelude.<$> authenticationStrategy,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("engineVersion" Data..= engineVersion),
            Prelude.Just ("engineType" Data..= engineType),
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateConfiguration where
  toPath = Prelude.const "/v1/configurations"

instance Data.ToQuery CreateConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateConfigurationResponse' smart constructor.
data CreateConfigurationResponse = CreateConfigurationResponse'
  { -- | Required. The Amazon Resource Name (ARN) of the configuration.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Optional. The authentication strategy associated with the configuration.
    -- The default is SIMPLE.
    authenticationStrategy :: Prelude.Maybe AuthenticationStrategy,
    -- | Required. The date and time of the configuration.
    created :: Prelude.Maybe Data.ISO8601,
    -- | Required. The unique ID that Amazon MQ generates for the configuration.
    id :: Prelude.Maybe Prelude.Text,
    -- | The latest revision of the configuration.
    latestRevision :: Prelude.Maybe ConfigurationRevision,
    -- | Required. The name of the configuration. This value can contain only
    -- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
    -- ~). This value must be 1-150 characters long.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createConfigurationResponse_arn' - Required. The Amazon Resource Name (ARN) of the configuration.
--
-- 'authenticationStrategy', 'createConfigurationResponse_authenticationStrategy' - Optional. The authentication strategy associated with the configuration.
-- The default is SIMPLE.
--
-- 'created', 'createConfigurationResponse_created' - Required. The date and time of the configuration.
--
-- 'id', 'createConfigurationResponse_id' - Required. The unique ID that Amazon MQ generates for the configuration.
--
-- 'latestRevision', 'createConfigurationResponse_latestRevision' - The latest revision of the configuration.
--
-- 'name', 'createConfigurationResponse_name' - Required. The name of the configuration. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 1-150 characters long.
--
-- 'httpStatus', 'createConfigurationResponse_httpStatus' - The response's http status code.
newCreateConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateConfigurationResponse
newCreateConfigurationResponse pHttpStatus_ =
  CreateConfigurationResponse'
    { arn = Prelude.Nothing,
      authenticationStrategy = Prelude.Nothing,
      created = Prelude.Nothing,
      id = Prelude.Nothing,
      latestRevision = Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Required. The Amazon Resource Name (ARN) of the configuration.
createConfigurationResponse_arn :: Lens.Lens' CreateConfigurationResponse (Prelude.Maybe Prelude.Text)
createConfigurationResponse_arn = Lens.lens (\CreateConfigurationResponse' {arn} -> arn) (\s@CreateConfigurationResponse' {} a -> s {arn = a} :: CreateConfigurationResponse)

-- | Optional. The authentication strategy associated with the configuration.
-- The default is SIMPLE.
createConfigurationResponse_authenticationStrategy :: Lens.Lens' CreateConfigurationResponse (Prelude.Maybe AuthenticationStrategy)
createConfigurationResponse_authenticationStrategy = Lens.lens (\CreateConfigurationResponse' {authenticationStrategy} -> authenticationStrategy) (\s@CreateConfigurationResponse' {} a -> s {authenticationStrategy = a} :: CreateConfigurationResponse)

-- | Required. The date and time of the configuration.
createConfigurationResponse_created :: Lens.Lens' CreateConfigurationResponse (Prelude.Maybe Prelude.UTCTime)
createConfigurationResponse_created = Lens.lens (\CreateConfigurationResponse' {created} -> created) (\s@CreateConfigurationResponse' {} a -> s {created = a} :: CreateConfigurationResponse) Prelude.. Lens.mapping Data._Time

-- | Required. The unique ID that Amazon MQ generates for the configuration.
createConfigurationResponse_id :: Lens.Lens' CreateConfigurationResponse (Prelude.Maybe Prelude.Text)
createConfigurationResponse_id = Lens.lens (\CreateConfigurationResponse' {id} -> id) (\s@CreateConfigurationResponse' {} a -> s {id = a} :: CreateConfigurationResponse)

-- | The latest revision of the configuration.
createConfigurationResponse_latestRevision :: Lens.Lens' CreateConfigurationResponse (Prelude.Maybe ConfigurationRevision)
createConfigurationResponse_latestRevision = Lens.lens (\CreateConfigurationResponse' {latestRevision} -> latestRevision) (\s@CreateConfigurationResponse' {} a -> s {latestRevision = a} :: CreateConfigurationResponse)

-- | Required. The name of the configuration. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 1-150 characters long.
createConfigurationResponse_name :: Lens.Lens' CreateConfigurationResponse (Prelude.Maybe Prelude.Text)
createConfigurationResponse_name = Lens.lens (\CreateConfigurationResponse' {name} -> name) (\s@CreateConfigurationResponse' {} a -> s {name = a} :: CreateConfigurationResponse)

-- | The response's http status code.
createConfigurationResponse_httpStatus :: Lens.Lens' CreateConfigurationResponse Prelude.Int
createConfigurationResponse_httpStatus = Lens.lens (\CreateConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateConfigurationResponse' {} a -> s {httpStatus = a} :: CreateConfigurationResponse)

instance Prelude.NFData CreateConfigurationResponse where
  rnf CreateConfigurationResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf authenticationStrategy
      `Prelude.seq` Prelude.rnf created
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf latestRevision
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus
