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
-- Module      : Amazonka.Kafka.CreateConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new MSK configuration.
module Amazonka.Kafka.CreateConfiguration
  ( -- * Creating a Request
    CreateConfiguration (..),
    newCreateConfiguration,

    -- * Request Lenses
    createConfiguration_description,
    createConfiguration_kafkaVersions,
    createConfiguration_serverProperties,
    createConfiguration_name,

    -- * Destructuring the Response
    CreateConfigurationResponse (..),
    newCreateConfigurationResponse,

    -- * Response Lenses
    createConfigurationResponse_arn,
    createConfigurationResponse_creationTime,
    createConfigurationResponse_latestRevision,
    createConfigurationResponse_name,
    createConfigurationResponse_state,
    createConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateConfiguration' smart constructor.
data CreateConfiguration = CreateConfiguration'
  { -- | The description of the configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | The versions of Apache Kafka with which you can use this MSK
    -- configuration.
    kafkaVersions :: Prelude.Maybe [Prelude.Text],
    -- | Contents of the server.properties file. When using the API, you must
    -- ensure that the contents of the file are base64 encoded. When using the
    -- AWS Management Console, the SDK, or the AWS CLI, the contents of
    -- server.properties can be in plaintext.
    serverProperties :: Data.Base64,
    -- | The name of the configuration.
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
-- 'description', 'createConfiguration_description' - The description of the configuration.
--
-- 'kafkaVersions', 'createConfiguration_kafkaVersions' - The versions of Apache Kafka with which you can use this MSK
-- configuration.
--
-- 'serverProperties', 'createConfiguration_serverProperties' - Contents of the server.properties file. When using the API, you must
-- ensure that the contents of the file are base64 encoded. When using the
-- AWS Management Console, the SDK, or the AWS CLI, the contents of
-- server.properties can be in plaintext.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'name', 'createConfiguration_name' - The name of the configuration.
newCreateConfiguration ::
  -- | 'serverProperties'
  Prelude.ByteString ->
  -- | 'name'
  Prelude.Text ->
  CreateConfiguration
newCreateConfiguration pServerProperties_ pName_ =
  CreateConfiguration'
    { description = Prelude.Nothing,
      kafkaVersions = Prelude.Nothing,
      serverProperties =
        Data._Base64 Lens.# pServerProperties_,
      name = pName_
    }

-- | The description of the configuration.
createConfiguration_description :: Lens.Lens' CreateConfiguration (Prelude.Maybe Prelude.Text)
createConfiguration_description = Lens.lens (\CreateConfiguration' {description} -> description) (\s@CreateConfiguration' {} a -> s {description = a} :: CreateConfiguration)

-- | The versions of Apache Kafka with which you can use this MSK
-- configuration.
createConfiguration_kafkaVersions :: Lens.Lens' CreateConfiguration (Prelude.Maybe [Prelude.Text])
createConfiguration_kafkaVersions = Lens.lens (\CreateConfiguration' {kafkaVersions} -> kafkaVersions) (\s@CreateConfiguration' {} a -> s {kafkaVersions = a} :: CreateConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Contents of the server.properties file. When using the API, you must
-- ensure that the contents of the file are base64 encoded. When using the
-- AWS Management Console, the SDK, or the AWS CLI, the contents of
-- server.properties can be in plaintext.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
createConfiguration_serverProperties :: Lens.Lens' CreateConfiguration Prelude.ByteString
createConfiguration_serverProperties = Lens.lens (\CreateConfiguration' {serverProperties} -> serverProperties) (\s@CreateConfiguration' {} a -> s {serverProperties = a} :: CreateConfiguration) Prelude.. Data._Base64

-- | The name of the configuration.
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
            Prelude.<*> (x Data..?> "creationTime")
            Prelude.<*> (x Data..?> "latestRevision")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "state")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateConfiguration where
  hashWithSalt _salt CreateConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` kafkaVersions
      `Prelude.hashWithSalt` serverProperties
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateConfiguration where
  rnf CreateConfiguration' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf kafkaVersions
      `Prelude.seq` Prelude.rnf serverProperties
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
          [ ("description" Data..=) Prelude.<$> description,
            ("kafkaVersions" Data..=) Prelude.<$> kafkaVersions,
            Prelude.Just
              ("serverProperties" Data..= serverProperties),
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateConfiguration where
  toPath = Prelude.const "/v1/configurations"

instance Data.ToQuery CreateConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateConfigurationResponse' smart constructor.
data CreateConfigurationResponse = CreateConfigurationResponse'
  { -- | The Amazon Resource Name (ARN) of the configuration.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time when the configuration was created.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | Latest revision of the configuration.
    latestRevision :: Prelude.Maybe ConfigurationRevision,
    -- | The name of the configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | The state of the configuration. The possible states are ACTIVE,
    -- DELETING, and DELETE_FAILED.
    state :: Prelude.Maybe ConfigurationState,
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
-- 'arn', 'createConfigurationResponse_arn' - The Amazon Resource Name (ARN) of the configuration.
--
-- 'creationTime', 'createConfigurationResponse_creationTime' - The time when the configuration was created.
--
-- 'latestRevision', 'createConfigurationResponse_latestRevision' - Latest revision of the configuration.
--
-- 'name', 'createConfigurationResponse_name' - The name of the configuration.
--
-- 'state', 'createConfigurationResponse_state' - The state of the configuration. The possible states are ACTIVE,
-- DELETING, and DELETE_FAILED.
--
-- 'httpStatus', 'createConfigurationResponse_httpStatus' - The response's http status code.
newCreateConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateConfigurationResponse
newCreateConfigurationResponse pHttpStatus_ =
  CreateConfigurationResponse'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      latestRevision = Prelude.Nothing,
      name = Prelude.Nothing,
      state = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the configuration.
createConfigurationResponse_arn :: Lens.Lens' CreateConfigurationResponse (Prelude.Maybe Prelude.Text)
createConfigurationResponse_arn = Lens.lens (\CreateConfigurationResponse' {arn} -> arn) (\s@CreateConfigurationResponse' {} a -> s {arn = a} :: CreateConfigurationResponse)

-- | The time when the configuration was created.
createConfigurationResponse_creationTime :: Lens.Lens' CreateConfigurationResponse (Prelude.Maybe Prelude.UTCTime)
createConfigurationResponse_creationTime = Lens.lens (\CreateConfigurationResponse' {creationTime} -> creationTime) (\s@CreateConfigurationResponse' {} a -> s {creationTime = a} :: CreateConfigurationResponse) Prelude.. Lens.mapping Data._Time

-- | Latest revision of the configuration.
createConfigurationResponse_latestRevision :: Lens.Lens' CreateConfigurationResponse (Prelude.Maybe ConfigurationRevision)
createConfigurationResponse_latestRevision = Lens.lens (\CreateConfigurationResponse' {latestRevision} -> latestRevision) (\s@CreateConfigurationResponse' {} a -> s {latestRevision = a} :: CreateConfigurationResponse)

-- | The name of the configuration.
createConfigurationResponse_name :: Lens.Lens' CreateConfigurationResponse (Prelude.Maybe Prelude.Text)
createConfigurationResponse_name = Lens.lens (\CreateConfigurationResponse' {name} -> name) (\s@CreateConfigurationResponse' {} a -> s {name = a} :: CreateConfigurationResponse)

-- | The state of the configuration. The possible states are ACTIVE,
-- DELETING, and DELETE_FAILED.
createConfigurationResponse_state :: Lens.Lens' CreateConfigurationResponse (Prelude.Maybe ConfigurationState)
createConfigurationResponse_state = Lens.lens (\CreateConfigurationResponse' {state} -> state) (\s@CreateConfigurationResponse' {} a -> s {state = a} :: CreateConfigurationResponse)

-- | The response's http status code.
createConfigurationResponse_httpStatus :: Lens.Lens' CreateConfigurationResponse Prelude.Int
createConfigurationResponse_httpStatus = Lens.lens (\CreateConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateConfigurationResponse' {} a -> s {httpStatus = a} :: CreateConfigurationResponse)

instance Prelude.NFData CreateConfigurationResponse where
  rnf CreateConfigurationResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf latestRevision
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf httpStatus
