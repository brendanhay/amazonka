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
-- Module      : Amazonka.Kafka.UpdateConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an MSK configuration.
module Amazonka.Kafka.UpdateConfiguration
  ( -- * Creating a Request
    UpdateConfiguration (..),
    newUpdateConfiguration,

    -- * Request Lenses
    updateConfiguration_description,
    updateConfiguration_arn,
    updateConfiguration_serverProperties,

    -- * Destructuring the Response
    UpdateConfigurationResponse (..),
    newUpdateConfigurationResponse,

    -- * Response Lenses
    updateConfigurationResponse_arn,
    updateConfigurationResponse_latestRevision,
    updateConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateConfiguration' smart constructor.
data UpdateConfiguration = UpdateConfiguration'
  { -- | The description of the configuration revision.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the configuration.
    arn :: Prelude.Text,
    -- | Contents of the server.properties file. When using the API, you must
    -- ensure that the contents of the file are base64 encoded. When using the
    -- AWS Management Console, the SDK, or the AWS CLI, the contents of
    -- server.properties can be in plaintext.
    serverProperties :: Data.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateConfiguration_description' - The description of the configuration revision.
--
-- 'arn', 'updateConfiguration_arn' - The Amazon Resource Name (ARN) of the configuration.
--
-- 'serverProperties', 'updateConfiguration_serverProperties' - Contents of the server.properties file. When using the API, you must
-- ensure that the contents of the file are base64 encoded. When using the
-- AWS Management Console, the SDK, or the AWS CLI, the contents of
-- server.properties can be in plaintext.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newUpdateConfiguration ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'serverProperties'
  Prelude.ByteString ->
  UpdateConfiguration
newUpdateConfiguration pArn_ pServerProperties_ =
  UpdateConfiguration'
    { description = Prelude.Nothing,
      arn = pArn_,
      serverProperties =
        Data._Base64 Lens.# pServerProperties_
    }

-- | The description of the configuration revision.
updateConfiguration_description :: Lens.Lens' UpdateConfiguration (Prelude.Maybe Prelude.Text)
updateConfiguration_description = Lens.lens (\UpdateConfiguration' {description} -> description) (\s@UpdateConfiguration' {} a -> s {description = a} :: UpdateConfiguration)

-- | The Amazon Resource Name (ARN) of the configuration.
updateConfiguration_arn :: Lens.Lens' UpdateConfiguration Prelude.Text
updateConfiguration_arn = Lens.lens (\UpdateConfiguration' {arn} -> arn) (\s@UpdateConfiguration' {} a -> s {arn = a} :: UpdateConfiguration)

-- | Contents of the server.properties file. When using the API, you must
-- ensure that the contents of the file are base64 encoded. When using the
-- AWS Management Console, the SDK, or the AWS CLI, the contents of
-- server.properties can be in plaintext.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
updateConfiguration_serverProperties :: Lens.Lens' UpdateConfiguration Prelude.ByteString
updateConfiguration_serverProperties = Lens.lens (\UpdateConfiguration' {serverProperties} -> serverProperties) (\s@UpdateConfiguration' {} a -> s {serverProperties = a} :: UpdateConfiguration) Prelude.. Data._Base64

instance Core.AWSRequest UpdateConfiguration where
  type
    AWSResponse UpdateConfiguration =
      UpdateConfigurationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateConfigurationResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "latestRevision")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateConfiguration where
  hashWithSalt _salt UpdateConfiguration' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` serverProperties

instance Prelude.NFData UpdateConfiguration where
  rnf UpdateConfiguration' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf serverProperties

instance Data.ToHeaders UpdateConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateConfiguration where
  toJSON UpdateConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            Prelude.Just
              ("serverProperties" Data..= serverProperties)
          ]
      )

instance Data.ToPath UpdateConfiguration where
  toPath UpdateConfiguration' {..} =
    Prelude.mconcat
      ["/v1/configurations/", Data.toBS arn]

instance Data.ToQuery UpdateConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateConfigurationResponse' smart constructor.
data UpdateConfigurationResponse = UpdateConfigurationResponse'
  { -- | The Amazon Resource Name (ARN) of the configuration.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Latest revision of the configuration.
    latestRevision :: Prelude.Maybe ConfigurationRevision,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateConfigurationResponse_arn' - The Amazon Resource Name (ARN) of the configuration.
--
-- 'latestRevision', 'updateConfigurationResponse_latestRevision' - Latest revision of the configuration.
--
-- 'httpStatus', 'updateConfigurationResponse_httpStatus' - The response's http status code.
newUpdateConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateConfigurationResponse
newUpdateConfigurationResponse pHttpStatus_ =
  UpdateConfigurationResponse'
    { arn = Prelude.Nothing,
      latestRevision = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the configuration.
updateConfigurationResponse_arn :: Lens.Lens' UpdateConfigurationResponse (Prelude.Maybe Prelude.Text)
updateConfigurationResponse_arn = Lens.lens (\UpdateConfigurationResponse' {arn} -> arn) (\s@UpdateConfigurationResponse' {} a -> s {arn = a} :: UpdateConfigurationResponse)

-- | Latest revision of the configuration.
updateConfigurationResponse_latestRevision :: Lens.Lens' UpdateConfigurationResponse (Prelude.Maybe ConfigurationRevision)
updateConfigurationResponse_latestRevision = Lens.lens (\UpdateConfigurationResponse' {latestRevision} -> latestRevision) (\s@UpdateConfigurationResponse' {} a -> s {latestRevision = a} :: UpdateConfigurationResponse)

-- | The response's http status code.
updateConfigurationResponse_httpStatus :: Lens.Lens' UpdateConfigurationResponse Prelude.Int
updateConfigurationResponse_httpStatus = Lens.lens (\UpdateConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateConfigurationResponse)

instance Prelude.NFData UpdateConfigurationResponse where
  rnf UpdateConfigurationResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf latestRevision
      `Prelude.seq` Prelude.rnf httpStatus
