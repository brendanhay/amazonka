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
-- Module      : Amazonka.IVSChat.UpdateLoggingConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified logging configuration.
module Amazonka.IVSChat.UpdateLoggingConfiguration
  ( -- * Creating a Request
    UpdateLoggingConfiguration (..),
    newUpdateLoggingConfiguration,

    -- * Request Lenses
    updateLoggingConfiguration_destinationConfiguration,
    updateLoggingConfiguration_name,
    updateLoggingConfiguration_identifier,

    -- * Destructuring the Response
    UpdateLoggingConfigurationResponse (..),
    newUpdateLoggingConfigurationResponse,

    -- * Response Lenses
    updateLoggingConfigurationResponse_arn,
    updateLoggingConfigurationResponse_createTime,
    updateLoggingConfigurationResponse_destinationConfiguration,
    updateLoggingConfigurationResponse_id,
    updateLoggingConfigurationResponse_name,
    updateLoggingConfigurationResponse_state,
    updateLoggingConfigurationResponse_tags,
    updateLoggingConfigurationResponse_updateTime,
    updateLoggingConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVSChat.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateLoggingConfiguration' smart constructor.
data UpdateLoggingConfiguration = UpdateLoggingConfiguration'
  { -- | A complex type that contains a destination configuration for where chat
    -- content will be logged. There can be only one type of destination
    -- (@cloudWatchLogs@, @firehose@, or @s3@) in a @destinationConfiguration@.
    destinationConfiguration :: Prelude.Maybe DestinationConfiguration,
    -- | Logging-configuration name. The value does not need to be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | Identifier of the logging configuration to be updated.
    identifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLoggingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationConfiguration', 'updateLoggingConfiguration_destinationConfiguration' - A complex type that contains a destination configuration for where chat
-- content will be logged. There can be only one type of destination
-- (@cloudWatchLogs@, @firehose@, or @s3@) in a @destinationConfiguration@.
--
-- 'name', 'updateLoggingConfiguration_name' - Logging-configuration name. The value does not need to be unique.
--
-- 'identifier', 'updateLoggingConfiguration_identifier' - Identifier of the logging configuration to be updated.
newUpdateLoggingConfiguration ::
  -- | 'identifier'
  Prelude.Text ->
  UpdateLoggingConfiguration
newUpdateLoggingConfiguration pIdentifier_ =
  UpdateLoggingConfiguration'
    { destinationConfiguration =
        Prelude.Nothing,
      name = Prelude.Nothing,
      identifier = pIdentifier_
    }

-- | A complex type that contains a destination configuration for where chat
-- content will be logged. There can be only one type of destination
-- (@cloudWatchLogs@, @firehose@, or @s3@) in a @destinationConfiguration@.
updateLoggingConfiguration_destinationConfiguration :: Lens.Lens' UpdateLoggingConfiguration (Prelude.Maybe DestinationConfiguration)
updateLoggingConfiguration_destinationConfiguration = Lens.lens (\UpdateLoggingConfiguration' {destinationConfiguration} -> destinationConfiguration) (\s@UpdateLoggingConfiguration' {} a -> s {destinationConfiguration = a} :: UpdateLoggingConfiguration)

-- | Logging-configuration name. The value does not need to be unique.
updateLoggingConfiguration_name :: Lens.Lens' UpdateLoggingConfiguration (Prelude.Maybe Prelude.Text)
updateLoggingConfiguration_name = Lens.lens (\UpdateLoggingConfiguration' {name} -> name) (\s@UpdateLoggingConfiguration' {} a -> s {name = a} :: UpdateLoggingConfiguration)

-- | Identifier of the logging configuration to be updated.
updateLoggingConfiguration_identifier :: Lens.Lens' UpdateLoggingConfiguration Prelude.Text
updateLoggingConfiguration_identifier = Lens.lens (\UpdateLoggingConfiguration' {identifier} -> identifier) (\s@UpdateLoggingConfiguration' {} a -> s {identifier = a} :: UpdateLoggingConfiguration)

instance Core.AWSRequest UpdateLoggingConfiguration where
  type
    AWSResponse UpdateLoggingConfiguration =
      UpdateLoggingConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateLoggingConfigurationResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "createTime")
            Prelude.<*> (x Data..?> "destinationConfiguration")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "state")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "updateTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateLoggingConfiguration where
  hashWithSalt _salt UpdateLoggingConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` destinationConfiguration
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` identifier

instance Prelude.NFData UpdateLoggingConfiguration where
  rnf UpdateLoggingConfiguration' {..} =
    Prelude.rnf destinationConfiguration
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf identifier

instance Data.ToHeaders UpdateLoggingConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateLoggingConfiguration where
  toJSON UpdateLoggingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("destinationConfiguration" Data..=)
              Prelude.<$> destinationConfiguration,
            ("name" Data..=) Prelude.<$> name,
            Prelude.Just ("identifier" Data..= identifier)
          ]
      )

instance Data.ToPath UpdateLoggingConfiguration where
  toPath = Prelude.const "/UpdateLoggingConfiguration"

instance Data.ToQuery UpdateLoggingConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateLoggingConfigurationResponse' smart constructor.
data UpdateLoggingConfigurationResponse = UpdateLoggingConfigurationResponse'
  { -- | Logging-configuration ARN, from the request (if @identifier@ was an
    -- ARN).
    arn :: Prelude.Maybe Prelude.Text,
    -- | Time when the logging configuration was created. This is an ISO 8601
    -- timestamp; /note that this is returned as a string/.
    createTime :: Prelude.Maybe Data.POSIX,
    -- | A complex type that contains a destination configuration for where chat
    -- content will be logged, from the request. There is only one type of
    -- destination (@cloudWatchLogs@, @firehose@, or @s3@) in a
    -- @destinationConfiguration@.
    destinationConfiguration :: Prelude.Maybe DestinationConfiguration,
    -- | Logging-configuration ID, generated by the system. This is a relative
    -- identifier, the part of the ARN that uniquely identifies the room.
    id :: Prelude.Maybe Prelude.Text,
    -- | Logging-configuration name, from the request (if specified).
    name :: Prelude.Maybe Prelude.Text,
    -- | The state of the logging configuration. When the state is @ACTIVE@, the
    -- configuration is ready to log chat content.
    state :: Prelude.Maybe UpdateLoggingConfigurationState,
    -- | Tags attached to the resource. Array of maps, each of the form
    -- @string:string (key:value)@.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Time of the logging configuration’s last update. This is an ISO 8601
    -- timestamp; /note that this is returned as a string/.
    updateTime :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLoggingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateLoggingConfigurationResponse_arn' - Logging-configuration ARN, from the request (if @identifier@ was an
-- ARN).
--
-- 'createTime', 'updateLoggingConfigurationResponse_createTime' - Time when the logging configuration was created. This is an ISO 8601
-- timestamp; /note that this is returned as a string/.
--
-- 'destinationConfiguration', 'updateLoggingConfigurationResponse_destinationConfiguration' - A complex type that contains a destination configuration for where chat
-- content will be logged, from the request. There is only one type of
-- destination (@cloudWatchLogs@, @firehose@, or @s3@) in a
-- @destinationConfiguration@.
--
-- 'id', 'updateLoggingConfigurationResponse_id' - Logging-configuration ID, generated by the system. This is a relative
-- identifier, the part of the ARN that uniquely identifies the room.
--
-- 'name', 'updateLoggingConfigurationResponse_name' - Logging-configuration name, from the request (if specified).
--
-- 'state', 'updateLoggingConfigurationResponse_state' - The state of the logging configuration. When the state is @ACTIVE@, the
-- configuration is ready to log chat content.
--
-- 'tags', 'updateLoggingConfigurationResponse_tags' - Tags attached to the resource. Array of maps, each of the form
-- @string:string (key:value)@.
--
-- 'updateTime', 'updateLoggingConfigurationResponse_updateTime' - Time of the logging configuration’s last update. This is an ISO 8601
-- timestamp; /note that this is returned as a string/.
--
-- 'httpStatus', 'updateLoggingConfigurationResponse_httpStatus' - The response's http status code.
newUpdateLoggingConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateLoggingConfigurationResponse
newUpdateLoggingConfigurationResponse pHttpStatus_ =
  UpdateLoggingConfigurationResponse'
    { arn =
        Prelude.Nothing,
      createTime = Prelude.Nothing,
      destinationConfiguration =
        Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      updateTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Logging-configuration ARN, from the request (if @identifier@ was an
-- ARN).
updateLoggingConfigurationResponse_arn :: Lens.Lens' UpdateLoggingConfigurationResponse (Prelude.Maybe Prelude.Text)
updateLoggingConfigurationResponse_arn = Lens.lens (\UpdateLoggingConfigurationResponse' {arn} -> arn) (\s@UpdateLoggingConfigurationResponse' {} a -> s {arn = a} :: UpdateLoggingConfigurationResponse)

-- | Time when the logging configuration was created. This is an ISO 8601
-- timestamp; /note that this is returned as a string/.
updateLoggingConfigurationResponse_createTime :: Lens.Lens' UpdateLoggingConfigurationResponse (Prelude.Maybe Prelude.UTCTime)
updateLoggingConfigurationResponse_createTime = Lens.lens (\UpdateLoggingConfigurationResponse' {createTime} -> createTime) (\s@UpdateLoggingConfigurationResponse' {} a -> s {createTime = a} :: UpdateLoggingConfigurationResponse) Prelude.. Lens.mapping Data._Time

-- | A complex type that contains a destination configuration for where chat
-- content will be logged, from the request. There is only one type of
-- destination (@cloudWatchLogs@, @firehose@, or @s3@) in a
-- @destinationConfiguration@.
updateLoggingConfigurationResponse_destinationConfiguration :: Lens.Lens' UpdateLoggingConfigurationResponse (Prelude.Maybe DestinationConfiguration)
updateLoggingConfigurationResponse_destinationConfiguration = Lens.lens (\UpdateLoggingConfigurationResponse' {destinationConfiguration} -> destinationConfiguration) (\s@UpdateLoggingConfigurationResponse' {} a -> s {destinationConfiguration = a} :: UpdateLoggingConfigurationResponse)

-- | Logging-configuration ID, generated by the system. This is a relative
-- identifier, the part of the ARN that uniquely identifies the room.
updateLoggingConfigurationResponse_id :: Lens.Lens' UpdateLoggingConfigurationResponse (Prelude.Maybe Prelude.Text)
updateLoggingConfigurationResponse_id = Lens.lens (\UpdateLoggingConfigurationResponse' {id} -> id) (\s@UpdateLoggingConfigurationResponse' {} a -> s {id = a} :: UpdateLoggingConfigurationResponse)

-- | Logging-configuration name, from the request (if specified).
updateLoggingConfigurationResponse_name :: Lens.Lens' UpdateLoggingConfigurationResponse (Prelude.Maybe Prelude.Text)
updateLoggingConfigurationResponse_name = Lens.lens (\UpdateLoggingConfigurationResponse' {name} -> name) (\s@UpdateLoggingConfigurationResponse' {} a -> s {name = a} :: UpdateLoggingConfigurationResponse)

-- | The state of the logging configuration. When the state is @ACTIVE@, the
-- configuration is ready to log chat content.
updateLoggingConfigurationResponse_state :: Lens.Lens' UpdateLoggingConfigurationResponse (Prelude.Maybe UpdateLoggingConfigurationState)
updateLoggingConfigurationResponse_state = Lens.lens (\UpdateLoggingConfigurationResponse' {state} -> state) (\s@UpdateLoggingConfigurationResponse' {} a -> s {state = a} :: UpdateLoggingConfigurationResponse)

-- | Tags attached to the resource. Array of maps, each of the form
-- @string:string (key:value)@.
updateLoggingConfigurationResponse_tags :: Lens.Lens' UpdateLoggingConfigurationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateLoggingConfigurationResponse_tags = Lens.lens (\UpdateLoggingConfigurationResponse' {tags} -> tags) (\s@UpdateLoggingConfigurationResponse' {} a -> s {tags = a} :: UpdateLoggingConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | Time of the logging configuration’s last update. This is an ISO 8601
-- timestamp; /note that this is returned as a string/.
updateLoggingConfigurationResponse_updateTime :: Lens.Lens' UpdateLoggingConfigurationResponse (Prelude.Maybe Prelude.UTCTime)
updateLoggingConfigurationResponse_updateTime = Lens.lens (\UpdateLoggingConfigurationResponse' {updateTime} -> updateTime) (\s@UpdateLoggingConfigurationResponse' {} a -> s {updateTime = a} :: UpdateLoggingConfigurationResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
updateLoggingConfigurationResponse_httpStatus :: Lens.Lens' UpdateLoggingConfigurationResponse Prelude.Int
updateLoggingConfigurationResponse_httpStatus = Lens.lens (\UpdateLoggingConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateLoggingConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateLoggingConfigurationResponse)

instance
  Prelude.NFData
    UpdateLoggingConfigurationResponse
  where
  rnf UpdateLoggingConfigurationResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf destinationConfiguration
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf updateTime
      `Prelude.seq` Prelude.rnf httpStatus
