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
-- Module      : Amazonka.IVSChat.GetLoggingConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the specified logging configuration.
module Amazonka.IVSChat.GetLoggingConfiguration
  ( -- * Creating a Request
    GetLoggingConfiguration (..),
    newGetLoggingConfiguration,

    -- * Request Lenses
    getLoggingConfiguration_identifier,

    -- * Destructuring the Response
    GetLoggingConfigurationResponse (..),
    newGetLoggingConfigurationResponse,

    -- * Response Lenses
    getLoggingConfigurationResponse_tags,
    getLoggingConfigurationResponse_name,
    getLoggingConfigurationResponse_arn,
    getLoggingConfigurationResponse_state,
    getLoggingConfigurationResponse_id,
    getLoggingConfigurationResponse_updateTime,
    getLoggingConfigurationResponse_createTime,
    getLoggingConfigurationResponse_destinationConfiguration,
    getLoggingConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IVSChat.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLoggingConfiguration' smart constructor.
data GetLoggingConfiguration = GetLoggingConfiguration'
  { -- | Identifier of the logging configuration to be retrieved.
    identifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLoggingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'getLoggingConfiguration_identifier' - Identifier of the logging configuration to be retrieved.
newGetLoggingConfiguration ::
  -- | 'identifier'
  Prelude.Text ->
  GetLoggingConfiguration
newGetLoggingConfiguration pIdentifier_ =
  GetLoggingConfiguration' {identifier = pIdentifier_}

-- | Identifier of the logging configuration to be retrieved.
getLoggingConfiguration_identifier :: Lens.Lens' GetLoggingConfiguration Prelude.Text
getLoggingConfiguration_identifier = Lens.lens (\GetLoggingConfiguration' {identifier} -> identifier) (\s@GetLoggingConfiguration' {} a -> s {identifier = a} :: GetLoggingConfiguration)

instance Core.AWSRequest GetLoggingConfiguration where
  type
    AWSResponse GetLoggingConfiguration =
      GetLoggingConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLoggingConfigurationResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "state")
            Prelude.<*> (x Core..?> "id")
            Prelude.<*> (x Core..?> "updateTime")
            Prelude.<*> (x Core..?> "createTime")
            Prelude.<*> (x Core..?> "destinationConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLoggingConfiguration where
  hashWithSalt _salt GetLoggingConfiguration' {..} =
    _salt `Prelude.hashWithSalt` identifier

instance Prelude.NFData GetLoggingConfiguration where
  rnf GetLoggingConfiguration' {..} =
    Prelude.rnf identifier

instance Core.ToHeaders GetLoggingConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetLoggingConfiguration where
  toJSON GetLoggingConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("identifier" Core..= identifier)]
      )

instance Core.ToPath GetLoggingConfiguration where
  toPath = Prelude.const "/GetLoggingConfiguration"

instance Core.ToQuery GetLoggingConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLoggingConfigurationResponse' smart constructor.
data GetLoggingConfigurationResponse = GetLoggingConfigurationResponse'
  { -- | Tags attached to the resource. Array of maps, each of the form
    -- @string:string (key:value)@.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Logging-configuration name. This value does not need to be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | Logging-configuration ARN, from the request (if @identifier@ was an
    -- ARN).
    arn :: Prelude.Maybe Prelude.Text,
    -- | The state of the logging configuration. When the state is @ACTIVE@, the
    -- configuration is ready to log chat content.
    state :: Prelude.Maybe LoggingConfigurationState,
    -- | Logging-configuration ID, generated by the system. This is a relative
    -- identifier, the part of the ARN that uniquely identifies the logging
    -- configuration.
    id :: Prelude.Maybe Prelude.Text,
    -- | Time of the logging configuration’s last update. This is an ISO 8601
    -- timestamp; /note that this is returned as a string/.
    updateTime :: Prelude.Maybe Core.POSIX,
    -- | Time when the logging configuration was created. This is an ISO 8601
    -- timestamp; /note that this is returned as a string/.
    createTime :: Prelude.Maybe Core.POSIX,
    -- | A complex type that contains a destination configuration for where chat
    -- content will be logged. There is only one type of destination
    -- (@cloudWatchLogs@, @firehose@, or @s3@) in a @destinationConfiguration@.
    destinationConfiguration :: Prelude.Maybe DestinationConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLoggingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'getLoggingConfigurationResponse_tags' - Tags attached to the resource. Array of maps, each of the form
-- @string:string (key:value)@.
--
-- 'name', 'getLoggingConfigurationResponse_name' - Logging-configuration name. This value does not need to be unique.
--
-- 'arn', 'getLoggingConfigurationResponse_arn' - Logging-configuration ARN, from the request (if @identifier@ was an
-- ARN).
--
-- 'state', 'getLoggingConfigurationResponse_state' - The state of the logging configuration. When the state is @ACTIVE@, the
-- configuration is ready to log chat content.
--
-- 'id', 'getLoggingConfigurationResponse_id' - Logging-configuration ID, generated by the system. This is a relative
-- identifier, the part of the ARN that uniquely identifies the logging
-- configuration.
--
-- 'updateTime', 'getLoggingConfigurationResponse_updateTime' - Time of the logging configuration’s last update. This is an ISO 8601
-- timestamp; /note that this is returned as a string/.
--
-- 'createTime', 'getLoggingConfigurationResponse_createTime' - Time when the logging configuration was created. This is an ISO 8601
-- timestamp; /note that this is returned as a string/.
--
-- 'destinationConfiguration', 'getLoggingConfigurationResponse_destinationConfiguration' - A complex type that contains a destination configuration for where chat
-- content will be logged. There is only one type of destination
-- (@cloudWatchLogs@, @firehose@, or @s3@) in a @destinationConfiguration@.
--
-- 'httpStatus', 'getLoggingConfigurationResponse_httpStatus' - The response's http status code.
newGetLoggingConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLoggingConfigurationResponse
newGetLoggingConfigurationResponse pHttpStatus_ =
  GetLoggingConfigurationResponse'
    { tags =
        Prelude.Nothing,
      name = Prelude.Nothing,
      arn = Prelude.Nothing,
      state = Prelude.Nothing,
      id = Prelude.Nothing,
      updateTime = Prelude.Nothing,
      createTime = Prelude.Nothing,
      destinationConfiguration = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Tags attached to the resource. Array of maps, each of the form
-- @string:string (key:value)@.
getLoggingConfigurationResponse_tags :: Lens.Lens' GetLoggingConfigurationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getLoggingConfigurationResponse_tags = Lens.lens (\GetLoggingConfigurationResponse' {tags} -> tags) (\s@GetLoggingConfigurationResponse' {} a -> s {tags = a} :: GetLoggingConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | Logging-configuration name. This value does not need to be unique.
getLoggingConfigurationResponse_name :: Lens.Lens' GetLoggingConfigurationResponse (Prelude.Maybe Prelude.Text)
getLoggingConfigurationResponse_name = Lens.lens (\GetLoggingConfigurationResponse' {name} -> name) (\s@GetLoggingConfigurationResponse' {} a -> s {name = a} :: GetLoggingConfigurationResponse)

-- | Logging-configuration ARN, from the request (if @identifier@ was an
-- ARN).
getLoggingConfigurationResponse_arn :: Lens.Lens' GetLoggingConfigurationResponse (Prelude.Maybe Prelude.Text)
getLoggingConfigurationResponse_arn = Lens.lens (\GetLoggingConfigurationResponse' {arn} -> arn) (\s@GetLoggingConfigurationResponse' {} a -> s {arn = a} :: GetLoggingConfigurationResponse)

-- | The state of the logging configuration. When the state is @ACTIVE@, the
-- configuration is ready to log chat content.
getLoggingConfigurationResponse_state :: Lens.Lens' GetLoggingConfigurationResponse (Prelude.Maybe LoggingConfigurationState)
getLoggingConfigurationResponse_state = Lens.lens (\GetLoggingConfigurationResponse' {state} -> state) (\s@GetLoggingConfigurationResponse' {} a -> s {state = a} :: GetLoggingConfigurationResponse)

-- | Logging-configuration ID, generated by the system. This is a relative
-- identifier, the part of the ARN that uniquely identifies the logging
-- configuration.
getLoggingConfigurationResponse_id :: Lens.Lens' GetLoggingConfigurationResponse (Prelude.Maybe Prelude.Text)
getLoggingConfigurationResponse_id = Lens.lens (\GetLoggingConfigurationResponse' {id} -> id) (\s@GetLoggingConfigurationResponse' {} a -> s {id = a} :: GetLoggingConfigurationResponse)

-- | Time of the logging configuration’s last update. This is an ISO 8601
-- timestamp; /note that this is returned as a string/.
getLoggingConfigurationResponse_updateTime :: Lens.Lens' GetLoggingConfigurationResponse (Prelude.Maybe Prelude.UTCTime)
getLoggingConfigurationResponse_updateTime = Lens.lens (\GetLoggingConfigurationResponse' {updateTime} -> updateTime) (\s@GetLoggingConfigurationResponse' {} a -> s {updateTime = a} :: GetLoggingConfigurationResponse) Prelude.. Lens.mapping Core._Time

-- | Time when the logging configuration was created. This is an ISO 8601
-- timestamp; /note that this is returned as a string/.
getLoggingConfigurationResponse_createTime :: Lens.Lens' GetLoggingConfigurationResponse (Prelude.Maybe Prelude.UTCTime)
getLoggingConfigurationResponse_createTime = Lens.lens (\GetLoggingConfigurationResponse' {createTime} -> createTime) (\s@GetLoggingConfigurationResponse' {} a -> s {createTime = a} :: GetLoggingConfigurationResponse) Prelude.. Lens.mapping Core._Time

-- | A complex type that contains a destination configuration for where chat
-- content will be logged. There is only one type of destination
-- (@cloudWatchLogs@, @firehose@, or @s3@) in a @destinationConfiguration@.
getLoggingConfigurationResponse_destinationConfiguration :: Lens.Lens' GetLoggingConfigurationResponse (Prelude.Maybe DestinationConfiguration)
getLoggingConfigurationResponse_destinationConfiguration = Lens.lens (\GetLoggingConfigurationResponse' {destinationConfiguration} -> destinationConfiguration) (\s@GetLoggingConfigurationResponse' {} a -> s {destinationConfiguration = a} :: GetLoggingConfigurationResponse)

-- | The response's http status code.
getLoggingConfigurationResponse_httpStatus :: Lens.Lens' GetLoggingConfigurationResponse Prelude.Int
getLoggingConfigurationResponse_httpStatus = Lens.lens (\GetLoggingConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetLoggingConfigurationResponse' {} a -> s {httpStatus = a} :: GetLoggingConfigurationResponse)

instance
  Prelude.NFData
    GetLoggingConfigurationResponse
  where
  rnf GetLoggingConfigurationResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf updateTime
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf destinationConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
