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
-- Module      : Amazonka.IVSChat.CreateLoggingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a logging configuration that allows clients to store and record
-- sent messages.
module Amazonka.IVSChat.CreateLoggingConfiguration
  ( -- * Creating a Request
    CreateLoggingConfiguration (..),
    newCreateLoggingConfiguration,

    -- * Request Lenses
    createLoggingConfiguration_name,
    createLoggingConfiguration_tags,
    createLoggingConfiguration_destinationConfiguration,

    -- * Destructuring the Response
    CreateLoggingConfigurationResponse (..),
    newCreateLoggingConfigurationResponse,

    -- * Response Lenses
    createLoggingConfigurationResponse_arn,
    createLoggingConfigurationResponse_createTime,
    createLoggingConfigurationResponse_destinationConfiguration,
    createLoggingConfigurationResponse_id,
    createLoggingConfigurationResponse_name,
    createLoggingConfigurationResponse_state,
    createLoggingConfigurationResponse_tags,
    createLoggingConfigurationResponse_updateTime,
    createLoggingConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVSChat.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLoggingConfiguration' smart constructor.
data CreateLoggingConfiguration = CreateLoggingConfiguration'
  { -- | Logging-configuration name. The value does not need to be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | Tags to attach to the resource. Array of maps, each of the form
    -- @string:string (key:value)@. See
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
    -- for details, including restrictions that apply to tags and \"Tag naming
    -- limits and requirements\"; Amazon IVS Chat has no constraints on tags
    -- beyond what is documented there.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A complex type that contains a destination configuration for where chat
    -- content will be logged. There can be only one type of destination
    -- (@cloudWatchLogs@, @firehose@, or @s3@) in a @destinationConfiguration@.
    destinationConfiguration :: DestinationConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLoggingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createLoggingConfiguration_name' - Logging-configuration name. The value does not need to be unique.
--
-- 'tags', 'createLoggingConfiguration_tags' - Tags to attach to the resource. Array of maps, each of the form
-- @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- for details, including restrictions that apply to tags and \"Tag naming
-- limits and requirements\"; Amazon IVS Chat has no constraints on tags
-- beyond what is documented there.
--
-- 'destinationConfiguration', 'createLoggingConfiguration_destinationConfiguration' - A complex type that contains a destination configuration for where chat
-- content will be logged. There can be only one type of destination
-- (@cloudWatchLogs@, @firehose@, or @s3@) in a @destinationConfiguration@.
newCreateLoggingConfiguration ::
  -- | 'destinationConfiguration'
  DestinationConfiguration ->
  CreateLoggingConfiguration
newCreateLoggingConfiguration
  pDestinationConfiguration_ =
    CreateLoggingConfiguration'
      { name = Prelude.Nothing,
        tags = Prelude.Nothing,
        destinationConfiguration =
          pDestinationConfiguration_
      }

-- | Logging-configuration name. The value does not need to be unique.
createLoggingConfiguration_name :: Lens.Lens' CreateLoggingConfiguration (Prelude.Maybe Prelude.Text)
createLoggingConfiguration_name = Lens.lens (\CreateLoggingConfiguration' {name} -> name) (\s@CreateLoggingConfiguration' {} a -> s {name = a} :: CreateLoggingConfiguration)

-- | Tags to attach to the resource. Array of maps, each of the form
-- @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- for details, including restrictions that apply to tags and \"Tag naming
-- limits and requirements\"; Amazon IVS Chat has no constraints on tags
-- beyond what is documented there.
createLoggingConfiguration_tags :: Lens.Lens' CreateLoggingConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createLoggingConfiguration_tags = Lens.lens (\CreateLoggingConfiguration' {tags} -> tags) (\s@CreateLoggingConfiguration' {} a -> s {tags = a} :: CreateLoggingConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A complex type that contains a destination configuration for where chat
-- content will be logged. There can be only one type of destination
-- (@cloudWatchLogs@, @firehose@, or @s3@) in a @destinationConfiguration@.
createLoggingConfiguration_destinationConfiguration :: Lens.Lens' CreateLoggingConfiguration DestinationConfiguration
createLoggingConfiguration_destinationConfiguration = Lens.lens (\CreateLoggingConfiguration' {destinationConfiguration} -> destinationConfiguration) (\s@CreateLoggingConfiguration' {} a -> s {destinationConfiguration = a} :: CreateLoggingConfiguration)

instance Core.AWSRequest CreateLoggingConfiguration where
  type
    AWSResponse CreateLoggingConfiguration =
      CreateLoggingConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLoggingConfigurationResponse'
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

instance Prelude.Hashable CreateLoggingConfiguration where
  hashWithSalt _salt CreateLoggingConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` destinationConfiguration

instance Prelude.NFData CreateLoggingConfiguration where
  rnf CreateLoggingConfiguration' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf destinationConfiguration

instance Data.ToHeaders CreateLoggingConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateLoggingConfiguration where
  toJSON CreateLoggingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ( "destinationConfiguration"
                  Data..= destinationConfiguration
              )
          ]
      )

instance Data.ToPath CreateLoggingConfiguration where
  toPath = Prelude.const "/CreateLoggingConfiguration"

instance Data.ToQuery CreateLoggingConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLoggingConfigurationResponse' smart constructor.
data CreateLoggingConfigurationResponse = CreateLoggingConfigurationResponse'
  { -- | Logging-configuration ARN, assigned by the system.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Time when the logging configuration was created. This is an ISO 8601
    -- timestamp; /note that this is returned as a string/.
    createTime :: Prelude.Maybe Data.ISO8601,
    -- | A complex type that contains a destination configuration for where chat
    -- content will be logged, from the request. There is only one type of
    -- destination (@cloudWatchLogs@, @firehose@, or @s3@) in a
    -- @destinationConfiguration@.
    destinationConfiguration :: Prelude.Maybe DestinationConfiguration,
    -- | Logging-configuration ID, generated by the system. This is a relative
    -- identifier, the part of the ARN that uniquely identifies the logging
    -- configuration.
    id :: Prelude.Maybe Prelude.Text,
    -- | Logging-configuration name, from the request (if specified).
    name :: Prelude.Maybe Prelude.Text,
    -- | The state of the logging configuration. When the state is @ACTIVE@, the
    -- configuration is ready to log chat content.
    state :: Prelude.Maybe CreateLoggingConfigurationState,
    -- | Tags attached to the resource, from the request (if specified). Array of
    -- maps, each of the form @string:string (key:value)@.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Time of the logging configuration’s last update. This is an ISO 8601
    -- timestamp; /note that this is returned as a string/.
    updateTime :: Prelude.Maybe Data.ISO8601,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLoggingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createLoggingConfigurationResponse_arn' - Logging-configuration ARN, assigned by the system.
--
-- 'createTime', 'createLoggingConfigurationResponse_createTime' - Time when the logging configuration was created. This is an ISO 8601
-- timestamp; /note that this is returned as a string/.
--
-- 'destinationConfiguration', 'createLoggingConfigurationResponse_destinationConfiguration' - A complex type that contains a destination configuration for where chat
-- content will be logged, from the request. There is only one type of
-- destination (@cloudWatchLogs@, @firehose@, or @s3@) in a
-- @destinationConfiguration@.
--
-- 'id', 'createLoggingConfigurationResponse_id' - Logging-configuration ID, generated by the system. This is a relative
-- identifier, the part of the ARN that uniquely identifies the logging
-- configuration.
--
-- 'name', 'createLoggingConfigurationResponse_name' - Logging-configuration name, from the request (if specified).
--
-- 'state', 'createLoggingConfigurationResponse_state' - The state of the logging configuration. When the state is @ACTIVE@, the
-- configuration is ready to log chat content.
--
-- 'tags', 'createLoggingConfigurationResponse_tags' - Tags attached to the resource, from the request (if specified). Array of
-- maps, each of the form @string:string (key:value)@.
--
-- 'updateTime', 'createLoggingConfigurationResponse_updateTime' - Time of the logging configuration’s last update. This is an ISO 8601
-- timestamp; /note that this is returned as a string/.
--
-- 'httpStatus', 'createLoggingConfigurationResponse_httpStatus' - The response's http status code.
newCreateLoggingConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLoggingConfigurationResponse
newCreateLoggingConfigurationResponse pHttpStatus_ =
  CreateLoggingConfigurationResponse'
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

-- | Logging-configuration ARN, assigned by the system.
createLoggingConfigurationResponse_arn :: Lens.Lens' CreateLoggingConfigurationResponse (Prelude.Maybe Prelude.Text)
createLoggingConfigurationResponse_arn = Lens.lens (\CreateLoggingConfigurationResponse' {arn} -> arn) (\s@CreateLoggingConfigurationResponse' {} a -> s {arn = a} :: CreateLoggingConfigurationResponse)

-- | Time when the logging configuration was created. This is an ISO 8601
-- timestamp; /note that this is returned as a string/.
createLoggingConfigurationResponse_createTime :: Lens.Lens' CreateLoggingConfigurationResponse (Prelude.Maybe Prelude.UTCTime)
createLoggingConfigurationResponse_createTime = Lens.lens (\CreateLoggingConfigurationResponse' {createTime} -> createTime) (\s@CreateLoggingConfigurationResponse' {} a -> s {createTime = a} :: CreateLoggingConfigurationResponse) Prelude.. Lens.mapping Data._Time

-- | A complex type that contains a destination configuration for where chat
-- content will be logged, from the request. There is only one type of
-- destination (@cloudWatchLogs@, @firehose@, or @s3@) in a
-- @destinationConfiguration@.
createLoggingConfigurationResponse_destinationConfiguration :: Lens.Lens' CreateLoggingConfigurationResponse (Prelude.Maybe DestinationConfiguration)
createLoggingConfigurationResponse_destinationConfiguration = Lens.lens (\CreateLoggingConfigurationResponse' {destinationConfiguration} -> destinationConfiguration) (\s@CreateLoggingConfigurationResponse' {} a -> s {destinationConfiguration = a} :: CreateLoggingConfigurationResponse)

-- | Logging-configuration ID, generated by the system. This is a relative
-- identifier, the part of the ARN that uniquely identifies the logging
-- configuration.
createLoggingConfigurationResponse_id :: Lens.Lens' CreateLoggingConfigurationResponse (Prelude.Maybe Prelude.Text)
createLoggingConfigurationResponse_id = Lens.lens (\CreateLoggingConfigurationResponse' {id} -> id) (\s@CreateLoggingConfigurationResponse' {} a -> s {id = a} :: CreateLoggingConfigurationResponse)

-- | Logging-configuration name, from the request (if specified).
createLoggingConfigurationResponse_name :: Lens.Lens' CreateLoggingConfigurationResponse (Prelude.Maybe Prelude.Text)
createLoggingConfigurationResponse_name = Lens.lens (\CreateLoggingConfigurationResponse' {name} -> name) (\s@CreateLoggingConfigurationResponse' {} a -> s {name = a} :: CreateLoggingConfigurationResponse)

-- | The state of the logging configuration. When the state is @ACTIVE@, the
-- configuration is ready to log chat content.
createLoggingConfigurationResponse_state :: Lens.Lens' CreateLoggingConfigurationResponse (Prelude.Maybe CreateLoggingConfigurationState)
createLoggingConfigurationResponse_state = Lens.lens (\CreateLoggingConfigurationResponse' {state} -> state) (\s@CreateLoggingConfigurationResponse' {} a -> s {state = a} :: CreateLoggingConfigurationResponse)

-- | Tags attached to the resource, from the request (if specified). Array of
-- maps, each of the form @string:string (key:value)@.
createLoggingConfigurationResponse_tags :: Lens.Lens' CreateLoggingConfigurationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createLoggingConfigurationResponse_tags = Lens.lens (\CreateLoggingConfigurationResponse' {tags} -> tags) (\s@CreateLoggingConfigurationResponse' {} a -> s {tags = a} :: CreateLoggingConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | Time of the logging configuration’s last update. This is an ISO 8601
-- timestamp; /note that this is returned as a string/.
createLoggingConfigurationResponse_updateTime :: Lens.Lens' CreateLoggingConfigurationResponse (Prelude.Maybe Prelude.UTCTime)
createLoggingConfigurationResponse_updateTime = Lens.lens (\CreateLoggingConfigurationResponse' {updateTime} -> updateTime) (\s@CreateLoggingConfigurationResponse' {} a -> s {updateTime = a} :: CreateLoggingConfigurationResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
createLoggingConfigurationResponse_httpStatus :: Lens.Lens' CreateLoggingConfigurationResponse Prelude.Int
createLoggingConfigurationResponse_httpStatus = Lens.lens (\CreateLoggingConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateLoggingConfigurationResponse' {} a -> s {httpStatus = a} :: CreateLoggingConfigurationResponse)

instance
  Prelude.NFData
    CreateLoggingConfigurationResponse
  where
  rnf CreateLoggingConfigurationResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf destinationConfiguration
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf updateTime
      `Prelude.seq` Prelude.rnf httpStatus
