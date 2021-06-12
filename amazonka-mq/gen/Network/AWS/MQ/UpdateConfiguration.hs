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
-- Module      : Network.AWS.MQ.UpdateConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified configuration.
module Network.AWS.MQ.UpdateConfiguration
  ( -- * Creating a Request
    UpdateConfiguration (..),
    newUpdateConfiguration,

    -- * Request Lenses
    updateConfiguration_data,
    updateConfiguration_description,
    updateConfiguration_configurationId,

    -- * Destructuring the Response
    UpdateConfigurationResponse (..),
    newUpdateConfigurationResponse,

    -- * Response Lenses
    updateConfigurationResponse_warnings,
    updateConfigurationResponse_latestRevision,
    updateConfigurationResponse_arn,
    updateConfigurationResponse_id,
    updateConfigurationResponse_name,
    updateConfigurationResponse_created,
    updateConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Updates the specified configuration.
--
-- /See:/ 'newUpdateConfiguration' smart constructor.
data UpdateConfiguration = UpdateConfiguration'
  { -- | Required. The base64-encoded XML configuration.
    data' :: Core.Maybe Core.Text,
    -- | The description of the configuration.
    description :: Core.Maybe Core.Text,
    -- | The unique ID that Amazon MQ generates for the configuration.
    configurationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'data'', 'updateConfiguration_data' - Required. The base64-encoded XML configuration.
--
-- 'description', 'updateConfiguration_description' - The description of the configuration.
--
-- 'configurationId', 'updateConfiguration_configurationId' - The unique ID that Amazon MQ generates for the configuration.
newUpdateConfiguration ::
  -- | 'configurationId'
  Core.Text ->
  UpdateConfiguration
newUpdateConfiguration pConfigurationId_ =
  UpdateConfiguration'
    { data' = Core.Nothing,
      description = Core.Nothing,
      configurationId = pConfigurationId_
    }

-- | Required. The base64-encoded XML configuration.
updateConfiguration_data :: Lens.Lens' UpdateConfiguration (Core.Maybe Core.Text)
updateConfiguration_data = Lens.lens (\UpdateConfiguration' {data'} -> data') (\s@UpdateConfiguration' {} a -> s {data' = a} :: UpdateConfiguration)

-- | The description of the configuration.
updateConfiguration_description :: Lens.Lens' UpdateConfiguration (Core.Maybe Core.Text)
updateConfiguration_description = Lens.lens (\UpdateConfiguration' {description} -> description) (\s@UpdateConfiguration' {} a -> s {description = a} :: UpdateConfiguration)

-- | The unique ID that Amazon MQ generates for the configuration.
updateConfiguration_configurationId :: Lens.Lens' UpdateConfiguration Core.Text
updateConfiguration_configurationId = Lens.lens (\UpdateConfiguration' {configurationId} -> configurationId) (\s@UpdateConfiguration' {} a -> s {configurationId = a} :: UpdateConfiguration)

instance Core.AWSRequest UpdateConfiguration where
  type
    AWSResponse UpdateConfiguration =
      UpdateConfigurationResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateConfigurationResponse'
            Core.<$> (x Core..?> "warnings" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "latestRevision")
            Core.<*> (x Core..?> "arn")
            Core.<*> (x Core..?> "id")
            Core.<*> (x Core..?> "name")
            Core.<*> (x Core..?> "created")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateConfiguration

instance Core.NFData UpdateConfiguration

instance Core.ToHeaders UpdateConfiguration where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateConfiguration where
  toJSON UpdateConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("data" Core..=) Core.<$> data',
            ("description" Core..=) Core.<$> description
          ]
      )

instance Core.ToPath UpdateConfiguration where
  toPath UpdateConfiguration' {..} =
    Core.mconcat
      ["/v1/configurations/", Core.toBS configurationId]

instance Core.ToQuery UpdateConfiguration where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateConfigurationResponse' smart constructor.
data UpdateConfigurationResponse = UpdateConfigurationResponse'
  { -- | The list of the first 20 warnings about the configuration XML elements
    -- or attributes that were sanitized.
    warnings :: Core.Maybe [SanitizationWarning],
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
-- Create a value of 'UpdateConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'warnings', 'updateConfigurationResponse_warnings' - The list of the first 20 warnings about the configuration XML elements
-- or attributes that were sanitized.
--
-- 'latestRevision', 'updateConfigurationResponse_latestRevision' - The latest revision of the configuration.
--
-- 'arn', 'updateConfigurationResponse_arn' - Required. The Amazon Resource Name (ARN) of the configuration.
--
-- 'id', 'updateConfigurationResponse_id' - Required. The unique ID that Amazon MQ generates for the configuration.
--
-- 'name', 'updateConfigurationResponse_name' - Required. The name of the configuration. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 1-150 characters long.
--
-- 'created', 'updateConfigurationResponse_created' - Required. The date and time of the configuration.
--
-- 'httpStatus', 'updateConfigurationResponse_httpStatus' - The response's http status code.
newUpdateConfigurationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateConfigurationResponse
newUpdateConfigurationResponse pHttpStatus_ =
  UpdateConfigurationResponse'
    { warnings =
        Core.Nothing,
      latestRevision = Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing,
      created = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of the first 20 warnings about the configuration XML elements
-- or attributes that were sanitized.
updateConfigurationResponse_warnings :: Lens.Lens' UpdateConfigurationResponse (Core.Maybe [SanitizationWarning])
updateConfigurationResponse_warnings = Lens.lens (\UpdateConfigurationResponse' {warnings} -> warnings) (\s@UpdateConfigurationResponse' {} a -> s {warnings = a} :: UpdateConfigurationResponse) Core.. Lens.mapping Lens._Coerce

-- | The latest revision of the configuration.
updateConfigurationResponse_latestRevision :: Lens.Lens' UpdateConfigurationResponse (Core.Maybe ConfigurationRevision)
updateConfigurationResponse_latestRevision = Lens.lens (\UpdateConfigurationResponse' {latestRevision} -> latestRevision) (\s@UpdateConfigurationResponse' {} a -> s {latestRevision = a} :: UpdateConfigurationResponse)

-- | Required. The Amazon Resource Name (ARN) of the configuration.
updateConfigurationResponse_arn :: Lens.Lens' UpdateConfigurationResponse (Core.Maybe Core.Text)
updateConfigurationResponse_arn = Lens.lens (\UpdateConfigurationResponse' {arn} -> arn) (\s@UpdateConfigurationResponse' {} a -> s {arn = a} :: UpdateConfigurationResponse)

-- | Required. The unique ID that Amazon MQ generates for the configuration.
updateConfigurationResponse_id :: Lens.Lens' UpdateConfigurationResponse (Core.Maybe Core.Text)
updateConfigurationResponse_id = Lens.lens (\UpdateConfigurationResponse' {id} -> id) (\s@UpdateConfigurationResponse' {} a -> s {id = a} :: UpdateConfigurationResponse)

-- | Required. The name of the configuration. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 1-150 characters long.
updateConfigurationResponse_name :: Lens.Lens' UpdateConfigurationResponse (Core.Maybe Core.Text)
updateConfigurationResponse_name = Lens.lens (\UpdateConfigurationResponse' {name} -> name) (\s@UpdateConfigurationResponse' {} a -> s {name = a} :: UpdateConfigurationResponse)

-- | Required. The date and time of the configuration.
updateConfigurationResponse_created :: Lens.Lens' UpdateConfigurationResponse (Core.Maybe Core.UTCTime)
updateConfigurationResponse_created = Lens.lens (\UpdateConfigurationResponse' {created} -> created) (\s@UpdateConfigurationResponse' {} a -> s {created = a} :: UpdateConfigurationResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
updateConfigurationResponse_httpStatus :: Lens.Lens' UpdateConfigurationResponse Core.Int
updateConfigurationResponse_httpStatus = Lens.lens (\UpdateConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateConfigurationResponse)

instance Core.NFData UpdateConfigurationResponse
