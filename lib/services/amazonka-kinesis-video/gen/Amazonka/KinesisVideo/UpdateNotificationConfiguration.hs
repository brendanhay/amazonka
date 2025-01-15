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
-- Module      : Amazonka.KinesisVideo.UpdateNotificationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the notification information for a stream.
module Amazonka.KinesisVideo.UpdateNotificationConfiguration
  ( -- * Creating a Request
    UpdateNotificationConfiguration (..),
    newUpdateNotificationConfiguration,

    -- * Request Lenses
    updateNotificationConfiguration_notificationConfiguration,
    updateNotificationConfiguration_streamARN,
    updateNotificationConfiguration_streamName,

    -- * Destructuring the Response
    UpdateNotificationConfigurationResponse (..),
    newUpdateNotificationConfigurationResponse,

    -- * Response Lenses
    updateNotificationConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideo.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateNotificationConfiguration' smart constructor.
data UpdateNotificationConfiguration = UpdateNotificationConfiguration'
  { -- | The structure containing the information required for notifications. If
    -- the structure is null, the configuration will be deleted from the
    -- stream.
    notificationConfiguration :: Prelude.Maybe NotificationConfiguration,
    -- | The Amazon Resource Name (ARN) of the Kinesis video stream from where
    -- you want to update the notification configuration. You must specify
    -- either the @StreamName@ or the @StreamARN@.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the stream from which to update the notification
    -- configuration. You must specify either the @StreamName@ or the
    -- @StreamARN@.
    streamName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNotificationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationConfiguration', 'updateNotificationConfiguration_notificationConfiguration' - The structure containing the information required for notifications. If
-- the structure is null, the configuration will be deleted from the
-- stream.
--
-- 'streamARN', 'updateNotificationConfiguration_streamARN' - The Amazon Resource Name (ARN) of the Kinesis video stream from where
-- you want to update the notification configuration. You must specify
-- either the @StreamName@ or the @StreamARN@.
--
-- 'streamName', 'updateNotificationConfiguration_streamName' - The name of the stream from which to update the notification
-- configuration. You must specify either the @StreamName@ or the
-- @StreamARN@.
newUpdateNotificationConfiguration ::
  UpdateNotificationConfiguration
newUpdateNotificationConfiguration =
  UpdateNotificationConfiguration'
    { notificationConfiguration =
        Prelude.Nothing,
      streamARN = Prelude.Nothing,
      streamName = Prelude.Nothing
    }

-- | The structure containing the information required for notifications. If
-- the structure is null, the configuration will be deleted from the
-- stream.
updateNotificationConfiguration_notificationConfiguration :: Lens.Lens' UpdateNotificationConfiguration (Prelude.Maybe NotificationConfiguration)
updateNotificationConfiguration_notificationConfiguration = Lens.lens (\UpdateNotificationConfiguration' {notificationConfiguration} -> notificationConfiguration) (\s@UpdateNotificationConfiguration' {} a -> s {notificationConfiguration = a} :: UpdateNotificationConfiguration)

-- | The Amazon Resource Name (ARN) of the Kinesis video stream from where
-- you want to update the notification configuration. You must specify
-- either the @StreamName@ or the @StreamARN@.
updateNotificationConfiguration_streamARN :: Lens.Lens' UpdateNotificationConfiguration (Prelude.Maybe Prelude.Text)
updateNotificationConfiguration_streamARN = Lens.lens (\UpdateNotificationConfiguration' {streamARN} -> streamARN) (\s@UpdateNotificationConfiguration' {} a -> s {streamARN = a} :: UpdateNotificationConfiguration)

-- | The name of the stream from which to update the notification
-- configuration. You must specify either the @StreamName@ or the
-- @StreamARN@.
updateNotificationConfiguration_streamName :: Lens.Lens' UpdateNotificationConfiguration (Prelude.Maybe Prelude.Text)
updateNotificationConfiguration_streamName = Lens.lens (\UpdateNotificationConfiguration' {streamName} -> streamName) (\s@UpdateNotificationConfiguration' {} a -> s {streamName = a} :: UpdateNotificationConfiguration)

instance
  Core.AWSRequest
    UpdateNotificationConfiguration
  where
  type
    AWSResponse UpdateNotificationConfiguration =
      UpdateNotificationConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateNotificationConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateNotificationConfiguration
  where
  hashWithSalt
    _salt
    UpdateNotificationConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` notificationConfiguration
        `Prelude.hashWithSalt` streamARN
        `Prelude.hashWithSalt` streamName

instance
  Prelude.NFData
    UpdateNotificationConfiguration
  where
  rnf UpdateNotificationConfiguration' {..} =
    Prelude.rnf notificationConfiguration `Prelude.seq`
      Prelude.rnf streamARN `Prelude.seq`
        Prelude.rnf streamName

instance
  Data.ToHeaders
    UpdateNotificationConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateNotificationConfiguration where
  toJSON UpdateNotificationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NotificationConfiguration" Data..=)
              Prelude.<$> notificationConfiguration,
            ("StreamARN" Data..=) Prelude.<$> streamARN,
            ("StreamName" Data..=) Prelude.<$> streamName
          ]
      )

instance Data.ToPath UpdateNotificationConfiguration where
  toPath =
    Prelude.const "/updateNotificationConfiguration"

instance Data.ToQuery UpdateNotificationConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateNotificationConfigurationResponse' smart constructor.
data UpdateNotificationConfigurationResponse = UpdateNotificationConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNotificationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateNotificationConfigurationResponse_httpStatus' - The response's http status code.
newUpdateNotificationConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateNotificationConfigurationResponse
newUpdateNotificationConfigurationResponse
  pHttpStatus_ =
    UpdateNotificationConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateNotificationConfigurationResponse_httpStatus :: Lens.Lens' UpdateNotificationConfigurationResponse Prelude.Int
updateNotificationConfigurationResponse_httpStatus = Lens.lens (\UpdateNotificationConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateNotificationConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateNotificationConfigurationResponse)

instance
  Prelude.NFData
    UpdateNotificationConfigurationResponse
  where
  rnf UpdateNotificationConfigurationResponse' {..} =
    Prelude.rnf httpStatus
