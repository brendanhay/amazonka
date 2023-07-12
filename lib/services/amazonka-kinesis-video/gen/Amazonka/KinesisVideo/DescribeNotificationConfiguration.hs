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
-- Module      : Amazonka.KinesisVideo.DescribeNotificationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the @NotificationConfiguration@ for a given Kinesis video stream.
module Amazonka.KinesisVideo.DescribeNotificationConfiguration
  ( -- * Creating a Request
    DescribeNotificationConfiguration (..),
    newDescribeNotificationConfiguration,

    -- * Request Lenses
    describeNotificationConfiguration_streamARN,
    describeNotificationConfiguration_streamName,

    -- * Destructuring the Response
    DescribeNotificationConfigurationResponse (..),
    newDescribeNotificationConfigurationResponse,

    -- * Response Lenses
    describeNotificationConfigurationResponse_notificationConfiguration,
    describeNotificationConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideo.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeNotificationConfiguration' smart constructor.
data DescribeNotificationConfiguration = DescribeNotificationConfiguration'
  { -- | The Amazon Resource Name (ARN) of the Kinesis video stream from where
    -- you want to retrieve the notification configuration. You must specify
    -- either the @StreamName@ or the StreamARN.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the stream from which to retrieve the notification
    -- configuration. You must specify either the @StreamName@ or the
    -- @StreamARN@.
    streamName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNotificationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamARN', 'describeNotificationConfiguration_streamARN' - The Amazon Resource Name (ARN) of the Kinesis video stream from where
-- you want to retrieve the notification configuration. You must specify
-- either the @StreamName@ or the StreamARN.
--
-- 'streamName', 'describeNotificationConfiguration_streamName' - The name of the stream from which to retrieve the notification
-- configuration. You must specify either the @StreamName@ or the
-- @StreamARN@.
newDescribeNotificationConfiguration ::
  DescribeNotificationConfiguration
newDescribeNotificationConfiguration =
  DescribeNotificationConfiguration'
    { streamARN =
        Prelude.Nothing,
      streamName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the Kinesis video stream from where
-- you want to retrieve the notification configuration. You must specify
-- either the @StreamName@ or the StreamARN.
describeNotificationConfiguration_streamARN :: Lens.Lens' DescribeNotificationConfiguration (Prelude.Maybe Prelude.Text)
describeNotificationConfiguration_streamARN = Lens.lens (\DescribeNotificationConfiguration' {streamARN} -> streamARN) (\s@DescribeNotificationConfiguration' {} a -> s {streamARN = a} :: DescribeNotificationConfiguration)

-- | The name of the stream from which to retrieve the notification
-- configuration. You must specify either the @StreamName@ or the
-- @StreamARN@.
describeNotificationConfiguration_streamName :: Lens.Lens' DescribeNotificationConfiguration (Prelude.Maybe Prelude.Text)
describeNotificationConfiguration_streamName = Lens.lens (\DescribeNotificationConfiguration' {streamName} -> streamName) (\s@DescribeNotificationConfiguration' {} a -> s {streamName = a} :: DescribeNotificationConfiguration)

instance
  Core.AWSRequest
    DescribeNotificationConfiguration
  where
  type
    AWSResponse DescribeNotificationConfiguration =
      DescribeNotificationConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeNotificationConfigurationResponse'
            Prelude.<$> (x Data..?> "NotificationConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeNotificationConfiguration
  where
  hashWithSalt
    _salt
    DescribeNotificationConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` streamARN
        `Prelude.hashWithSalt` streamName

instance
  Prelude.NFData
    DescribeNotificationConfiguration
  where
  rnf DescribeNotificationConfiguration' {..} =
    Prelude.rnf streamARN
      `Prelude.seq` Prelude.rnf streamName

instance
  Data.ToHeaders
    DescribeNotificationConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    DescribeNotificationConfiguration
  where
  toJSON DescribeNotificationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StreamARN" Data..=) Prelude.<$> streamARN,
            ("StreamName" Data..=) Prelude.<$> streamName
          ]
      )

instance
  Data.ToPath
    DescribeNotificationConfiguration
  where
  toPath =
    Prelude.const "/describeNotificationConfiguration"

instance
  Data.ToQuery
    DescribeNotificationConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeNotificationConfigurationResponse' smart constructor.
data DescribeNotificationConfigurationResponse = DescribeNotificationConfigurationResponse'
  { -- | The structure that contains the information required for notifications.
    -- If the structure is null, the configuration will be deleted from the
    -- stream.
    notificationConfiguration :: Prelude.Maybe NotificationConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNotificationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationConfiguration', 'describeNotificationConfigurationResponse_notificationConfiguration' - The structure that contains the information required for notifications.
-- If the structure is null, the configuration will be deleted from the
-- stream.
--
-- 'httpStatus', 'describeNotificationConfigurationResponse_httpStatus' - The response's http status code.
newDescribeNotificationConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeNotificationConfigurationResponse
newDescribeNotificationConfigurationResponse
  pHttpStatus_ =
    DescribeNotificationConfigurationResponse'
      { notificationConfiguration =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The structure that contains the information required for notifications.
-- If the structure is null, the configuration will be deleted from the
-- stream.
describeNotificationConfigurationResponse_notificationConfiguration :: Lens.Lens' DescribeNotificationConfigurationResponse (Prelude.Maybe NotificationConfiguration)
describeNotificationConfigurationResponse_notificationConfiguration = Lens.lens (\DescribeNotificationConfigurationResponse' {notificationConfiguration} -> notificationConfiguration) (\s@DescribeNotificationConfigurationResponse' {} a -> s {notificationConfiguration = a} :: DescribeNotificationConfigurationResponse)

-- | The response's http status code.
describeNotificationConfigurationResponse_httpStatus :: Lens.Lens' DescribeNotificationConfigurationResponse Prelude.Int
describeNotificationConfigurationResponse_httpStatus = Lens.lens (\DescribeNotificationConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeNotificationConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeNotificationConfigurationResponse)

instance
  Prelude.NFData
    DescribeNotificationConfigurationResponse
  where
  rnf DescribeNotificationConfigurationResponse' {..} =
    Prelude.rnf notificationConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
