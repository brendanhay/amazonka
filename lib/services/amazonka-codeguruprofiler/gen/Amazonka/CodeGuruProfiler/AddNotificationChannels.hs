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
-- Module      : Amazonka.CodeGuruProfiler.AddNotificationChannels
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Add up to 2 anomaly notifications channels for a profiling group.
module Amazonka.CodeGuruProfiler.AddNotificationChannels
  ( -- * Creating a Request
    AddNotificationChannels (..),
    newAddNotificationChannels,

    -- * Request Lenses
    addNotificationChannels_channels,
    addNotificationChannels_profilingGroupName,

    -- * Destructuring the Response
    AddNotificationChannelsResponse (..),
    newAddNotificationChannelsResponse,

    -- * Response Lenses
    addNotificationChannelsResponse_notificationConfiguration,
    addNotificationChannelsResponse_httpStatus,
  )
where

import Amazonka.CodeGuruProfiler.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The structure representing the AddNotificationChannelsRequest.
--
-- /See:/ 'newAddNotificationChannels' smart constructor.
data AddNotificationChannels = AddNotificationChannels'
  { -- | One or 2 channels to report to when anomalies are detected.
    channels :: Prelude.NonEmpty Channel,
    -- | The name of the profiling group that we are setting up notifications
    -- for.
    profilingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddNotificationChannels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channels', 'addNotificationChannels_channels' - One or 2 channels to report to when anomalies are detected.
--
-- 'profilingGroupName', 'addNotificationChannels_profilingGroupName' - The name of the profiling group that we are setting up notifications
-- for.
newAddNotificationChannels ::
  -- | 'channels'
  Prelude.NonEmpty Channel ->
  -- | 'profilingGroupName'
  Prelude.Text ->
  AddNotificationChannels
newAddNotificationChannels
  pChannels_
  pProfilingGroupName_ =
    AddNotificationChannels'
      { channels =
          Lens.coerced Lens.# pChannels_,
        profilingGroupName = pProfilingGroupName_
      }

-- | One or 2 channels to report to when anomalies are detected.
addNotificationChannels_channels :: Lens.Lens' AddNotificationChannels (Prelude.NonEmpty Channel)
addNotificationChannels_channels = Lens.lens (\AddNotificationChannels' {channels} -> channels) (\s@AddNotificationChannels' {} a -> s {channels = a} :: AddNotificationChannels) Prelude.. Lens.coerced

-- | The name of the profiling group that we are setting up notifications
-- for.
addNotificationChannels_profilingGroupName :: Lens.Lens' AddNotificationChannels Prelude.Text
addNotificationChannels_profilingGroupName = Lens.lens (\AddNotificationChannels' {profilingGroupName} -> profilingGroupName) (\s@AddNotificationChannels' {} a -> s {profilingGroupName = a} :: AddNotificationChannels)

instance Core.AWSRequest AddNotificationChannels where
  type
    AWSResponse AddNotificationChannels =
      AddNotificationChannelsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AddNotificationChannelsResponse'
            Prelude.<$> (x Data..?> "notificationConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddNotificationChannels where
  hashWithSalt _salt AddNotificationChannels' {..} =
    _salt
      `Prelude.hashWithSalt` channels
      `Prelude.hashWithSalt` profilingGroupName

instance Prelude.NFData AddNotificationChannels where
  rnf AddNotificationChannels' {..} =
    Prelude.rnf channels
      `Prelude.seq` Prelude.rnf profilingGroupName

instance Data.ToHeaders AddNotificationChannels where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AddNotificationChannels where
  toJSON AddNotificationChannels' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("channels" Data..= channels)]
      )

instance Data.ToPath AddNotificationChannels where
  toPath AddNotificationChannels' {..} =
    Prelude.mconcat
      [ "/profilingGroups/",
        Data.toBS profilingGroupName,
        "/notificationConfiguration"
      ]

instance Data.ToQuery AddNotificationChannels where
  toQuery = Prelude.const Prelude.mempty

-- | The structure representing the AddNotificationChannelsResponse.
--
-- /See:/ 'newAddNotificationChannelsResponse' smart constructor.
data AddNotificationChannelsResponse = AddNotificationChannelsResponse'
  { -- | The new notification configuration for this profiling group.
    notificationConfiguration :: Prelude.Maybe NotificationConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddNotificationChannelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationConfiguration', 'addNotificationChannelsResponse_notificationConfiguration' - The new notification configuration for this profiling group.
--
-- 'httpStatus', 'addNotificationChannelsResponse_httpStatus' - The response's http status code.
newAddNotificationChannelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddNotificationChannelsResponse
newAddNotificationChannelsResponse pHttpStatus_ =
  AddNotificationChannelsResponse'
    { notificationConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The new notification configuration for this profiling group.
addNotificationChannelsResponse_notificationConfiguration :: Lens.Lens' AddNotificationChannelsResponse (Prelude.Maybe NotificationConfiguration)
addNotificationChannelsResponse_notificationConfiguration = Lens.lens (\AddNotificationChannelsResponse' {notificationConfiguration} -> notificationConfiguration) (\s@AddNotificationChannelsResponse' {} a -> s {notificationConfiguration = a} :: AddNotificationChannelsResponse)

-- | The response's http status code.
addNotificationChannelsResponse_httpStatus :: Lens.Lens' AddNotificationChannelsResponse Prelude.Int
addNotificationChannelsResponse_httpStatus = Lens.lens (\AddNotificationChannelsResponse' {httpStatus} -> httpStatus) (\s@AddNotificationChannelsResponse' {} a -> s {httpStatus = a} :: AddNotificationChannelsResponse)

instance
  Prelude.NFData
    AddNotificationChannelsResponse
  where
  rnf AddNotificationChannelsResponse' {..} =
    Prelude.rnf notificationConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
