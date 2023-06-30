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
-- Module      : Amazonka.CodeGuruProfiler.RemoveNotificationChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove one anomaly notifications channel for a profiling group.
module Amazonka.CodeGuruProfiler.RemoveNotificationChannel
  ( -- * Creating a Request
    RemoveNotificationChannel (..),
    newRemoveNotificationChannel,

    -- * Request Lenses
    removeNotificationChannel_channelId,
    removeNotificationChannel_profilingGroupName,

    -- * Destructuring the Response
    RemoveNotificationChannelResponse (..),
    newRemoveNotificationChannelResponse,

    -- * Response Lenses
    removeNotificationChannelResponse_notificationConfiguration,
    removeNotificationChannelResponse_httpStatus,
  )
where

import Amazonka.CodeGuruProfiler.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The structure representing the RemoveNotificationChannelRequest.
--
-- /See:/ 'newRemoveNotificationChannel' smart constructor.
data RemoveNotificationChannel = RemoveNotificationChannel'
  { -- | The id of the channel that we want to stop receiving notifications.
    channelId :: Prelude.Text,
    -- | The name of the profiling group we want to change notification
    -- configuration for.
    profilingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveNotificationChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelId', 'removeNotificationChannel_channelId' - The id of the channel that we want to stop receiving notifications.
--
-- 'profilingGroupName', 'removeNotificationChannel_profilingGroupName' - The name of the profiling group we want to change notification
-- configuration for.
newRemoveNotificationChannel ::
  -- | 'channelId'
  Prelude.Text ->
  -- | 'profilingGroupName'
  Prelude.Text ->
  RemoveNotificationChannel
newRemoveNotificationChannel
  pChannelId_
  pProfilingGroupName_ =
    RemoveNotificationChannel'
      { channelId = pChannelId_,
        profilingGroupName = pProfilingGroupName_
      }

-- | The id of the channel that we want to stop receiving notifications.
removeNotificationChannel_channelId :: Lens.Lens' RemoveNotificationChannel Prelude.Text
removeNotificationChannel_channelId = Lens.lens (\RemoveNotificationChannel' {channelId} -> channelId) (\s@RemoveNotificationChannel' {} a -> s {channelId = a} :: RemoveNotificationChannel)

-- | The name of the profiling group we want to change notification
-- configuration for.
removeNotificationChannel_profilingGroupName :: Lens.Lens' RemoveNotificationChannel Prelude.Text
removeNotificationChannel_profilingGroupName = Lens.lens (\RemoveNotificationChannel' {profilingGroupName} -> profilingGroupName) (\s@RemoveNotificationChannel' {} a -> s {profilingGroupName = a} :: RemoveNotificationChannel)

instance Core.AWSRequest RemoveNotificationChannel where
  type
    AWSResponse RemoveNotificationChannel =
      RemoveNotificationChannelResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RemoveNotificationChannelResponse'
            Prelude.<$> (x Data..?> "notificationConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveNotificationChannel where
  hashWithSalt _salt RemoveNotificationChannel' {..} =
    _salt
      `Prelude.hashWithSalt` channelId
      `Prelude.hashWithSalt` profilingGroupName

instance Prelude.NFData RemoveNotificationChannel where
  rnf RemoveNotificationChannel' {..} =
    Prelude.rnf channelId
      `Prelude.seq` Prelude.rnf profilingGroupName

instance Data.ToHeaders RemoveNotificationChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath RemoveNotificationChannel where
  toPath RemoveNotificationChannel' {..} =
    Prelude.mconcat
      [ "/profilingGroups/",
        Data.toBS profilingGroupName,
        "/notificationConfiguration/",
        Data.toBS channelId
      ]

instance Data.ToQuery RemoveNotificationChannel where
  toQuery = Prelude.const Prelude.mempty

-- | The structure representing the RemoveNotificationChannelResponse.
--
-- /See:/ 'newRemoveNotificationChannelResponse' smart constructor.
data RemoveNotificationChannelResponse = RemoveNotificationChannelResponse'
  { -- | The new notification configuration for this profiling group.
    notificationConfiguration :: Prelude.Maybe NotificationConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveNotificationChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationConfiguration', 'removeNotificationChannelResponse_notificationConfiguration' - The new notification configuration for this profiling group.
--
-- 'httpStatus', 'removeNotificationChannelResponse_httpStatus' - The response's http status code.
newRemoveNotificationChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveNotificationChannelResponse
newRemoveNotificationChannelResponse pHttpStatus_ =
  RemoveNotificationChannelResponse'
    { notificationConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The new notification configuration for this profiling group.
removeNotificationChannelResponse_notificationConfiguration :: Lens.Lens' RemoveNotificationChannelResponse (Prelude.Maybe NotificationConfiguration)
removeNotificationChannelResponse_notificationConfiguration = Lens.lens (\RemoveNotificationChannelResponse' {notificationConfiguration} -> notificationConfiguration) (\s@RemoveNotificationChannelResponse' {} a -> s {notificationConfiguration = a} :: RemoveNotificationChannelResponse)

-- | The response's http status code.
removeNotificationChannelResponse_httpStatus :: Lens.Lens' RemoveNotificationChannelResponse Prelude.Int
removeNotificationChannelResponse_httpStatus = Lens.lens (\RemoveNotificationChannelResponse' {httpStatus} -> httpStatus) (\s@RemoveNotificationChannelResponse' {} a -> s {httpStatus = a} :: RemoveNotificationChannelResponse)

instance
  Prelude.NFData
    RemoveNotificationChannelResponse
  where
  rnf RemoveNotificationChannelResponse' {..} =
    Prelude.rnf notificationConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
