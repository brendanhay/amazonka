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
-- Module      : Network.AWS.SMS.NotifyAppValidationOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information to AWS SMS about whether application validation is
-- successful.
module Network.AWS.SMS.NotifyAppValidationOutput
  ( -- * Creating a Request
    NotifyAppValidationOutput (..),
    newNotifyAppValidationOutput,

    -- * Request Lenses
    notifyAppValidationOutput_notificationContext,
    notifyAppValidationOutput_appId,

    -- * Destructuring the Response
    NotifyAppValidationOutputResponse (..),
    newNotifyAppValidationOutputResponse,

    -- * Response Lenses
    notifyAppValidationOutputResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newNotifyAppValidationOutput' smart constructor.
data NotifyAppValidationOutput = NotifyAppValidationOutput'
  { -- | The notification information.
    notificationContext :: Core.Maybe NotificationContext,
    -- | The ID of the application.
    appId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NotifyAppValidationOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationContext', 'notifyAppValidationOutput_notificationContext' - The notification information.
--
-- 'appId', 'notifyAppValidationOutput_appId' - The ID of the application.
newNotifyAppValidationOutput ::
  -- | 'appId'
  Core.Text ->
  NotifyAppValidationOutput
newNotifyAppValidationOutput pAppId_ =
  NotifyAppValidationOutput'
    { notificationContext =
        Core.Nothing,
      appId = pAppId_
    }

-- | The notification information.
notifyAppValidationOutput_notificationContext :: Lens.Lens' NotifyAppValidationOutput (Core.Maybe NotificationContext)
notifyAppValidationOutput_notificationContext = Lens.lens (\NotifyAppValidationOutput' {notificationContext} -> notificationContext) (\s@NotifyAppValidationOutput' {} a -> s {notificationContext = a} :: NotifyAppValidationOutput)

-- | The ID of the application.
notifyAppValidationOutput_appId :: Lens.Lens' NotifyAppValidationOutput Core.Text
notifyAppValidationOutput_appId = Lens.lens (\NotifyAppValidationOutput' {appId} -> appId) (\s@NotifyAppValidationOutput' {} a -> s {appId = a} :: NotifyAppValidationOutput)

instance Core.AWSRequest NotifyAppValidationOutput where
  type
    AWSResponse NotifyAppValidationOutput =
      NotifyAppValidationOutputResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          NotifyAppValidationOutputResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable NotifyAppValidationOutput

instance Core.NFData NotifyAppValidationOutput

instance Core.ToHeaders NotifyAppValidationOutput where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.NotifyAppValidationOutput" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON NotifyAppValidationOutput where
  toJSON NotifyAppValidationOutput' {..} =
    Core.object
      ( Core.catMaybes
          [ ("notificationContext" Core..=)
              Core.<$> notificationContext,
            Core.Just ("appId" Core..= appId)
          ]
      )

instance Core.ToPath NotifyAppValidationOutput where
  toPath = Core.const "/"

instance Core.ToQuery NotifyAppValidationOutput where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newNotifyAppValidationOutputResponse' smart constructor.
data NotifyAppValidationOutputResponse = NotifyAppValidationOutputResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NotifyAppValidationOutputResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'notifyAppValidationOutputResponse_httpStatus' - The response's http status code.
newNotifyAppValidationOutputResponse ::
  -- | 'httpStatus'
  Core.Int ->
  NotifyAppValidationOutputResponse
newNotifyAppValidationOutputResponse pHttpStatus_ =
  NotifyAppValidationOutputResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
notifyAppValidationOutputResponse_httpStatus :: Lens.Lens' NotifyAppValidationOutputResponse Core.Int
notifyAppValidationOutputResponse_httpStatus = Lens.lens (\NotifyAppValidationOutputResponse' {httpStatus} -> httpStatus) (\s@NotifyAppValidationOutputResponse' {} a -> s {httpStatus = a} :: NotifyAppValidationOutputResponse)

instance
  Core.NFData
    NotifyAppValidationOutputResponse
