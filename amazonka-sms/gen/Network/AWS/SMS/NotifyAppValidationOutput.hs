{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newNotifyAppValidationOutput' smart constructor.
data NotifyAppValidationOutput = NotifyAppValidationOutput'
  { -- | The notification information.
    notificationContext :: Prelude.Maybe NotificationContext,
    -- | The ID of the application.
    appId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  NotifyAppValidationOutput
newNotifyAppValidationOutput pAppId_ =
  NotifyAppValidationOutput'
    { notificationContext =
        Prelude.Nothing,
      appId = pAppId_
    }

-- | The notification information.
notifyAppValidationOutput_notificationContext :: Lens.Lens' NotifyAppValidationOutput (Prelude.Maybe NotificationContext)
notifyAppValidationOutput_notificationContext = Lens.lens (\NotifyAppValidationOutput' {notificationContext} -> notificationContext) (\s@NotifyAppValidationOutput' {} a -> s {notificationContext = a} :: NotifyAppValidationOutput)

-- | The ID of the application.
notifyAppValidationOutput_appId :: Lens.Lens' NotifyAppValidationOutput Prelude.Text
notifyAppValidationOutput_appId = Lens.lens (\NotifyAppValidationOutput' {appId} -> appId) (\s@NotifyAppValidationOutput' {} a -> s {appId = a} :: NotifyAppValidationOutput)

instance Prelude.AWSRequest NotifyAppValidationOutput where
  type
    Rs NotifyAppValidationOutput =
      NotifyAppValidationOutputResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          NotifyAppValidationOutputResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable NotifyAppValidationOutput

instance Prelude.NFData NotifyAppValidationOutput

instance Prelude.ToHeaders NotifyAppValidationOutput where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSServerMigrationService_V2016_10_24.NotifyAppValidationOutput" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON NotifyAppValidationOutput where
  toJSON NotifyAppValidationOutput' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("notificationContext" Prelude..=)
              Prelude.<$> notificationContext,
            Prelude.Just ("appId" Prelude..= appId)
          ]
      )

instance Prelude.ToPath NotifyAppValidationOutput where
  toPath = Prelude.const "/"

instance Prelude.ToQuery NotifyAppValidationOutput where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newNotifyAppValidationOutputResponse' smart constructor.
data NotifyAppValidationOutputResponse = NotifyAppValidationOutputResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  NotifyAppValidationOutputResponse
newNotifyAppValidationOutputResponse pHttpStatus_ =
  NotifyAppValidationOutputResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
notifyAppValidationOutputResponse_httpStatus :: Lens.Lens' NotifyAppValidationOutputResponse Prelude.Int
notifyAppValidationOutputResponse_httpStatus = Lens.lens (\NotifyAppValidationOutputResponse' {httpStatus} -> httpStatus) (\s@NotifyAppValidationOutputResponse' {} a -> s {httpStatus = a} :: NotifyAppValidationOutputResponse)

instance
  Prelude.NFData
    NotifyAppValidationOutputResponse
