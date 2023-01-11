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
-- Module      : Amazonka.SMS.NotifyAppValidationOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information to Server Migration Service about whether
-- application validation is successful.
module Amazonka.SMS.NotifyAppValidationOutput
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SMS.Types

-- | /See:/ 'newNotifyAppValidationOutput' smart constructor.
data NotifyAppValidationOutput = NotifyAppValidationOutput'
  { -- | The notification information.
    notificationContext :: Prelude.Maybe NotificationContext,
    -- | The ID of the application.
    appId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest NotifyAppValidationOutput where
  type
    AWSResponse NotifyAppValidationOutput =
      NotifyAppValidationOutputResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          NotifyAppValidationOutputResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable NotifyAppValidationOutput where
  hashWithSalt _salt NotifyAppValidationOutput' {..} =
    _salt `Prelude.hashWithSalt` notificationContext
      `Prelude.hashWithSalt` appId

instance Prelude.NFData NotifyAppValidationOutput where
  rnf NotifyAppValidationOutput' {..} =
    Prelude.rnf notificationContext
      `Prelude.seq` Prelude.rnf appId

instance Data.ToHeaders NotifyAppValidationOutput where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSServerMigrationService_V2016_10_24.NotifyAppValidationOutput" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON NotifyAppValidationOutput where
  toJSON NotifyAppValidationOutput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("notificationContext" Data..=)
              Prelude.<$> notificationContext,
            Prelude.Just ("appId" Data..= appId)
          ]
      )

instance Data.ToPath NotifyAppValidationOutput where
  toPath = Prelude.const "/"

instance Data.ToQuery NotifyAppValidationOutput where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newNotifyAppValidationOutputResponse' smart constructor.
data NotifyAppValidationOutputResponse = NotifyAppValidationOutputResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf NotifyAppValidationOutputResponse' {..} =
    Prelude.rnf httpStatus
