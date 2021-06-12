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
-- Module      : Network.AWS.SMS.TerminateApp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates the stack for the specified application.
module Network.AWS.SMS.TerminateApp
  ( -- * Creating a Request
    TerminateApp (..),
    newTerminateApp,

    -- * Request Lenses
    terminateApp_appId,

    -- * Destructuring the Response
    TerminateAppResponse (..),
    newTerminateAppResponse,

    -- * Response Lenses
    terminateAppResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newTerminateApp' smart constructor.
data TerminateApp = TerminateApp'
  { -- | The ID of the application.
    appId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TerminateApp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'terminateApp_appId' - The ID of the application.
newTerminateApp ::
  TerminateApp
newTerminateApp = TerminateApp' {appId = Core.Nothing}

-- | The ID of the application.
terminateApp_appId :: Lens.Lens' TerminateApp (Core.Maybe Core.Text)
terminateApp_appId = Lens.lens (\TerminateApp' {appId} -> appId) (\s@TerminateApp' {} a -> s {appId = a} :: TerminateApp)

instance Core.AWSRequest TerminateApp where
  type AWSResponse TerminateApp = TerminateAppResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          TerminateAppResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable TerminateApp

instance Core.NFData TerminateApp

instance Core.ToHeaders TerminateApp where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.TerminateApp" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON TerminateApp where
  toJSON TerminateApp' {..} =
    Core.object
      (Core.catMaybes [("appId" Core..=) Core.<$> appId])

instance Core.ToPath TerminateApp where
  toPath = Core.const "/"

instance Core.ToQuery TerminateApp where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newTerminateAppResponse' smart constructor.
data TerminateAppResponse = TerminateAppResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TerminateAppResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'terminateAppResponse_httpStatus' - The response's http status code.
newTerminateAppResponse ::
  -- | 'httpStatus'
  Core.Int ->
  TerminateAppResponse
newTerminateAppResponse pHttpStatus_ =
  TerminateAppResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
terminateAppResponse_httpStatus :: Lens.Lens' TerminateAppResponse Core.Int
terminateAppResponse_httpStatus = Lens.lens (\TerminateAppResponse' {httpStatus} -> httpStatus) (\s@TerminateAppResponse' {} a -> s {httpStatus = a} :: TerminateAppResponse)

instance Core.NFData TerminateAppResponse
