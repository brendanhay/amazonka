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
-- Module      : Network.AWS.SMS.LaunchApp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Launches the specified application as a stack in AWS CloudFormation.
module Network.AWS.SMS.LaunchApp
  ( -- * Creating a Request
    LaunchApp (..),
    newLaunchApp,

    -- * Request Lenses
    launchApp_appId,

    -- * Destructuring the Response
    LaunchAppResponse (..),
    newLaunchAppResponse,

    -- * Response Lenses
    launchAppResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newLaunchApp' smart constructor.
data LaunchApp = LaunchApp'
  { -- | The ID of the application.
    appId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LaunchApp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'launchApp_appId' - The ID of the application.
newLaunchApp ::
  LaunchApp
newLaunchApp = LaunchApp' {appId = Core.Nothing}

-- | The ID of the application.
launchApp_appId :: Lens.Lens' LaunchApp (Core.Maybe Core.Text)
launchApp_appId = Lens.lens (\LaunchApp' {appId} -> appId) (\s@LaunchApp' {} a -> s {appId = a} :: LaunchApp)

instance Core.AWSRequest LaunchApp where
  type AWSResponse LaunchApp = LaunchAppResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          LaunchAppResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable LaunchApp

instance Core.NFData LaunchApp

instance Core.ToHeaders LaunchApp where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.LaunchApp" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON LaunchApp where
  toJSON LaunchApp' {..} =
    Core.object
      (Core.catMaybes [("appId" Core..=) Core.<$> appId])

instance Core.ToPath LaunchApp where
  toPath = Core.const "/"

instance Core.ToQuery LaunchApp where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newLaunchAppResponse' smart constructor.
data LaunchAppResponse = LaunchAppResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LaunchAppResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'launchAppResponse_httpStatus' - The response's http status code.
newLaunchAppResponse ::
  -- | 'httpStatus'
  Core.Int ->
  LaunchAppResponse
newLaunchAppResponse pHttpStatus_ =
  LaunchAppResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
launchAppResponse_httpStatus :: Lens.Lens' LaunchAppResponse Core.Int
launchAppResponse_httpStatus = Lens.lens (\LaunchAppResponse' {httpStatus} -> httpStatus) (\s@LaunchAppResponse' {} a -> s {httpStatus = a} :: LaunchAppResponse)

instance Core.NFData LaunchAppResponse
