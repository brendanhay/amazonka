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
-- Module      : Network.AWS.OpsWorks.DescribeApps
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a description of a specified set of apps.
--
-- This call accepts only one resource-identifying parameter.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information about
-- user permissions, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.DescribeApps
  ( -- * Creating a Request
    DescribeApps (..),
    newDescribeApps,

    -- * Request Lenses
    describeApps_appIds,
    describeApps_stackId,

    -- * Destructuring the Response
    DescribeAppsResponse (..),
    newDescribeAppsResponse,

    -- * Response Lenses
    describeAppsResponse_apps,
    describeAppsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeApps' smart constructor.
data DescribeApps = DescribeApps'
  { -- | An array of app IDs for the apps to be described. If you use this
    -- parameter, @DescribeApps@ returns a description of the specified apps.
    -- Otherwise, it returns a description of every app.
    appIds :: Core.Maybe [Core.Text],
    -- | The app stack ID. If you use this parameter, @DescribeApps@ returns a
    -- description of the apps in the specified stack.
    stackId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeApps' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appIds', 'describeApps_appIds' - An array of app IDs for the apps to be described. If you use this
-- parameter, @DescribeApps@ returns a description of the specified apps.
-- Otherwise, it returns a description of every app.
--
-- 'stackId', 'describeApps_stackId' - The app stack ID. If you use this parameter, @DescribeApps@ returns a
-- description of the apps in the specified stack.
newDescribeApps ::
  DescribeApps
newDescribeApps =
  DescribeApps'
    { appIds = Core.Nothing,
      stackId = Core.Nothing
    }

-- | An array of app IDs for the apps to be described. If you use this
-- parameter, @DescribeApps@ returns a description of the specified apps.
-- Otherwise, it returns a description of every app.
describeApps_appIds :: Lens.Lens' DescribeApps (Core.Maybe [Core.Text])
describeApps_appIds = Lens.lens (\DescribeApps' {appIds} -> appIds) (\s@DescribeApps' {} a -> s {appIds = a} :: DescribeApps) Core.. Lens.mapping Lens._Coerce

-- | The app stack ID. If you use this parameter, @DescribeApps@ returns a
-- description of the apps in the specified stack.
describeApps_stackId :: Lens.Lens' DescribeApps (Core.Maybe Core.Text)
describeApps_stackId = Lens.lens (\DescribeApps' {stackId} -> stackId) (\s@DescribeApps' {} a -> s {stackId = a} :: DescribeApps)

instance Core.AWSRequest DescribeApps where
  type AWSResponse DescribeApps = DescribeAppsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAppsResponse'
            Core.<$> (x Core..?> "Apps" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeApps

instance Core.NFData DescribeApps

instance Core.ToHeaders DescribeApps where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.DescribeApps" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeApps where
  toJSON DescribeApps' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AppIds" Core..=) Core.<$> appIds,
            ("StackId" Core..=) Core.<$> stackId
          ]
      )

instance Core.ToPath DescribeApps where
  toPath = Core.const "/"

instance Core.ToQuery DescribeApps where
  toQuery = Core.const Core.mempty

-- | Contains the response to a @DescribeApps@ request.
--
-- /See:/ 'newDescribeAppsResponse' smart constructor.
data DescribeAppsResponse = DescribeAppsResponse'
  { -- | An array of @App@ objects that describe the specified apps.
    apps :: Core.Maybe [App],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAppsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apps', 'describeAppsResponse_apps' - An array of @App@ objects that describe the specified apps.
--
-- 'httpStatus', 'describeAppsResponse_httpStatus' - The response's http status code.
newDescribeAppsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeAppsResponse
newDescribeAppsResponse pHttpStatus_ =
  DescribeAppsResponse'
    { apps = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @App@ objects that describe the specified apps.
describeAppsResponse_apps :: Lens.Lens' DescribeAppsResponse (Core.Maybe [App])
describeAppsResponse_apps = Lens.lens (\DescribeAppsResponse' {apps} -> apps) (\s@DescribeAppsResponse' {} a -> s {apps = a} :: DescribeAppsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeAppsResponse_httpStatus :: Lens.Lens' DescribeAppsResponse Core.Int
describeAppsResponse_httpStatus = Lens.lens (\DescribeAppsResponse' {httpStatus} -> httpStatus) (\s@DescribeAppsResponse' {} a -> s {httpStatus = a} :: DescribeAppsResponse)

instance Core.NFData DescribeAppsResponse
