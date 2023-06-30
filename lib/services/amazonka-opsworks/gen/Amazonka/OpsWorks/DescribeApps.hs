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
-- Module      : Amazonka.OpsWorks.DescribeApps
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.OpsWorks.DescribeApps
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeApps' smart constructor.
data DescribeApps = DescribeApps'
  { -- | An array of app IDs for the apps to be described. If you use this
    -- parameter, @DescribeApps@ returns a description of the specified apps.
    -- Otherwise, it returns a description of every app.
    appIds :: Prelude.Maybe [Prelude.Text],
    -- | The app stack ID. If you use this parameter, @DescribeApps@ returns a
    -- description of the apps in the specified stack.
    stackId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { appIds = Prelude.Nothing,
      stackId = Prelude.Nothing
    }

-- | An array of app IDs for the apps to be described. If you use this
-- parameter, @DescribeApps@ returns a description of the specified apps.
-- Otherwise, it returns a description of every app.
describeApps_appIds :: Lens.Lens' DescribeApps (Prelude.Maybe [Prelude.Text])
describeApps_appIds = Lens.lens (\DescribeApps' {appIds} -> appIds) (\s@DescribeApps' {} a -> s {appIds = a} :: DescribeApps) Prelude.. Lens.mapping Lens.coerced

-- | The app stack ID. If you use this parameter, @DescribeApps@ returns a
-- description of the apps in the specified stack.
describeApps_stackId :: Lens.Lens' DescribeApps (Prelude.Maybe Prelude.Text)
describeApps_stackId = Lens.lens (\DescribeApps' {stackId} -> stackId) (\s@DescribeApps' {} a -> s {stackId = a} :: DescribeApps)

instance Core.AWSRequest DescribeApps where
  type AWSResponse DescribeApps = DescribeAppsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAppsResponse'
            Prelude.<$> (x Data..?> "Apps" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeApps where
  hashWithSalt _salt DescribeApps' {..} =
    _salt
      `Prelude.hashWithSalt` appIds
      `Prelude.hashWithSalt` stackId

instance Prelude.NFData DescribeApps where
  rnf DescribeApps' {..} =
    Prelude.rnf appIds
      `Prelude.seq` Prelude.rnf stackId

instance Data.ToHeaders DescribeApps where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.DescribeApps" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeApps where
  toJSON DescribeApps' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AppIds" Data..=) Prelude.<$> appIds,
            ("StackId" Data..=) Prelude.<$> stackId
          ]
      )

instance Data.ToPath DescribeApps where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeApps where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to a @DescribeApps@ request.
--
-- /See:/ 'newDescribeAppsResponse' smart constructor.
data DescribeAppsResponse = DescribeAppsResponse'
  { -- | An array of @App@ objects that describe the specified apps.
    apps :: Prelude.Maybe [App],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeAppsResponse
newDescribeAppsResponse pHttpStatus_ =
  DescribeAppsResponse'
    { apps = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @App@ objects that describe the specified apps.
describeAppsResponse_apps :: Lens.Lens' DescribeAppsResponse (Prelude.Maybe [App])
describeAppsResponse_apps = Lens.lens (\DescribeAppsResponse' {apps} -> apps) (\s@DescribeAppsResponse' {} a -> s {apps = a} :: DescribeAppsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeAppsResponse_httpStatus :: Lens.Lens' DescribeAppsResponse Prelude.Int
describeAppsResponse_httpStatus = Lens.lens (\DescribeAppsResponse' {httpStatus} -> httpStatus) (\s@DescribeAppsResponse' {} a -> s {httpStatus = a} :: DescribeAppsResponse)

instance Prelude.NFData DescribeAppsResponse where
  rnf DescribeAppsResponse' {..} =
    Prelude.rnf apps
      `Prelude.seq` Prelude.rnf httpStatus
