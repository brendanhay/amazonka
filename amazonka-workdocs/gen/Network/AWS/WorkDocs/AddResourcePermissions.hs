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
-- Module      : Network.AWS.WorkDocs.AddResourcePermissions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a set of permissions for the specified folder or document. The
-- resource permissions are overwritten if the principals already have
-- different permissions.
module Network.AWS.WorkDocs.AddResourcePermissions
  ( -- * Creating a Request
    AddResourcePermissions (..),
    newAddResourcePermissions,

    -- * Request Lenses
    addResourcePermissions_notificationOptions,
    addResourcePermissions_authenticationToken,
    addResourcePermissions_resourceId,
    addResourcePermissions_principals,

    -- * Destructuring the Response
    AddResourcePermissionsResponse (..),
    newAddResourcePermissionsResponse,

    -- * Response Lenses
    addResourcePermissionsResponse_shareResults,
    addResourcePermissionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newAddResourcePermissions' smart constructor.
data AddResourcePermissions = AddResourcePermissions'
  { -- | The notification options.
    notificationOptions :: Prelude.Maybe NotificationOptions,
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The ID of the resource.
    resourceId :: Prelude.Text,
    -- | The users, groups, or organization being granted permission.
    principals :: [SharePrincipal]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddResourcePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationOptions', 'addResourcePermissions_notificationOptions' - The notification options.
--
-- 'authenticationToken', 'addResourcePermissions_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'resourceId', 'addResourcePermissions_resourceId' - The ID of the resource.
--
-- 'principals', 'addResourcePermissions_principals' - The users, groups, or organization being granted permission.
newAddResourcePermissions ::
  -- | 'resourceId'
  Prelude.Text ->
  AddResourcePermissions
newAddResourcePermissions pResourceId_ =
  AddResourcePermissions'
    { notificationOptions =
        Prelude.Nothing,
      authenticationToken = Prelude.Nothing,
      resourceId = pResourceId_,
      principals = Prelude.mempty
    }

-- | The notification options.
addResourcePermissions_notificationOptions :: Lens.Lens' AddResourcePermissions (Prelude.Maybe NotificationOptions)
addResourcePermissions_notificationOptions = Lens.lens (\AddResourcePermissions' {notificationOptions} -> notificationOptions) (\s@AddResourcePermissions' {} a -> s {notificationOptions = a} :: AddResourcePermissions)

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
addResourcePermissions_authenticationToken :: Lens.Lens' AddResourcePermissions (Prelude.Maybe Prelude.Text)
addResourcePermissions_authenticationToken = Lens.lens (\AddResourcePermissions' {authenticationToken} -> authenticationToken) (\s@AddResourcePermissions' {} a -> s {authenticationToken = a} :: AddResourcePermissions) Prelude.. Lens.mapping Core._Sensitive

-- | The ID of the resource.
addResourcePermissions_resourceId :: Lens.Lens' AddResourcePermissions Prelude.Text
addResourcePermissions_resourceId = Lens.lens (\AddResourcePermissions' {resourceId} -> resourceId) (\s@AddResourcePermissions' {} a -> s {resourceId = a} :: AddResourcePermissions)

-- | The users, groups, or organization being granted permission.
addResourcePermissions_principals :: Lens.Lens' AddResourcePermissions [SharePrincipal]
addResourcePermissions_principals = Lens.lens (\AddResourcePermissions' {principals} -> principals) (\s@AddResourcePermissions' {} a -> s {principals = a} :: AddResourcePermissions) Prelude.. Lens._Coerce

instance Core.AWSRequest AddResourcePermissions where
  type
    AWSResponse AddResourcePermissions =
      AddResourcePermissionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AddResourcePermissionsResponse'
            Prelude.<$> (x Core..?> "ShareResults" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddResourcePermissions

instance Prelude.NFData AddResourcePermissions

instance Core.ToHeaders AddResourcePermissions where
  toHeaders AddResourcePermissions' {..} =
    Prelude.mconcat
      [ "Authentication" Core.=# authenticationToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToJSON AddResourcePermissions where
  toJSON AddResourcePermissions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NotificationOptions" Core..=)
              Prelude.<$> notificationOptions,
            Prelude.Just ("Principals" Core..= principals)
          ]
      )

instance Core.ToPath AddResourcePermissions where
  toPath AddResourcePermissions' {..} =
    Prelude.mconcat
      [ "/api/v1/resources/",
        Core.toBS resourceId,
        "/permissions"
      ]

instance Core.ToQuery AddResourcePermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddResourcePermissionsResponse' smart constructor.
data AddResourcePermissionsResponse = AddResourcePermissionsResponse'
  { -- | The share results.
    shareResults :: Prelude.Maybe [ShareResult],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddResourcePermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shareResults', 'addResourcePermissionsResponse_shareResults' - The share results.
--
-- 'httpStatus', 'addResourcePermissionsResponse_httpStatus' - The response's http status code.
newAddResourcePermissionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddResourcePermissionsResponse
newAddResourcePermissionsResponse pHttpStatus_ =
  AddResourcePermissionsResponse'
    { shareResults =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The share results.
addResourcePermissionsResponse_shareResults :: Lens.Lens' AddResourcePermissionsResponse (Prelude.Maybe [ShareResult])
addResourcePermissionsResponse_shareResults = Lens.lens (\AddResourcePermissionsResponse' {shareResults} -> shareResults) (\s@AddResourcePermissionsResponse' {} a -> s {shareResults = a} :: AddResourcePermissionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
addResourcePermissionsResponse_httpStatus :: Lens.Lens' AddResourcePermissionsResponse Prelude.Int
addResourcePermissionsResponse_httpStatus = Lens.lens (\AddResourcePermissionsResponse' {httpStatus} -> httpStatus) (\s@AddResourcePermissionsResponse' {} a -> s {httpStatus = a} :: AddResourcePermissionsResponse)

instance
  Prelude.NFData
    AddResourcePermissionsResponse
