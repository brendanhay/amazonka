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
-- Module      : Network.AWS.WorkDocs.RemoveResourcePermission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the permission for the specified principal from the specified
-- resource.
module Network.AWS.WorkDocs.RemoveResourcePermission
  ( -- * Creating a Request
    RemoveResourcePermission (..),
    newRemoveResourcePermission,

    -- * Request Lenses
    removeResourcePermission_authenticationToken,
    removeResourcePermission_principalType,
    removeResourcePermission_resourceId,
    removeResourcePermission_principalId,

    -- * Destructuring the Response
    RemoveResourcePermissionResponse (..),
    newRemoveResourcePermissionResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newRemoveResourcePermission' smart constructor.
data RemoveResourcePermission = RemoveResourcePermission'
  { -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The principal type of the resource.
    principalType :: Core.Maybe PrincipalType,
    -- | The ID of the resource.
    resourceId :: Core.Text,
    -- | The principal ID of the resource.
    principalId :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemoveResourcePermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationToken', 'removeResourcePermission_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'principalType', 'removeResourcePermission_principalType' - The principal type of the resource.
--
-- 'resourceId', 'removeResourcePermission_resourceId' - The ID of the resource.
--
-- 'principalId', 'removeResourcePermission_principalId' - The principal ID of the resource.
newRemoveResourcePermission ::
  -- | 'resourceId'
  Core.Text ->
  -- | 'principalId'
  Core.Text ->
  RemoveResourcePermission
newRemoveResourcePermission
  pResourceId_
  pPrincipalId_ =
    RemoveResourcePermission'
      { authenticationToken =
          Core.Nothing,
        principalType = Core.Nothing,
        resourceId = pResourceId_,
        principalId = pPrincipalId_
      }

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
removeResourcePermission_authenticationToken :: Lens.Lens' RemoveResourcePermission (Core.Maybe Core.Text)
removeResourcePermission_authenticationToken = Lens.lens (\RemoveResourcePermission' {authenticationToken} -> authenticationToken) (\s@RemoveResourcePermission' {} a -> s {authenticationToken = a} :: RemoveResourcePermission) Core.. Lens.mapping Core._Sensitive

-- | The principal type of the resource.
removeResourcePermission_principalType :: Lens.Lens' RemoveResourcePermission (Core.Maybe PrincipalType)
removeResourcePermission_principalType = Lens.lens (\RemoveResourcePermission' {principalType} -> principalType) (\s@RemoveResourcePermission' {} a -> s {principalType = a} :: RemoveResourcePermission)

-- | The ID of the resource.
removeResourcePermission_resourceId :: Lens.Lens' RemoveResourcePermission Core.Text
removeResourcePermission_resourceId = Lens.lens (\RemoveResourcePermission' {resourceId} -> resourceId) (\s@RemoveResourcePermission' {} a -> s {resourceId = a} :: RemoveResourcePermission)

-- | The principal ID of the resource.
removeResourcePermission_principalId :: Lens.Lens' RemoveResourcePermission Core.Text
removeResourcePermission_principalId = Lens.lens (\RemoveResourcePermission' {principalId} -> principalId) (\s@RemoveResourcePermission' {} a -> s {principalId = a} :: RemoveResourcePermission)

instance Core.AWSRequest RemoveResourcePermission where
  type
    AWSResponse RemoveResourcePermission =
      RemoveResourcePermissionResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      RemoveResourcePermissionResponse'

instance Core.Hashable RemoveResourcePermission

instance Core.NFData RemoveResourcePermission

instance Core.ToHeaders RemoveResourcePermission where
  toHeaders RemoveResourcePermission' {..} =
    Core.mconcat
      [ "Authentication" Core.=# authenticationToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToPath RemoveResourcePermission where
  toPath RemoveResourcePermission' {..} =
    Core.mconcat
      [ "/api/v1/resources/",
        Core.toBS resourceId,
        "/permissions/",
        Core.toBS principalId
      ]

instance Core.ToQuery RemoveResourcePermission where
  toQuery RemoveResourcePermission' {..} =
    Core.mconcat ["type" Core.=: principalType]

-- | /See:/ 'newRemoveResourcePermissionResponse' smart constructor.
data RemoveResourcePermissionResponse = RemoveResourcePermissionResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemoveResourcePermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRemoveResourcePermissionResponse ::
  RemoveResourcePermissionResponse
newRemoveResourcePermissionResponse =
  RemoveResourcePermissionResponse'

instance Core.NFData RemoveResourcePermissionResponse
