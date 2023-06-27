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
-- Module      : Amazonka.WorkDocs.RemoveResourcePermission
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the permission for the specified principal from the specified
-- resource.
module Amazonka.WorkDocs.RemoveResourcePermission
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkDocs.Types

-- | /See:/ 'newRemoveResourcePermission' smart constructor.
data RemoveResourcePermission = RemoveResourcePermission'
  { -- | Amazon WorkDocs authentication token. Not required when using Amazon Web
    -- Services administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The principal type of the resource.
    principalType :: Prelude.Maybe PrincipalType,
    -- | The ID of the resource.
    resourceId :: Prelude.Text,
    -- | The principal ID of the resource.
    principalId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveResourcePermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationToken', 'removeResourcePermission_authenticationToken' - Amazon WorkDocs authentication token. Not required when using Amazon Web
-- Services administrator credentials to access the API.
--
-- 'principalType', 'removeResourcePermission_principalType' - The principal type of the resource.
--
-- 'resourceId', 'removeResourcePermission_resourceId' - The ID of the resource.
--
-- 'principalId', 'removeResourcePermission_principalId' - The principal ID of the resource.
newRemoveResourcePermission ::
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'principalId'
  Prelude.Text ->
  RemoveResourcePermission
newRemoveResourcePermission
  pResourceId_
  pPrincipalId_ =
    RemoveResourcePermission'
      { authenticationToken =
          Prelude.Nothing,
        principalType = Prelude.Nothing,
        resourceId = pResourceId_,
        principalId = pPrincipalId_
      }

-- | Amazon WorkDocs authentication token. Not required when using Amazon Web
-- Services administrator credentials to access the API.
removeResourcePermission_authenticationToken :: Lens.Lens' RemoveResourcePermission (Prelude.Maybe Prelude.Text)
removeResourcePermission_authenticationToken = Lens.lens (\RemoveResourcePermission' {authenticationToken} -> authenticationToken) (\s@RemoveResourcePermission' {} a -> s {authenticationToken = a} :: RemoveResourcePermission) Prelude.. Lens.mapping Data._Sensitive

-- | The principal type of the resource.
removeResourcePermission_principalType :: Lens.Lens' RemoveResourcePermission (Prelude.Maybe PrincipalType)
removeResourcePermission_principalType = Lens.lens (\RemoveResourcePermission' {principalType} -> principalType) (\s@RemoveResourcePermission' {} a -> s {principalType = a} :: RemoveResourcePermission)

-- | The ID of the resource.
removeResourcePermission_resourceId :: Lens.Lens' RemoveResourcePermission Prelude.Text
removeResourcePermission_resourceId = Lens.lens (\RemoveResourcePermission' {resourceId} -> resourceId) (\s@RemoveResourcePermission' {} a -> s {resourceId = a} :: RemoveResourcePermission)

-- | The principal ID of the resource.
removeResourcePermission_principalId :: Lens.Lens' RemoveResourcePermission Prelude.Text
removeResourcePermission_principalId = Lens.lens (\RemoveResourcePermission' {principalId} -> principalId) (\s@RemoveResourcePermission' {} a -> s {principalId = a} :: RemoveResourcePermission)

instance Core.AWSRequest RemoveResourcePermission where
  type
    AWSResponse RemoveResourcePermission =
      RemoveResourcePermissionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      RemoveResourcePermissionResponse'

instance Prelude.Hashable RemoveResourcePermission where
  hashWithSalt _salt RemoveResourcePermission' {..} =
    _salt
      `Prelude.hashWithSalt` authenticationToken
      `Prelude.hashWithSalt` principalType
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` principalId

instance Prelude.NFData RemoveResourcePermission where
  rnf RemoveResourcePermission' {..} =
    Prelude.rnf authenticationToken
      `Prelude.seq` Prelude.rnf principalType
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf principalId

instance Data.ToHeaders RemoveResourcePermission where
  toHeaders RemoveResourcePermission' {..} =
    Prelude.mconcat
      [ "Authentication" Data.=# authenticationToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToPath RemoveResourcePermission where
  toPath RemoveResourcePermission' {..} =
    Prelude.mconcat
      [ "/api/v1/resources/",
        Data.toBS resourceId,
        "/permissions/",
        Data.toBS principalId
      ]

instance Data.ToQuery RemoveResourcePermission where
  toQuery RemoveResourcePermission' {..} =
    Prelude.mconcat ["type" Data.=: principalType]

-- | /See:/ 'newRemoveResourcePermissionResponse' smart constructor.
data RemoveResourcePermissionResponse = RemoveResourcePermissionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveResourcePermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRemoveResourcePermissionResponse ::
  RemoveResourcePermissionResponse
newRemoveResourcePermissionResponse =
  RemoveResourcePermissionResponse'

instance
  Prelude.NFData
    RemoveResourcePermissionResponse
  where
  rnf _ = ()
