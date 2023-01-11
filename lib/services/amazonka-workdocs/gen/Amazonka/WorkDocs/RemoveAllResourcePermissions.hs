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
-- Module      : Amazonka.WorkDocs.RemoveAllResourcePermissions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes all the permissions from the specified resource.
module Amazonka.WorkDocs.RemoveAllResourcePermissions
  ( -- * Creating a Request
    RemoveAllResourcePermissions (..),
    newRemoveAllResourcePermissions,

    -- * Request Lenses
    removeAllResourcePermissions_authenticationToken,
    removeAllResourcePermissions_resourceId,

    -- * Destructuring the Response
    RemoveAllResourcePermissionsResponse (..),
    newRemoveAllResourcePermissionsResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkDocs.Types

-- | /See:/ 'newRemoveAllResourcePermissions' smart constructor.
data RemoveAllResourcePermissions = RemoveAllResourcePermissions'
  { -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ID of the resource.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveAllResourcePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationToken', 'removeAllResourcePermissions_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'resourceId', 'removeAllResourcePermissions_resourceId' - The ID of the resource.
newRemoveAllResourcePermissions ::
  -- | 'resourceId'
  Prelude.Text ->
  RemoveAllResourcePermissions
newRemoveAllResourcePermissions pResourceId_ =
  RemoveAllResourcePermissions'
    { authenticationToken =
        Prelude.Nothing,
      resourceId = pResourceId_
    }

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
removeAllResourcePermissions_authenticationToken :: Lens.Lens' RemoveAllResourcePermissions (Prelude.Maybe Prelude.Text)
removeAllResourcePermissions_authenticationToken = Lens.lens (\RemoveAllResourcePermissions' {authenticationToken} -> authenticationToken) (\s@RemoveAllResourcePermissions' {} a -> s {authenticationToken = a} :: RemoveAllResourcePermissions) Prelude.. Lens.mapping Data._Sensitive

-- | The ID of the resource.
removeAllResourcePermissions_resourceId :: Lens.Lens' RemoveAllResourcePermissions Prelude.Text
removeAllResourcePermissions_resourceId = Lens.lens (\RemoveAllResourcePermissions' {resourceId} -> resourceId) (\s@RemoveAllResourcePermissions' {} a -> s {resourceId = a} :: RemoveAllResourcePermissions)

instance Core.AWSRequest RemoveAllResourcePermissions where
  type
    AWSResponse RemoveAllResourcePermissions =
      RemoveAllResourcePermissionsResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      RemoveAllResourcePermissionsResponse'

instance
  Prelude.Hashable
    RemoveAllResourcePermissions
  where
  hashWithSalt _salt RemoveAllResourcePermissions' {..} =
    _salt `Prelude.hashWithSalt` authenticationToken
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData RemoveAllResourcePermissions where
  rnf RemoveAllResourcePermissions' {..} =
    Prelude.rnf authenticationToken
      `Prelude.seq` Prelude.rnf resourceId

instance Data.ToHeaders RemoveAllResourcePermissions where
  toHeaders RemoveAllResourcePermissions' {..} =
    Prelude.mconcat
      [ "Authentication" Data.=# authenticationToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToPath RemoveAllResourcePermissions where
  toPath RemoveAllResourcePermissions' {..} =
    Prelude.mconcat
      [ "/api/v1/resources/",
        Data.toBS resourceId,
        "/permissions"
      ]

instance Data.ToQuery RemoveAllResourcePermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveAllResourcePermissionsResponse' smart constructor.
data RemoveAllResourcePermissionsResponse = RemoveAllResourcePermissionsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveAllResourcePermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRemoveAllResourcePermissionsResponse ::
  RemoveAllResourcePermissionsResponse
newRemoveAllResourcePermissionsResponse =
  RemoveAllResourcePermissionsResponse'

instance
  Prelude.NFData
    RemoveAllResourcePermissionsResponse
  where
  rnf _ = ()
