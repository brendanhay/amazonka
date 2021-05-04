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
-- Module      : Network.AWS.WorkDocs.RemoveAllResourcePermissions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes all the permissions from the specified resource.
module Network.AWS.WorkDocs.RemoveAllResourcePermissions
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newRemoveAllResourcePermissions' smart constructor.
data RemoveAllResourcePermissions = RemoveAllResourcePermissions'
  { -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The ID of the resource.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
removeAllResourcePermissions_authenticationToken = Lens.lens (\RemoveAllResourcePermissions' {authenticationToken} -> authenticationToken) (\s@RemoveAllResourcePermissions' {} a -> s {authenticationToken = a} :: RemoveAllResourcePermissions) Prelude.. Lens.mapping Prelude._Sensitive

-- | The ID of the resource.
removeAllResourcePermissions_resourceId :: Lens.Lens' RemoveAllResourcePermissions Prelude.Text
removeAllResourcePermissions_resourceId = Lens.lens (\RemoveAllResourcePermissions' {resourceId} -> resourceId) (\s@RemoveAllResourcePermissions' {} a -> s {resourceId = a} :: RemoveAllResourcePermissions)

instance
  Prelude.AWSRequest
    RemoveAllResourcePermissions
  where
  type
    Rs RemoveAllResourcePermissions =
      RemoveAllResourcePermissionsResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      RemoveAllResourcePermissionsResponse'

instance
  Prelude.Hashable
    RemoveAllResourcePermissions

instance Prelude.NFData RemoveAllResourcePermissions

instance
  Prelude.ToHeaders
    RemoveAllResourcePermissions
  where
  toHeaders RemoveAllResourcePermissions' {..} =
    Prelude.mconcat
      [ "Authentication" Prelude.=# authenticationToken,
        "Content-Type"
          Prelude.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Prelude.ToPath RemoveAllResourcePermissions where
  toPath RemoveAllResourcePermissions' {..} =
    Prelude.mconcat
      [ "/api/v1/resources/",
        Prelude.toBS resourceId,
        "/permissions"
      ]

instance Prelude.ToQuery RemoveAllResourcePermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveAllResourcePermissionsResponse' smart constructor.
data RemoveAllResourcePermissionsResponse = RemoveAllResourcePermissionsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
