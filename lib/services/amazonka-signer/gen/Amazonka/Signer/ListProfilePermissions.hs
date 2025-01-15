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
-- Module      : Amazonka.Signer.ListProfilePermissions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the cross-account permissions associated with a signing profile.
module Amazonka.Signer.ListProfilePermissions
  ( -- * Creating a Request
    ListProfilePermissions (..),
    newListProfilePermissions,

    -- * Request Lenses
    listProfilePermissions_nextToken,
    listProfilePermissions_profileName,

    -- * Destructuring the Response
    ListProfilePermissionsResponse (..),
    newListProfilePermissionsResponse,

    -- * Response Lenses
    listProfilePermissionsResponse_nextToken,
    listProfilePermissionsResponse_permissions,
    listProfilePermissionsResponse_policySizeBytes,
    listProfilePermissionsResponse_revisionId,
    listProfilePermissionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Signer.Types

-- | /See:/ 'newListProfilePermissions' smart constructor.
data ListProfilePermissions = ListProfilePermissions'
  { -- | String for specifying the next set of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Name of the signing profile containing the cross-account permissions.
    profileName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProfilePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProfilePermissions_nextToken' - String for specifying the next set of paginated results.
--
-- 'profileName', 'listProfilePermissions_profileName' - Name of the signing profile containing the cross-account permissions.
newListProfilePermissions ::
  -- | 'profileName'
  Prelude.Text ->
  ListProfilePermissions
newListProfilePermissions pProfileName_ =
  ListProfilePermissions'
    { nextToken =
        Prelude.Nothing,
      profileName = pProfileName_
    }

-- | String for specifying the next set of paginated results.
listProfilePermissions_nextToken :: Lens.Lens' ListProfilePermissions (Prelude.Maybe Prelude.Text)
listProfilePermissions_nextToken = Lens.lens (\ListProfilePermissions' {nextToken} -> nextToken) (\s@ListProfilePermissions' {} a -> s {nextToken = a} :: ListProfilePermissions)

-- | Name of the signing profile containing the cross-account permissions.
listProfilePermissions_profileName :: Lens.Lens' ListProfilePermissions Prelude.Text
listProfilePermissions_profileName = Lens.lens (\ListProfilePermissions' {profileName} -> profileName) (\s@ListProfilePermissions' {} a -> s {profileName = a} :: ListProfilePermissions)

instance Core.AWSRequest ListProfilePermissions where
  type
    AWSResponse ListProfilePermissions =
      ListProfilePermissionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProfilePermissionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "permissions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "policySizeBytes")
            Prelude.<*> (x Data..?> "revisionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListProfilePermissions where
  hashWithSalt _salt ListProfilePermissions' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` profileName

instance Prelude.NFData ListProfilePermissions where
  rnf ListProfilePermissions' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf profileName

instance Data.ToHeaders ListProfilePermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListProfilePermissions where
  toPath ListProfilePermissions' {..} =
    Prelude.mconcat
      [ "/signing-profiles/",
        Data.toBS profileName,
        "/permissions"
      ]

instance Data.ToQuery ListProfilePermissions where
  toQuery ListProfilePermissions' {..} =
    Prelude.mconcat ["nextToken" Data.=: nextToken]

-- | /See:/ 'newListProfilePermissionsResponse' smart constructor.
data ListProfilePermissionsResponse = ListProfilePermissionsResponse'
  { -- | String for specifying the next set of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of permissions associated with the Signing Profile.
    permissions :: Prelude.Maybe [Permission],
    -- | Total size of the policy associated with the Signing Profile in bytes.
    policySizeBytes :: Prelude.Maybe Prelude.Int,
    -- | The identifier for the current revision of profile permissions.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProfilePermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProfilePermissionsResponse_nextToken' - String for specifying the next set of paginated results.
--
-- 'permissions', 'listProfilePermissionsResponse_permissions' - List of permissions associated with the Signing Profile.
--
-- 'policySizeBytes', 'listProfilePermissionsResponse_policySizeBytes' - Total size of the policy associated with the Signing Profile in bytes.
--
-- 'revisionId', 'listProfilePermissionsResponse_revisionId' - The identifier for the current revision of profile permissions.
--
-- 'httpStatus', 'listProfilePermissionsResponse_httpStatus' - The response's http status code.
newListProfilePermissionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProfilePermissionsResponse
newListProfilePermissionsResponse pHttpStatus_ =
  ListProfilePermissionsResponse'
    { nextToken =
        Prelude.Nothing,
      permissions = Prelude.Nothing,
      policySizeBytes = Prelude.Nothing,
      revisionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | String for specifying the next set of paginated results.
listProfilePermissionsResponse_nextToken :: Lens.Lens' ListProfilePermissionsResponse (Prelude.Maybe Prelude.Text)
listProfilePermissionsResponse_nextToken = Lens.lens (\ListProfilePermissionsResponse' {nextToken} -> nextToken) (\s@ListProfilePermissionsResponse' {} a -> s {nextToken = a} :: ListProfilePermissionsResponse)

-- | List of permissions associated with the Signing Profile.
listProfilePermissionsResponse_permissions :: Lens.Lens' ListProfilePermissionsResponse (Prelude.Maybe [Permission])
listProfilePermissionsResponse_permissions = Lens.lens (\ListProfilePermissionsResponse' {permissions} -> permissions) (\s@ListProfilePermissionsResponse' {} a -> s {permissions = a} :: ListProfilePermissionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Total size of the policy associated with the Signing Profile in bytes.
listProfilePermissionsResponse_policySizeBytes :: Lens.Lens' ListProfilePermissionsResponse (Prelude.Maybe Prelude.Int)
listProfilePermissionsResponse_policySizeBytes = Lens.lens (\ListProfilePermissionsResponse' {policySizeBytes} -> policySizeBytes) (\s@ListProfilePermissionsResponse' {} a -> s {policySizeBytes = a} :: ListProfilePermissionsResponse)

-- | The identifier for the current revision of profile permissions.
listProfilePermissionsResponse_revisionId :: Lens.Lens' ListProfilePermissionsResponse (Prelude.Maybe Prelude.Text)
listProfilePermissionsResponse_revisionId = Lens.lens (\ListProfilePermissionsResponse' {revisionId} -> revisionId) (\s@ListProfilePermissionsResponse' {} a -> s {revisionId = a} :: ListProfilePermissionsResponse)

-- | The response's http status code.
listProfilePermissionsResponse_httpStatus :: Lens.Lens' ListProfilePermissionsResponse Prelude.Int
listProfilePermissionsResponse_httpStatus = Lens.lens (\ListProfilePermissionsResponse' {httpStatus} -> httpStatus) (\s@ListProfilePermissionsResponse' {} a -> s {httpStatus = a} :: ListProfilePermissionsResponse)

instance
  Prelude.NFData
    ListProfilePermissionsResponse
  where
  rnf ListProfilePermissionsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf permissions `Prelude.seq`
        Prelude.rnf policySizeBytes `Prelude.seq`
          Prelude.rnf revisionId `Prelude.seq`
            Prelude.rnf httpStatus
