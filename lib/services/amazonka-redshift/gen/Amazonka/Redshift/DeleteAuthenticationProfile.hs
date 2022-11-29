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
-- Module      : Amazonka.Redshift.DeleteAuthenticationProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an authentication profile.
module Amazonka.Redshift.DeleteAuthenticationProfile
  ( -- * Creating a Request
    DeleteAuthenticationProfile (..),
    newDeleteAuthenticationProfile,

    -- * Request Lenses
    deleteAuthenticationProfile_authenticationProfileName,

    -- * Destructuring the Response
    DeleteAuthenticationProfileResponse (..),
    newDeleteAuthenticationProfileResponse,

    -- * Response Lenses
    deleteAuthenticationProfileResponse_authenticationProfileName,
    deleteAuthenticationProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAuthenticationProfile' smart constructor.
data DeleteAuthenticationProfile = DeleteAuthenticationProfile'
  { -- | The name of the authentication profile to delete.
    authenticationProfileName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAuthenticationProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationProfileName', 'deleteAuthenticationProfile_authenticationProfileName' - The name of the authentication profile to delete.
newDeleteAuthenticationProfile ::
  -- | 'authenticationProfileName'
  Prelude.Text ->
  DeleteAuthenticationProfile
newDeleteAuthenticationProfile
  pAuthenticationProfileName_ =
    DeleteAuthenticationProfile'
      { authenticationProfileName =
          pAuthenticationProfileName_
      }

-- | The name of the authentication profile to delete.
deleteAuthenticationProfile_authenticationProfileName :: Lens.Lens' DeleteAuthenticationProfile Prelude.Text
deleteAuthenticationProfile_authenticationProfileName = Lens.lens (\DeleteAuthenticationProfile' {authenticationProfileName} -> authenticationProfileName) (\s@DeleteAuthenticationProfile' {} a -> s {authenticationProfileName = a} :: DeleteAuthenticationProfile)

instance Core.AWSRequest DeleteAuthenticationProfile where
  type
    AWSResponse DeleteAuthenticationProfile =
      DeleteAuthenticationProfileResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteAuthenticationProfileResult"
      ( \s h x ->
          DeleteAuthenticationProfileResponse'
            Prelude.<$> (x Core..@? "AuthenticationProfileName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAuthenticationProfile where
  hashWithSalt _salt DeleteAuthenticationProfile' {..} =
    _salt
      `Prelude.hashWithSalt` authenticationProfileName

instance Prelude.NFData DeleteAuthenticationProfile where
  rnf DeleteAuthenticationProfile' {..} =
    Prelude.rnf authenticationProfileName

instance Core.ToHeaders DeleteAuthenticationProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteAuthenticationProfile where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteAuthenticationProfile where
  toQuery DeleteAuthenticationProfile' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DeleteAuthenticationProfile" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "AuthenticationProfileName"
          Core.=: authenticationProfileName
      ]

-- | /See:/ 'newDeleteAuthenticationProfileResponse' smart constructor.
data DeleteAuthenticationProfileResponse = DeleteAuthenticationProfileResponse'
  { -- | The name of the authentication profile that was deleted.
    authenticationProfileName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAuthenticationProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationProfileName', 'deleteAuthenticationProfileResponse_authenticationProfileName' - The name of the authentication profile that was deleted.
--
-- 'httpStatus', 'deleteAuthenticationProfileResponse_httpStatus' - The response's http status code.
newDeleteAuthenticationProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAuthenticationProfileResponse
newDeleteAuthenticationProfileResponse pHttpStatus_ =
  DeleteAuthenticationProfileResponse'
    { authenticationProfileName =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the authentication profile that was deleted.
deleteAuthenticationProfileResponse_authenticationProfileName :: Lens.Lens' DeleteAuthenticationProfileResponse (Prelude.Maybe Prelude.Text)
deleteAuthenticationProfileResponse_authenticationProfileName = Lens.lens (\DeleteAuthenticationProfileResponse' {authenticationProfileName} -> authenticationProfileName) (\s@DeleteAuthenticationProfileResponse' {} a -> s {authenticationProfileName = a} :: DeleteAuthenticationProfileResponse)

-- | The response's http status code.
deleteAuthenticationProfileResponse_httpStatus :: Lens.Lens' DeleteAuthenticationProfileResponse Prelude.Int
deleteAuthenticationProfileResponse_httpStatus = Lens.lens (\DeleteAuthenticationProfileResponse' {httpStatus} -> httpStatus) (\s@DeleteAuthenticationProfileResponse' {} a -> s {httpStatus = a} :: DeleteAuthenticationProfileResponse)

instance
  Prelude.NFData
    DeleteAuthenticationProfileResponse
  where
  rnf DeleteAuthenticationProfileResponse' {..} =
    Prelude.rnf authenticationProfileName
      `Prelude.seq` Prelude.rnf httpStatus
