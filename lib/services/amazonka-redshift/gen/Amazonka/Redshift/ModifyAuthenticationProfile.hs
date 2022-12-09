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
-- Module      : Amazonka.Redshift.ModifyAuthenticationProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an authentication profile.
module Amazonka.Redshift.ModifyAuthenticationProfile
  ( -- * Creating a Request
    ModifyAuthenticationProfile (..),
    newModifyAuthenticationProfile,

    -- * Request Lenses
    modifyAuthenticationProfile_authenticationProfileName,
    modifyAuthenticationProfile_authenticationProfileContent,

    -- * Destructuring the Response
    ModifyAuthenticationProfileResponse (..),
    newModifyAuthenticationProfileResponse,

    -- * Response Lenses
    modifyAuthenticationProfileResponse_authenticationProfileContent,
    modifyAuthenticationProfileResponse_authenticationProfileName,
    modifyAuthenticationProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyAuthenticationProfile' smart constructor.
data ModifyAuthenticationProfile = ModifyAuthenticationProfile'
  { -- | The name of the authentication profile to replace.
    authenticationProfileName :: Prelude.Text,
    -- | The new content of the authentication profile in JSON format. The
    -- maximum length of the JSON string is determined by a quota for your
    -- account.
    authenticationProfileContent :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyAuthenticationProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationProfileName', 'modifyAuthenticationProfile_authenticationProfileName' - The name of the authentication profile to replace.
--
-- 'authenticationProfileContent', 'modifyAuthenticationProfile_authenticationProfileContent' - The new content of the authentication profile in JSON format. The
-- maximum length of the JSON string is determined by a quota for your
-- account.
newModifyAuthenticationProfile ::
  -- | 'authenticationProfileName'
  Prelude.Text ->
  -- | 'authenticationProfileContent'
  Prelude.Text ->
  ModifyAuthenticationProfile
newModifyAuthenticationProfile
  pAuthenticationProfileName_
  pAuthenticationProfileContent_ =
    ModifyAuthenticationProfile'
      { authenticationProfileName =
          pAuthenticationProfileName_,
        authenticationProfileContent =
          pAuthenticationProfileContent_
      }

-- | The name of the authentication profile to replace.
modifyAuthenticationProfile_authenticationProfileName :: Lens.Lens' ModifyAuthenticationProfile Prelude.Text
modifyAuthenticationProfile_authenticationProfileName = Lens.lens (\ModifyAuthenticationProfile' {authenticationProfileName} -> authenticationProfileName) (\s@ModifyAuthenticationProfile' {} a -> s {authenticationProfileName = a} :: ModifyAuthenticationProfile)

-- | The new content of the authentication profile in JSON format. The
-- maximum length of the JSON string is determined by a quota for your
-- account.
modifyAuthenticationProfile_authenticationProfileContent :: Lens.Lens' ModifyAuthenticationProfile Prelude.Text
modifyAuthenticationProfile_authenticationProfileContent = Lens.lens (\ModifyAuthenticationProfile' {authenticationProfileContent} -> authenticationProfileContent) (\s@ModifyAuthenticationProfile' {} a -> s {authenticationProfileContent = a} :: ModifyAuthenticationProfile)

instance Core.AWSRequest ModifyAuthenticationProfile where
  type
    AWSResponse ModifyAuthenticationProfile =
      ModifyAuthenticationProfileResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyAuthenticationProfileResult"
      ( \s h x ->
          ModifyAuthenticationProfileResponse'
            Prelude.<$> (x Data..@? "AuthenticationProfileContent")
            Prelude.<*> (x Data..@? "AuthenticationProfileName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyAuthenticationProfile where
  hashWithSalt _salt ModifyAuthenticationProfile' {..} =
    _salt
      `Prelude.hashWithSalt` authenticationProfileName
      `Prelude.hashWithSalt` authenticationProfileContent

instance Prelude.NFData ModifyAuthenticationProfile where
  rnf ModifyAuthenticationProfile' {..} =
    Prelude.rnf authenticationProfileName
      `Prelude.seq` Prelude.rnf authenticationProfileContent

instance Data.ToHeaders ModifyAuthenticationProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyAuthenticationProfile where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyAuthenticationProfile where
  toQuery ModifyAuthenticationProfile' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ModifyAuthenticationProfile" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "AuthenticationProfileName"
          Data.=: authenticationProfileName,
        "AuthenticationProfileContent"
          Data.=: authenticationProfileContent
      ]

-- | /See:/ 'newModifyAuthenticationProfileResponse' smart constructor.
data ModifyAuthenticationProfileResponse = ModifyAuthenticationProfileResponse'
  { -- | The updated content of the authentication profile in JSON format.
    authenticationProfileContent :: Prelude.Maybe Prelude.Text,
    -- | The name of the authentication profile that was replaced.
    authenticationProfileName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyAuthenticationProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationProfileContent', 'modifyAuthenticationProfileResponse_authenticationProfileContent' - The updated content of the authentication profile in JSON format.
--
-- 'authenticationProfileName', 'modifyAuthenticationProfileResponse_authenticationProfileName' - The name of the authentication profile that was replaced.
--
-- 'httpStatus', 'modifyAuthenticationProfileResponse_httpStatus' - The response's http status code.
newModifyAuthenticationProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyAuthenticationProfileResponse
newModifyAuthenticationProfileResponse pHttpStatus_ =
  ModifyAuthenticationProfileResponse'
    { authenticationProfileContent =
        Prelude.Nothing,
      authenticationProfileName =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated content of the authentication profile in JSON format.
modifyAuthenticationProfileResponse_authenticationProfileContent :: Lens.Lens' ModifyAuthenticationProfileResponse (Prelude.Maybe Prelude.Text)
modifyAuthenticationProfileResponse_authenticationProfileContent = Lens.lens (\ModifyAuthenticationProfileResponse' {authenticationProfileContent} -> authenticationProfileContent) (\s@ModifyAuthenticationProfileResponse' {} a -> s {authenticationProfileContent = a} :: ModifyAuthenticationProfileResponse)

-- | The name of the authentication profile that was replaced.
modifyAuthenticationProfileResponse_authenticationProfileName :: Lens.Lens' ModifyAuthenticationProfileResponse (Prelude.Maybe Prelude.Text)
modifyAuthenticationProfileResponse_authenticationProfileName = Lens.lens (\ModifyAuthenticationProfileResponse' {authenticationProfileName} -> authenticationProfileName) (\s@ModifyAuthenticationProfileResponse' {} a -> s {authenticationProfileName = a} :: ModifyAuthenticationProfileResponse)

-- | The response's http status code.
modifyAuthenticationProfileResponse_httpStatus :: Lens.Lens' ModifyAuthenticationProfileResponse Prelude.Int
modifyAuthenticationProfileResponse_httpStatus = Lens.lens (\ModifyAuthenticationProfileResponse' {httpStatus} -> httpStatus) (\s@ModifyAuthenticationProfileResponse' {} a -> s {httpStatus = a} :: ModifyAuthenticationProfileResponse)

instance
  Prelude.NFData
    ModifyAuthenticationProfileResponse
  where
  rnf ModifyAuthenticationProfileResponse' {..} =
    Prelude.rnf authenticationProfileContent
      `Prelude.seq` Prelude.rnf authenticationProfileName
      `Prelude.seq` Prelude.rnf httpStatus
