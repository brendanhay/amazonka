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
-- Module      : Amazonka.Redshift.CreateAuthenticationProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an authentication profile with the specified parameters.
module Amazonka.Redshift.CreateAuthenticationProfile
  ( -- * Creating a Request
    CreateAuthenticationProfile (..),
    newCreateAuthenticationProfile,

    -- * Request Lenses
    createAuthenticationProfile_authenticationProfileName,
    createAuthenticationProfile_authenticationProfileContent,

    -- * Destructuring the Response
    CreateAuthenticationProfileResponse (..),
    newCreateAuthenticationProfileResponse,

    -- * Response Lenses
    createAuthenticationProfileResponse_authenticationProfileName,
    createAuthenticationProfileResponse_authenticationProfileContent,
    createAuthenticationProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAuthenticationProfile' smart constructor.
data CreateAuthenticationProfile = CreateAuthenticationProfile'
  { -- | The name of the authentication profile to be created.
    authenticationProfileName :: Prelude.Text,
    -- | The content of the authentication profile in JSON format. The maximum
    -- length of the JSON string is determined by a quota for your account.
    authenticationProfileContent :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAuthenticationProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationProfileName', 'createAuthenticationProfile_authenticationProfileName' - The name of the authentication profile to be created.
--
-- 'authenticationProfileContent', 'createAuthenticationProfile_authenticationProfileContent' - The content of the authentication profile in JSON format. The maximum
-- length of the JSON string is determined by a quota for your account.
newCreateAuthenticationProfile ::
  -- | 'authenticationProfileName'
  Prelude.Text ->
  -- | 'authenticationProfileContent'
  Prelude.Text ->
  CreateAuthenticationProfile
newCreateAuthenticationProfile
  pAuthenticationProfileName_
  pAuthenticationProfileContent_ =
    CreateAuthenticationProfile'
      { authenticationProfileName =
          pAuthenticationProfileName_,
        authenticationProfileContent =
          pAuthenticationProfileContent_
      }

-- | The name of the authentication profile to be created.
createAuthenticationProfile_authenticationProfileName :: Lens.Lens' CreateAuthenticationProfile Prelude.Text
createAuthenticationProfile_authenticationProfileName = Lens.lens (\CreateAuthenticationProfile' {authenticationProfileName} -> authenticationProfileName) (\s@CreateAuthenticationProfile' {} a -> s {authenticationProfileName = a} :: CreateAuthenticationProfile)

-- | The content of the authentication profile in JSON format. The maximum
-- length of the JSON string is determined by a quota for your account.
createAuthenticationProfile_authenticationProfileContent :: Lens.Lens' CreateAuthenticationProfile Prelude.Text
createAuthenticationProfile_authenticationProfileContent = Lens.lens (\CreateAuthenticationProfile' {authenticationProfileContent} -> authenticationProfileContent) (\s@CreateAuthenticationProfile' {} a -> s {authenticationProfileContent = a} :: CreateAuthenticationProfile)

instance Core.AWSRequest CreateAuthenticationProfile where
  type
    AWSResponse CreateAuthenticationProfile =
      CreateAuthenticationProfileResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateAuthenticationProfileResult"
      ( \s h x ->
          CreateAuthenticationProfileResponse'
            Prelude.<$> (x Core..@? "AuthenticationProfileName")
            Prelude.<*> (x Core..@? "AuthenticationProfileContent")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAuthenticationProfile where
  hashWithSalt _salt CreateAuthenticationProfile' {..} =
    _salt
      `Prelude.hashWithSalt` authenticationProfileName
      `Prelude.hashWithSalt` authenticationProfileContent

instance Prelude.NFData CreateAuthenticationProfile where
  rnf CreateAuthenticationProfile' {..} =
    Prelude.rnf authenticationProfileName
      `Prelude.seq` Prelude.rnf authenticationProfileContent

instance Core.ToHeaders CreateAuthenticationProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateAuthenticationProfile where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateAuthenticationProfile where
  toQuery CreateAuthenticationProfile' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "CreateAuthenticationProfile" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "AuthenticationProfileName"
          Core.=: authenticationProfileName,
        "AuthenticationProfileContent"
          Core.=: authenticationProfileContent
      ]

-- | /See:/ 'newCreateAuthenticationProfileResponse' smart constructor.
data CreateAuthenticationProfileResponse = CreateAuthenticationProfileResponse'
  { -- | The name of the authentication profile that was created.
    authenticationProfileName :: Prelude.Maybe Prelude.Text,
    -- | The content of the authentication profile in JSON format.
    authenticationProfileContent :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAuthenticationProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationProfileName', 'createAuthenticationProfileResponse_authenticationProfileName' - The name of the authentication profile that was created.
--
-- 'authenticationProfileContent', 'createAuthenticationProfileResponse_authenticationProfileContent' - The content of the authentication profile in JSON format.
--
-- 'httpStatus', 'createAuthenticationProfileResponse_httpStatus' - The response's http status code.
newCreateAuthenticationProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAuthenticationProfileResponse
newCreateAuthenticationProfileResponse pHttpStatus_ =
  CreateAuthenticationProfileResponse'
    { authenticationProfileName =
        Prelude.Nothing,
      authenticationProfileContent =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the authentication profile that was created.
createAuthenticationProfileResponse_authenticationProfileName :: Lens.Lens' CreateAuthenticationProfileResponse (Prelude.Maybe Prelude.Text)
createAuthenticationProfileResponse_authenticationProfileName = Lens.lens (\CreateAuthenticationProfileResponse' {authenticationProfileName} -> authenticationProfileName) (\s@CreateAuthenticationProfileResponse' {} a -> s {authenticationProfileName = a} :: CreateAuthenticationProfileResponse)

-- | The content of the authentication profile in JSON format.
createAuthenticationProfileResponse_authenticationProfileContent :: Lens.Lens' CreateAuthenticationProfileResponse (Prelude.Maybe Prelude.Text)
createAuthenticationProfileResponse_authenticationProfileContent = Lens.lens (\CreateAuthenticationProfileResponse' {authenticationProfileContent} -> authenticationProfileContent) (\s@CreateAuthenticationProfileResponse' {} a -> s {authenticationProfileContent = a} :: CreateAuthenticationProfileResponse)

-- | The response's http status code.
createAuthenticationProfileResponse_httpStatus :: Lens.Lens' CreateAuthenticationProfileResponse Prelude.Int
createAuthenticationProfileResponse_httpStatus = Lens.lens (\CreateAuthenticationProfileResponse' {httpStatus} -> httpStatus) (\s@CreateAuthenticationProfileResponse' {} a -> s {httpStatus = a} :: CreateAuthenticationProfileResponse)

instance
  Prelude.NFData
    CreateAuthenticationProfileResponse
  where
  rnf CreateAuthenticationProfileResponse' {..} =
    Prelude.rnf authenticationProfileName
      `Prelude.seq` Prelude.rnf authenticationProfileContent
      `Prelude.seq` Prelude.rnf httpStatus
