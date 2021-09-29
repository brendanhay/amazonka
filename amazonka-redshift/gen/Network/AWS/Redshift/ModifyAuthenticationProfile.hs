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
-- Module      : Network.AWS.Redshift.ModifyAuthenticationProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an authentication profile.
module Network.AWS.Redshift.ModifyAuthenticationProfile
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
    modifyAuthenticationProfileResponse_authenticationProfileName,
    modifyAuthenticationProfileResponse_authenticationProfileContent,
    modifyAuthenticationProfileResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyAuthenticationProfileResult"
      ( \s h x ->
          ModifyAuthenticationProfileResponse'
            Prelude.<$> (x Core..@? "AuthenticationProfileName")
            Prelude.<*> (x Core..@? "AuthenticationProfileContent")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyAuthenticationProfile

instance Prelude.NFData ModifyAuthenticationProfile

instance Core.ToHeaders ModifyAuthenticationProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyAuthenticationProfile where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyAuthenticationProfile where
  toQuery ModifyAuthenticationProfile' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "ModifyAuthenticationProfile" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "AuthenticationProfileName"
          Core.=: authenticationProfileName,
        "AuthenticationProfileContent"
          Core.=: authenticationProfileContent
      ]

-- | /See:/ 'newModifyAuthenticationProfileResponse' smart constructor.
data ModifyAuthenticationProfileResponse = ModifyAuthenticationProfileResponse'
  { -- | The name of the authentication profile that was replaced.
    authenticationProfileName :: Prelude.Maybe Prelude.Text,
    -- | The updated content of the authentication profile in JSON format.
    authenticationProfileContent :: Prelude.Maybe Prelude.Text,
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
-- 'authenticationProfileName', 'modifyAuthenticationProfileResponse_authenticationProfileName' - The name of the authentication profile that was replaced.
--
-- 'authenticationProfileContent', 'modifyAuthenticationProfileResponse_authenticationProfileContent' - The updated content of the authentication profile in JSON format.
--
-- 'httpStatus', 'modifyAuthenticationProfileResponse_httpStatus' - The response's http status code.
newModifyAuthenticationProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyAuthenticationProfileResponse
newModifyAuthenticationProfileResponse pHttpStatus_ =
  ModifyAuthenticationProfileResponse'
    { authenticationProfileName =
        Prelude.Nothing,
      authenticationProfileContent =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the authentication profile that was replaced.
modifyAuthenticationProfileResponse_authenticationProfileName :: Lens.Lens' ModifyAuthenticationProfileResponse (Prelude.Maybe Prelude.Text)
modifyAuthenticationProfileResponse_authenticationProfileName = Lens.lens (\ModifyAuthenticationProfileResponse' {authenticationProfileName} -> authenticationProfileName) (\s@ModifyAuthenticationProfileResponse' {} a -> s {authenticationProfileName = a} :: ModifyAuthenticationProfileResponse)

-- | The updated content of the authentication profile in JSON format.
modifyAuthenticationProfileResponse_authenticationProfileContent :: Lens.Lens' ModifyAuthenticationProfileResponse (Prelude.Maybe Prelude.Text)
modifyAuthenticationProfileResponse_authenticationProfileContent = Lens.lens (\ModifyAuthenticationProfileResponse' {authenticationProfileContent} -> authenticationProfileContent) (\s@ModifyAuthenticationProfileResponse' {} a -> s {authenticationProfileContent = a} :: ModifyAuthenticationProfileResponse)

-- | The response's http status code.
modifyAuthenticationProfileResponse_httpStatus :: Lens.Lens' ModifyAuthenticationProfileResponse Prelude.Int
modifyAuthenticationProfileResponse_httpStatus = Lens.lens (\ModifyAuthenticationProfileResponse' {httpStatus} -> httpStatus) (\s@ModifyAuthenticationProfileResponse' {} a -> s {httpStatus = a} :: ModifyAuthenticationProfileResponse)

instance
  Prelude.NFData
    ModifyAuthenticationProfileResponse
