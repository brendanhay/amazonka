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
-- Module      : Amazonka.WorkMail.GetImpersonationRoleEffect
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests whether the given impersonation role can impersonate a target
-- user.
module Amazonka.WorkMail.GetImpersonationRoleEffect
  ( -- * Creating a Request
    GetImpersonationRoleEffect (..),
    newGetImpersonationRoleEffect,

    -- * Request Lenses
    getImpersonationRoleEffect_organizationId,
    getImpersonationRoleEffect_impersonationRoleId,
    getImpersonationRoleEffect_targetUser,

    -- * Destructuring the Response
    GetImpersonationRoleEffectResponse (..),
    newGetImpersonationRoleEffectResponse,

    -- * Response Lenses
    getImpersonationRoleEffectResponse_effect,
    getImpersonationRoleEffectResponse_matchedRules,
    getImpersonationRoleEffectResponse_type,
    getImpersonationRoleEffectResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newGetImpersonationRoleEffect' smart constructor.
data GetImpersonationRoleEffect = GetImpersonationRoleEffect'
  { -- | The WorkMail organization where the impersonation role is defined.
    organizationId :: Prelude.Text,
    -- | The impersonation role ID to test.
    impersonationRoleId :: Prelude.Text,
    -- | The WorkMail organization user chosen to test the impersonation role.
    -- The following identity formats are available:
    --
    -- -   User ID: @12345678-1234-1234-1234-123456789012@ or
    --     @S-1-1-12-1234567890-123456789-123456789-1234@
    --
    -- -   Email address: @user\@domain.tld@
    --
    -- -   User name: @user@
    targetUser :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetImpersonationRoleEffect' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'getImpersonationRoleEffect_organizationId' - The WorkMail organization where the impersonation role is defined.
--
-- 'impersonationRoleId', 'getImpersonationRoleEffect_impersonationRoleId' - The impersonation role ID to test.
--
-- 'targetUser', 'getImpersonationRoleEffect_targetUser' - The WorkMail organization user chosen to test the impersonation role.
-- The following identity formats are available:
--
-- -   User ID: @12345678-1234-1234-1234-123456789012@ or
--     @S-1-1-12-1234567890-123456789-123456789-1234@
--
-- -   Email address: @user\@domain.tld@
--
-- -   User name: @user@
newGetImpersonationRoleEffect ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'impersonationRoleId'
  Prelude.Text ->
  -- | 'targetUser'
  Prelude.Text ->
  GetImpersonationRoleEffect
newGetImpersonationRoleEffect
  pOrganizationId_
  pImpersonationRoleId_
  pTargetUser_ =
    GetImpersonationRoleEffect'
      { organizationId =
          pOrganizationId_,
        impersonationRoleId = pImpersonationRoleId_,
        targetUser = pTargetUser_
      }

-- | The WorkMail organization where the impersonation role is defined.
getImpersonationRoleEffect_organizationId :: Lens.Lens' GetImpersonationRoleEffect Prelude.Text
getImpersonationRoleEffect_organizationId = Lens.lens (\GetImpersonationRoleEffect' {organizationId} -> organizationId) (\s@GetImpersonationRoleEffect' {} a -> s {organizationId = a} :: GetImpersonationRoleEffect)

-- | The impersonation role ID to test.
getImpersonationRoleEffect_impersonationRoleId :: Lens.Lens' GetImpersonationRoleEffect Prelude.Text
getImpersonationRoleEffect_impersonationRoleId = Lens.lens (\GetImpersonationRoleEffect' {impersonationRoleId} -> impersonationRoleId) (\s@GetImpersonationRoleEffect' {} a -> s {impersonationRoleId = a} :: GetImpersonationRoleEffect)

-- | The WorkMail organization user chosen to test the impersonation role.
-- The following identity formats are available:
--
-- -   User ID: @12345678-1234-1234-1234-123456789012@ or
--     @S-1-1-12-1234567890-123456789-123456789-1234@
--
-- -   Email address: @user\@domain.tld@
--
-- -   User name: @user@
getImpersonationRoleEffect_targetUser :: Lens.Lens' GetImpersonationRoleEffect Prelude.Text
getImpersonationRoleEffect_targetUser = Lens.lens (\GetImpersonationRoleEffect' {targetUser} -> targetUser) (\s@GetImpersonationRoleEffect' {} a -> s {targetUser = a} :: GetImpersonationRoleEffect)

instance Core.AWSRequest GetImpersonationRoleEffect where
  type
    AWSResponse GetImpersonationRoleEffect =
      GetImpersonationRoleEffectResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetImpersonationRoleEffectResponse'
            Prelude.<$> (x Data..?> "Effect")
            Prelude.<*> (x Data..?> "MatchedRules" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetImpersonationRoleEffect where
  hashWithSalt _salt GetImpersonationRoleEffect' {..} =
    _salt
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` impersonationRoleId
      `Prelude.hashWithSalt` targetUser

instance Prelude.NFData GetImpersonationRoleEffect where
  rnf GetImpersonationRoleEffect' {..} =
    Prelude.rnf organizationId `Prelude.seq`
      Prelude.rnf impersonationRoleId `Prelude.seq`
        Prelude.rnf targetUser

instance Data.ToHeaders GetImpersonationRoleEffect where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.GetImpersonationRoleEffect" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetImpersonationRoleEffect where
  toJSON GetImpersonationRoleEffect' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just
              ("ImpersonationRoleId" Data..= impersonationRoleId),
            Prelude.Just ("TargetUser" Data..= targetUser)
          ]
      )

instance Data.ToPath GetImpersonationRoleEffect where
  toPath = Prelude.const "/"

instance Data.ToQuery GetImpersonationRoleEffect where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetImpersonationRoleEffectResponse' smart constructor.
data GetImpersonationRoleEffectResponse = GetImpersonationRoleEffectResponse'
  { -- | Effect of the impersonation role on the target user based on its rules.
    -- Available effects are @ALLOW@ or @DENY@.
    effect :: Prelude.Maybe AccessEffect,
    -- | A list of the rules that match the input and produce the configured
    -- effect.
    matchedRules :: Prelude.Maybe [ImpersonationMatchedRule],
    -- | The impersonation role type.
    type' :: Prelude.Maybe ImpersonationRoleType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetImpersonationRoleEffectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'effect', 'getImpersonationRoleEffectResponse_effect' - Effect of the impersonation role on the target user based on its rules.
-- Available effects are @ALLOW@ or @DENY@.
--
-- 'matchedRules', 'getImpersonationRoleEffectResponse_matchedRules' - A list of the rules that match the input and produce the configured
-- effect.
--
-- 'type'', 'getImpersonationRoleEffectResponse_type' - The impersonation role type.
--
-- 'httpStatus', 'getImpersonationRoleEffectResponse_httpStatus' - The response's http status code.
newGetImpersonationRoleEffectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetImpersonationRoleEffectResponse
newGetImpersonationRoleEffectResponse pHttpStatus_ =
  GetImpersonationRoleEffectResponse'
    { effect =
        Prelude.Nothing,
      matchedRules = Prelude.Nothing,
      type' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Effect of the impersonation role on the target user based on its rules.
-- Available effects are @ALLOW@ or @DENY@.
getImpersonationRoleEffectResponse_effect :: Lens.Lens' GetImpersonationRoleEffectResponse (Prelude.Maybe AccessEffect)
getImpersonationRoleEffectResponse_effect = Lens.lens (\GetImpersonationRoleEffectResponse' {effect} -> effect) (\s@GetImpersonationRoleEffectResponse' {} a -> s {effect = a} :: GetImpersonationRoleEffectResponse)

-- | A list of the rules that match the input and produce the configured
-- effect.
getImpersonationRoleEffectResponse_matchedRules :: Lens.Lens' GetImpersonationRoleEffectResponse (Prelude.Maybe [ImpersonationMatchedRule])
getImpersonationRoleEffectResponse_matchedRules = Lens.lens (\GetImpersonationRoleEffectResponse' {matchedRules} -> matchedRules) (\s@GetImpersonationRoleEffectResponse' {} a -> s {matchedRules = a} :: GetImpersonationRoleEffectResponse) Prelude.. Lens.mapping Lens.coerced

-- | The impersonation role type.
getImpersonationRoleEffectResponse_type :: Lens.Lens' GetImpersonationRoleEffectResponse (Prelude.Maybe ImpersonationRoleType)
getImpersonationRoleEffectResponse_type = Lens.lens (\GetImpersonationRoleEffectResponse' {type'} -> type') (\s@GetImpersonationRoleEffectResponse' {} a -> s {type' = a} :: GetImpersonationRoleEffectResponse)

-- | The response's http status code.
getImpersonationRoleEffectResponse_httpStatus :: Lens.Lens' GetImpersonationRoleEffectResponse Prelude.Int
getImpersonationRoleEffectResponse_httpStatus = Lens.lens (\GetImpersonationRoleEffectResponse' {httpStatus} -> httpStatus) (\s@GetImpersonationRoleEffectResponse' {} a -> s {httpStatus = a} :: GetImpersonationRoleEffectResponse)

instance
  Prelude.NFData
    GetImpersonationRoleEffectResponse
  where
  rnf GetImpersonationRoleEffectResponse' {..} =
    Prelude.rnf effect `Prelude.seq`
      Prelude.rnf matchedRules `Prelude.seq`
        Prelude.rnf type' `Prelude.seq`
          Prelude.rnf httpStatus
