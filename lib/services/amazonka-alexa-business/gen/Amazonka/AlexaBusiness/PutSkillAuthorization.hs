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
-- Module      : Amazonka.AlexaBusiness.PutSkillAuthorization
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Links a user\'s account to a third-party skill provider. If this API
-- operation is called by an assumed IAM role, the skill being linked must
-- be a private skill. Also, the skill must be owned by the AWS account
-- that assumed the IAM role.
module Amazonka.AlexaBusiness.PutSkillAuthorization
  ( -- * Creating a Request
    PutSkillAuthorization (..),
    newPutSkillAuthorization,

    -- * Request Lenses
    putSkillAuthorization_roomArn,
    putSkillAuthorization_authorizationResult,
    putSkillAuthorization_skillId,

    -- * Destructuring the Response
    PutSkillAuthorizationResponse (..),
    newPutSkillAuthorizationResponse,

    -- * Response Lenses
    putSkillAuthorizationResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutSkillAuthorization' smart constructor.
data PutSkillAuthorization = PutSkillAuthorization'
  { -- | The room that the skill is authorized for.
    roomArn :: Prelude.Maybe Prelude.Text,
    -- | The authorization result specific to OAUTH code grant output. \"Code”
    -- must be populated in the AuthorizationResult map to establish the
    -- authorization.
    authorizationResult :: Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The unique identifier of a skill.
    skillId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutSkillAuthorization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roomArn', 'putSkillAuthorization_roomArn' - The room that the skill is authorized for.
--
-- 'authorizationResult', 'putSkillAuthorization_authorizationResult' - The authorization result specific to OAUTH code grant output. \"Code”
-- must be populated in the AuthorizationResult map to establish the
-- authorization.
--
-- 'skillId', 'putSkillAuthorization_skillId' - The unique identifier of a skill.
newPutSkillAuthorization ::
  -- | 'skillId'
  Prelude.Text ->
  PutSkillAuthorization
newPutSkillAuthorization pSkillId_ =
  PutSkillAuthorization'
    { roomArn = Prelude.Nothing,
      authorizationResult = Prelude.mempty,
      skillId = pSkillId_
    }

-- | The room that the skill is authorized for.
putSkillAuthorization_roomArn :: Lens.Lens' PutSkillAuthorization (Prelude.Maybe Prelude.Text)
putSkillAuthorization_roomArn = Lens.lens (\PutSkillAuthorization' {roomArn} -> roomArn) (\s@PutSkillAuthorization' {} a -> s {roomArn = a} :: PutSkillAuthorization)

-- | The authorization result specific to OAUTH code grant output. \"Code”
-- must be populated in the AuthorizationResult map to establish the
-- authorization.
putSkillAuthorization_authorizationResult :: Lens.Lens' PutSkillAuthorization (Prelude.HashMap Prelude.Text Prelude.Text)
putSkillAuthorization_authorizationResult = Lens.lens (\PutSkillAuthorization' {authorizationResult} -> authorizationResult) (\s@PutSkillAuthorization' {} a -> s {authorizationResult = a} :: PutSkillAuthorization) Prelude.. Data._Sensitive Prelude.. Lens.coerced

-- | The unique identifier of a skill.
putSkillAuthorization_skillId :: Lens.Lens' PutSkillAuthorization Prelude.Text
putSkillAuthorization_skillId = Lens.lens (\PutSkillAuthorization' {skillId} -> skillId) (\s@PutSkillAuthorization' {} a -> s {skillId = a} :: PutSkillAuthorization)

instance Core.AWSRequest PutSkillAuthorization where
  type
    AWSResponse PutSkillAuthorization =
      PutSkillAuthorizationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutSkillAuthorizationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutSkillAuthorization where
  hashWithSalt _salt PutSkillAuthorization' {..} =
    _salt
      `Prelude.hashWithSalt` roomArn
      `Prelude.hashWithSalt` authorizationResult
      `Prelude.hashWithSalt` skillId

instance Prelude.NFData PutSkillAuthorization where
  rnf PutSkillAuthorization' {..} =
    Prelude.rnf roomArn `Prelude.seq`
      Prelude.rnf authorizationResult `Prelude.seq`
        Prelude.rnf skillId

instance Data.ToHeaders PutSkillAuthorization where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.PutSkillAuthorization" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutSkillAuthorization where
  toJSON PutSkillAuthorization' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RoomArn" Data..=) Prelude.<$> roomArn,
            Prelude.Just
              ("AuthorizationResult" Data..= authorizationResult),
            Prelude.Just ("SkillId" Data..= skillId)
          ]
      )

instance Data.ToPath PutSkillAuthorization where
  toPath = Prelude.const "/"

instance Data.ToQuery PutSkillAuthorization where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutSkillAuthorizationResponse' smart constructor.
data PutSkillAuthorizationResponse = PutSkillAuthorizationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutSkillAuthorizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putSkillAuthorizationResponse_httpStatus' - The response's http status code.
newPutSkillAuthorizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutSkillAuthorizationResponse
newPutSkillAuthorizationResponse pHttpStatus_ =
  PutSkillAuthorizationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putSkillAuthorizationResponse_httpStatus :: Lens.Lens' PutSkillAuthorizationResponse Prelude.Int
putSkillAuthorizationResponse_httpStatus = Lens.lens (\PutSkillAuthorizationResponse' {httpStatus} -> httpStatus) (\s@PutSkillAuthorizationResponse' {} a -> s {httpStatus = a} :: PutSkillAuthorizationResponse)

instance Prelude.NFData PutSkillAuthorizationResponse where
  rnf PutSkillAuthorizationResponse' {..} =
    Prelude.rnf httpStatus
