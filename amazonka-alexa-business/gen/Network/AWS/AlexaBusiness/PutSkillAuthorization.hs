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
-- Module      : Network.AWS.AlexaBusiness.PutSkillAuthorization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Links a user\'s account to a third-party skill provider. If this API
-- operation is called by an assumed IAM role, the skill being linked must
-- be a private skill. Also, the skill must be owned by the AWS account
-- that assumed the IAM role.
module Network.AWS.AlexaBusiness.PutSkillAuthorization
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

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutSkillAuthorization' smart constructor.
data PutSkillAuthorization = PutSkillAuthorization'
  { -- | The room that the skill is authorized for.
    roomArn :: Core.Maybe Core.Text,
    -- | The authorization result specific to OAUTH code grant output. \"Code”
    -- must be populated in the AuthorizationResult map to establish the
    -- authorization.
    authorizationResult :: Core.Sensitive (Core.HashMap Core.Text Core.Text),
    -- | The unique identifier of a skill.
    skillId :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Text ->
  PutSkillAuthorization
newPutSkillAuthorization pSkillId_ =
  PutSkillAuthorization'
    { roomArn = Core.Nothing,
      authorizationResult = Core.mempty,
      skillId = pSkillId_
    }

-- | The room that the skill is authorized for.
putSkillAuthorization_roomArn :: Lens.Lens' PutSkillAuthorization (Core.Maybe Core.Text)
putSkillAuthorization_roomArn = Lens.lens (\PutSkillAuthorization' {roomArn} -> roomArn) (\s@PutSkillAuthorization' {} a -> s {roomArn = a} :: PutSkillAuthorization)

-- | The authorization result specific to OAUTH code grant output. \"Code”
-- must be populated in the AuthorizationResult map to establish the
-- authorization.
putSkillAuthorization_authorizationResult :: Lens.Lens' PutSkillAuthorization (Core.HashMap Core.Text Core.Text)
putSkillAuthorization_authorizationResult = Lens.lens (\PutSkillAuthorization' {authorizationResult} -> authorizationResult) (\s@PutSkillAuthorization' {} a -> s {authorizationResult = a} :: PutSkillAuthorization) Core.. Core._Sensitive Core.. Lens._Coerce

-- | The unique identifier of a skill.
putSkillAuthorization_skillId :: Lens.Lens' PutSkillAuthorization Core.Text
putSkillAuthorization_skillId = Lens.lens (\PutSkillAuthorization' {skillId} -> skillId) (\s@PutSkillAuthorization' {} a -> s {skillId = a} :: PutSkillAuthorization)

instance Core.AWSRequest PutSkillAuthorization where
  type
    AWSResponse PutSkillAuthorization =
      PutSkillAuthorizationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutSkillAuthorizationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutSkillAuthorization

instance Core.NFData PutSkillAuthorization

instance Core.ToHeaders PutSkillAuthorization where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.PutSkillAuthorization" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutSkillAuthorization where
  toJSON PutSkillAuthorization' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RoomArn" Core..=) Core.<$> roomArn,
            Core.Just
              ("AuthorizationResult" Core..= authorizationResult),
            Core.Just ("SkillId" Core..= skillId)
          ]
      )

instance Core.ToPath PutSkillAuthorization where
  toPath = Core.const "/"

instance Core.ToQuery PutSkillAuthorization where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutSkillAuthorizationResponse' smart constructor.
data PutSkillAuthorizationResponse = PutSkillAuthorizationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  PutSkillAuthorizationResponse
newPutSkillAuthorizationResponse pHttpStatus_ =
  PutSkillAuthorizationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putSkillAuthorizationResponse_httpStatus :: Lens.Lens' PutSkillAuthorizationResponse Core.Int
putSkillAuthorizationResponse_httpStatus = Lens.lens (\PutSkillAuthorizationResponse' {httpStatus} -> httpStatus) (\s@PutSkillAuthorizationResponse' {} a -> s {httpStatus = a} :: PutSkillAuthorizationResponse)

instance Core.NFData PutSkillAuthorizationResponse
