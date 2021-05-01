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
-- Module      : Network.AWS.AlexaBusiness.DeleteSkillAuthorization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unlinks a third-party account from a skill.
module Network.AWS.AlexaBusiness.DeleteSkillAuthorization
  ( -- * Creating a Request
    DeleteSkillAuthorization (..),
    newDeleteSkillAuthorization,

    -- * Request Lenses
    deleteSkillAuthorization_roomArn,
    deleteSkillAuthorization_skillId,

    -- * Destructuring the Response
    DeleteSkillAuthorizationResponse (..),
    newDeleteSkillAuthorizationResponse,

    -- * Response Lenses
    deleteSkillAuthorizationResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteSkillAuthorization' smart constructor.
data DeleteSkillAuthorization = DeleteSkillAuthorization'
  { -- | The room that the skill is authorized for.
    roomArn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of a skill.
    skillId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteSkillAuthorization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roomArn', 'deleteSkillAuthorization_roomArn' - The room that the skill is authorized for.
--
-- 'skillId', 'deleteSkillAuthorization_skillId' - The unique identifier of a skill.
newDeleteSkillAuthorization ::
  -- | 'skillId'
  Prelude.Text ->
  DeleteSkillAuthorization
newDeleteSkillAuthorization pSkillId_ =
  DeleteSkillAuthorization'
    { roomArn =
        Prelude.Nothing,
      skillId = pSkillId_
    }

-- | The room that the skill is authorized for.
deleteSkillAuthorization_roomArn :: Lens.Lens' DeleteSkillAuthorization (Prelude.Maybe Prelude.Text)
deleteSkillAuthorization_roomArn = Lens.lens (\DeleteSkillAuthorization' {roomArn} -> roomArn) (\s@DeleteSkillAuthorization' {} a -> s {roomArn = a} :: DeleteSkillAuthorization)

-- | The unique identifier of a skill.
deleteSkillAuthorization_skillId :: Lens.Lens' DeleteSkillAuthorization Prelude.Text
deleteSkillAuthorization_skillId = Lens.lens (\DeleteSkillAuthorization' {skillId} -> skillId) (\s@DeleteSkillAuthorization' {} a -> s {skillId = a} :: DeleteSkillAuthorization)

instance Prelude.AWSRequest DeleteSkillAuthorization where
  type
    Rs DeleteSkillAuthorization =
      DeleteSkillAuthorizationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSkillAuthorizationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSkillAuthorization

instance Prelude.NFData DeleteSkillAuthorization

instance Prelude.ToHeaders DeleteSkillAuthorization where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.DeleteSkillAuthorization" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteSkillAuthorization where
  toJSON DeleteSkillAuthorization' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("RoomArn" Prelude..=) Prelude.<$> roomArn,
            Prelude.Just ("SkillId" Prelude..= skillId)
          ]
      )

instance Prelude.ToPath DeleteSkillAuthorization where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteSkillAuthorization where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSkillAuthorizationResponse' smart constructor.
data DeleteSkillAuthorizationResponse = DeleteSkillAuthorizationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteSkillAuthorizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSkillAuthorizationResponse_httpStatus' - The response's http status code.
newDeleteSkillAuthorizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSkillAuthorizationResponse
newDeleteSkillAuthorizationResponse pHttpStatus_ =
  DeleteSkillAuthorizationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteSkillAuthorizationResponse_httpStatus :: Lens.Lens' DeleteSkillAuthorizationResponse Prelude.Int
deleteSkillAuthorizationResponse_httpStatus = Lens.lens (\DeleteSkillAuthorizationResponse' {httpStatus} -> httpStatus) (\s@DeleteSkillAuthorizationResponse' {} a -> s {httpStatus = a} :: DeleteSkillAuthorizationResponse)

instance
  Prelude.NFData
    DeleteSkillAuthorizationResponse
