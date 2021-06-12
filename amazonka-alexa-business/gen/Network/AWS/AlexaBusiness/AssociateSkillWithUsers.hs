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
-- Module      : Network.AWS.AlexaBusiness.AssociateSkillWithUsers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Makes a private skill available for enrolled users to enable on their
-- devices.
module Network.AWS.AlexaBusiness.AssociateSkillWithUsers
  ( -- * Creating a Request
    AssociateSkillWithUsers (..),
    newAssociateSkillWithUsers,

    -- * Request Lenses
    associateSkillWithUsers_skillId,

    -- * Destructuring the Response
    AssociateSkillWithUsersResponse (..),
    newAssociateSkillWithUsersResponse,

    -- * Response Lenses
    associateSkillWithUsersResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateSkillWithUsers' smart constructor.
data AssociateSkillWithUsers = AssociateSkillWithUsers'
  { -- | The private skill ID you want to make available to enrolled users.
    skillId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateSkillWithUsers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skillId', 'associateSkillWithUsers_skillId' - The private skill ID you want to make available to enrolled users.
newAssociateSkillWithUsers ::
  -- | 'skillId'
  Core.Text ->
  AssociateSkillWithUsers
newAssociateSkillWithUsers pSkillId_ =
  AssociateSkillWithUsers' {skillId = pSkillId_}

-- | The private skill ID you want to make available to enrolled users.
associateSkillWithUsers_skillId :: Lens.Lens' AssociateSkillWithUsers Core.Text
associateSkillWithUsers_skillId = Lens.lens (\AssociateSkillWithUsers' {skillId} -> skillId) (\s@AssociateSkillWithUsers' {} a -> s {skillId = a} :: AssociateSkillWithUsers)

instance Core.AWSRequest AssociateSkillWithUsers where
  type
    AWSResponse AssociateSkillWithUsers =
      AssociateSkillWithUsersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateSkillWithUsersResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AssociateSkillWithUsers

instance Core.NFData AssociateSkillWithUsers

instance Core.ToHeaders AssociateSkillWithUsers where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.AssociateSkillWithUsers" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AssociateSkillWithUsers where
  toJSON AssociateSkillWithUsers' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("SkillId" Core..= skillId)]
      )

instance Core.ToPath AssociateSkillWithUsers where
  toPath = Core.const "/"

instance Core.ToQuery AssociateSkillWithUsers where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAssociateSkillWithUsersResponse' smart constructor.
data AssociateSkillWithUsersResponse = AssociateSkillWithUsersResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateSkillWithUsersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateSkillWithUsersResponse_httpStatus' - The response's http status code.
newAssociateSkillWithUsersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AssociateSkillWithUsersResponse
newAssociateSkillWithUsersResponse pHttpStatus_ =
  AssociateSkillWithUsersResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateSkillWithUsersResponse_httpStatus :: Lens.Lens' AssociateSkillWithUsersResponse Core.Int
associateSkillWithUsersResponse_httpStatus = Lens.lens (\AssociateSkillWithUsersResponse' {httpStatus} -> httpStatus) (\s@AssociateSkillWithUsersResponse' {} a -> s {httpStatus = a} :: AssociateSkillWithUsersResponse)

instance Core.NFData AssociateSkillWithUsersResponse
