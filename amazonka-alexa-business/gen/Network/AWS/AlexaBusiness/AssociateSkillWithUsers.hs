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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateSkillWithUsers' smart constructor.
data AssociateSkillWithUsers = AssociateSkillWithUsers'
  { -- | The private skill ID you want to make available to enrolled users.
    skillId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  AssociateSkillWithUsers
newAssociateSkillWithUsers pSkillId_ =
  AssociateSkillWithUsers' {skillId = pSkillId_}

-- | The private skill ID you want to make available to enrolled users.
associateSkillWithUsers_skillId :: Lens.Lens' AssociateSkillWithUsers Prelude.Text
associateSkillWithUsers_skillId = Lens.lens (\AssociateSkillWithUsers' {skillId} -> skillId) (\s@AssociateSkillWithUsers' {} a -> s {skillId = a} :: AssociateSkillWithUsers)

instance Prelude.AWSRequest AssociateSkillWithUsers where
  type
    Rs AssociateSkillWithUsers =
      AssociateSkillWithUsersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateSkillWithUsersResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateSkillWithUsers

instance Prelude.NFData AssociateSkillWithUsers

instance Prelude.ToHeaders AssociateSkillWithUsers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.AssociateSkillWithUsers" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AssociateSkillWithUsers where
  toJSON AssociateSkillWithUsers' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("SkillId" Prelude..= skillId)]
      )

instance Prelude.ToPath AssociateSkillWithUsers where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AssociateSkillWithUsers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateSkillWithUsersResponse' smart constructor.
data AssociateSkillWithUsersResponse = AssociateSkillWithUsersResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  AssociateSkillWithUsersResponse
newAssociateSkillWithUsersResponse pHttpStatus_ =
  AssociateSkillWithUsersResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateSkillWithUsersResponse_httpStatus :: Lens.Lens' AssociateSkillWithUsersResponse Prelude.Int
associateSkillWithUsersResponse_httpStatus = Lens.lens (\AssociateSkillWithUsersResponse' {httpStatus} -> httpStatus) (\s@AssociateSkillWithUsersResponse' {} a -> s {httpStatus = a} :: AssociateSkillWithUsersResponse)

instance
  Prelude.NFData
    AssociateSkillWithUsersResponse
