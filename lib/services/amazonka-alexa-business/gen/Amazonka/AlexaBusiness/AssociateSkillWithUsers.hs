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
-- Module      : Amazonka.AlexaBusiness.AssociateSkillWithUsers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Makes a private skill available for enrolled users to enable on their
-- devices.
module Amazonka.AlexaBusiness.AssociateSkillWithUsers
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

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateSkillWithUsers' smart constructor.
data AssociateSkillWithUsers = AssociateSkillWithUsers'
  { -- | The private skill ID you want to make available to enrolled users.
    skillId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest AssociateSkillWithUsers where
  type
    AWSResponse AssociateSkillWithUsers =
      AssociateSkillWithUsersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateSkillWithUsersResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateSkillWithUsers where
  hashWithSalt _salt AssociateSkillWithUsers' {..} =
    _salt `Prelude.hashWithSalt` skillId

instance Prelude.NFData AssociateSkillWithUsers where
  rnf AssociateSkillWithUsers' {..} =
    Prelude.rnf skillId

instance Data.ToHeaders AssociateSkillWithUsers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.AssociateSkillWithUsers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateSkillWithUsers where
  toJSON AssociateSkillWithUsers' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("SkillId" Data..= skillId)]
      )

instance Data.ToPath AssociateSkillWithUsers where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateSkillWithUsers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateSkillWithUsersResponse' smart constructor.
data AssociateSkillWithUsersResponse = AssociateSkillWithUsersResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf AssociateSkillWithUsersResponse' {..} =
    Prelude.rnf httpStatus
