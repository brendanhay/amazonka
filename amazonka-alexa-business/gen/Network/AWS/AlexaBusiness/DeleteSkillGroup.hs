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
-- Module      : Network.AWS.AlexaBusiness.DeleteSkillGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a skill group by skill group ARN.
module Network.AWS.AlexaBusiness.DeleteSkillGroup
  ( -- * Creating a Request
    DeleteSkillGroup (..),
    newDeleteSkillGroup,

    -- * Request Lenses
    deleteSkillGroup_skillGroupArn,

    -- * Destructuring the Response
    DeleteSkillGroupResponse (..),
    newDeleteSkillGroupResponse,

    -- * Response Lenses
    deleteSkillGroupResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteSkillGroup' smart constructor.
data DeleteSkillGroup = DeleteSkillGroup'
  { -- | The ARN of the skill group to delete. Required.
    skillGroupArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteSkillGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skillGroupArn', 'deleteSkillGroup_skillGroupArn' - The ARN of the skill group to delete. Required.
newDeleteSkillGroup ::
  DeleteSkillGroup
newDeleteSkillGroup =
  DeleteSkillGroup' {skillGroupArn = Prelude.Nothing}

-- | The ARN of the skill group to delete. Required.
deleteSkillGroup_skillGroupArn :: Lens.Lens' DeleteSkillGroup (Prelude.Maybe Prelude.Text)
deleteSkillGroup_skillGroupArn = Lens.lens (\DeleteSkillGroup' {skillGroupArn} -> skillGroupArn) (\s@DeleteSkillGroup' {} a -> s {skillGroupArn = a} :: DeleteSkillGroup)

instance Prelude.AWSRequest DeleteSkillGroup where
  type Rs DeleteSkillGroup = DeleteSkillGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSkillGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSkillGroup

instance Prelude.NFData DeleteSkillGroup

instance Prelude.ToHeaders DeleteSkillGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.DeleteSkillGroup" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteSkillGroup where
  toJSON DeleteSkillGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SkillGroupArn" Prelude..=)
              Prelude.<$> skillGroupArn
          ]
      )

instance Prelude.ToPath DeleteSkillGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteSkillGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSkillGroupResponse' smart constructor.
data DeleteSkillGroupResponse = DeleteSkillGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteSkillGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSkillGroupResponse_httpStatus' - The response's http status code.
newDeleteSkillGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSkillGroupResponse
newDeleteSkillGroupResponse pHttpStatus_ =
  DeleteSkillGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteSkillGroupResponse_httpStatus :: Lens.Lens' DeleteSkillGroupResponse Prelude.Int
deleteSkillGroupResponse_httpStatus = Lens.lens (\DeleteSkillGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteSkillGroupResponse' {} a -> s {httpStatus = a} :: DeleteSkillGroupResponse)

instance Prelude.NFData DeleteSkillGroupResponse
