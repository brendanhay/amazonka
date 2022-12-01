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
-- Module      : Amazonka.AlexaBusiness.UpdateSkillGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates skill group details by skill group ARN.
module Amazonka.AlexaBusiness.UpdateSkillGroup
  ( -- * Creating a Request
    UpdateSkillGroup (..),
    newUpdateSkillGroup,

    -- * Request Lenses
    updateSkillGroup_skillGroupArn,
    updateSkillGroup_description,
    updateSkillGroup_skillGroupName,

    -- * Destructuring the Response
    UpdateSkillGroupResponse (..),
    newUpdateSkillGroupResponse,

    -- * Response Lenses
    updateSkillGroupResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSkillGroup' smart constructor.
data UpdateSkillGroup = UpdateSkillGroup'
  { -- | The ARN of the skill group to update.
    skillGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The updated description for the skill group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The updated name for the skill group.
    skillGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSkillGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skillGroupArn', 'updateSkillGroup_skillGroupArn' - The ARN of the skill group to update.
--
-- 'description', 'updateSkillGroup_description' - The updated description for the skill group.
--
-- 'skillGroupName', 'updateSkillGroup_skillGroupName' - The updated name for the skill group.
newUpdateSkillGroup ::
  UpdateSkillGroup
newUpdateSkillGroup =
  UpdateSkillGroup'
    { skillGroupArn = Prelude.Nothing,
      description = Prelude.Nothing,
      skillGroupName = Prelude.Nothing
    }

-- | The ARN of the skill group to update.
updateSkillGroup_skillGroupArn :: Lens.Lens' UpdateSkillGroup (Prelude.Maybe Prelude.Text)
updateSkillGroup_skillGroupArn = Lens.lens (\UpdateSkillGroup' {skillGroupArn} -> skillGroupArn) (\s@UpdateSkillGroup' {} a -> s {skillGroupArn = a} :: UpdateSkillGroup)

-- | The updated description for the skill group.
updateSkillGroup_description :: Lens.Lens' UpdateSkillGroup (Prelude.Maybe Prelude.Text)
updateSkillGroup_description = Lens.lens (\UpdateSkillGroup' {description} -> description) (\s@UpdateSkillGroup' {} a -> s {description = a} :: UpdateSkillGroup)

-- | The updated name for the skill group.
updateSkillGroup_skillGroupName :: Lens.Lens' UpdateSkillGroup (Prelude.Maybe Prelude.Text)
updateSkillGroup_skillGroupName = Lens.lens (\UpdateSkillGroup' {skillGroupName} -> skillGroupName) (\s@UpdateSkillGroup' {} a -> s {skillGroupName = a} :: UpdateSkillGroup)

instance Core.AWSRequest UpdateSkillGroup where
  type
    AWSResponse UpdateSkillGroup =
      UpdateSkillGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateSkillGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSkillGroup where
  hashWithSalt _salt UpdateSkillGroup' {..} =
    _salt `Prelude.hashWithSalt` skillGroupArn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` skillGroupName

instance Prelude.NFData UpdateSkillGroup where
  rnf UpdateSkillGroup' {..} =
    Prelude.rnf skillGroupArn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf skillGroupName

instance Core.ToHeaders UpdateSkillGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.UpdateSkillGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateSkillGroup where
  toJSON UpdateSkillGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SkillGroupArn" Core..=) Prelude.<$> skillGroupArn,
            ("Description" Core..=) Prelude.<$> description,
            ("SkillGroupName" Core..=)
              Prelude.<$> skillGroupName
          ]
      )

instance Core.ToPath UpdateSkillGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateSkillGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSkillGroupResponse' smart constructor.
data UpdateSkillGroupResponse = UpdateSkillGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSkillGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateSkillGroupResponse_httpStatus' - The response's http status code.
newUpdateSkillGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSkillGroupResponse
newUpdateSkillGroupResponse pHttpStatus_ =
  UpdateSkillGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateSkillGroupResponse_httpStatus :: Lens.Lens' UpdateSkillGroupResponse Prelude.Int
updateSkillGroupResponse_httpStatus = Lens.lens (\UpdateSkillGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateSkillGroupResponse' {} a -> s {httpStatus = a} :: UpdateSkillGroupResponse)

instance Prelude.NFData UpdateSkillGroupResponse where
  rnf UpdateSkillGroupResponse' {..} =
    Prelude.rnf httpStatus
