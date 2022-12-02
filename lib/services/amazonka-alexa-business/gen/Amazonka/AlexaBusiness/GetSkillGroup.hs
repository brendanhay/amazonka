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
-- Module      : Amazonka.AlexaBusiness.GetSkillGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets skill group details by skill group ARN.
module Amazonka.AlexaBusiness.GetSkillGroup
  ( -- * Creating a Request
    GetSkillGroup (..),
    newGetSkillGroup,

    -- * Request Lenses
    getSkillGroup_skillGroupArn,

    -- * Destructuring the Response
    GetSkillGroupResponse (..),
    newGetSkillGroupResponse,

    -- * Response Lenses
    getSkillGroupResponse_skillGroup,
    getSkillGroupResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSkillGroup' smart constructor.
data GetSkillGroup = GetSkillGroup'
  { -- | The ARN of the skill group for which to get details. Required.
    skillGroupArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSkillGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skillGroupArn', 'getSkillGroup_skillGroupArn' - The ARN of the skill group for which to get details. Required.
newGetSkillGroup ::
  GetSkillGroup
newGetSkillGroup =
  GetSkillGroup' {skillGroupArn = Prelude.Nothing}

-- | The ARN of the skill group for which to get details. Required.
getSkillGroup_skillGroupArn :: Lens.Lens' GetSkillGroup (Prelude.Maybe Prelude.Text)
getSkillGroup_skillGroupArn = Lens.lens (\GetSkillGroup' {skillGroupArn} -> skillGroupArn) (\s@GetSkillGroup' {} a -> s {skillGroupArn = a} :: GetSkillGroup)

instance Core.AWSRequest GetSkillGroup where
  type
    AWSResponse GetSkillGroup =
      GetSkillGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSkillGroupResponse'
            Prelude.<$> (x Data..?> "SkillGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSkillGroup where
  hashWithSalt _salt GetSkillGroup' {..} =
    _salt `Prelude.hashWithSalt` skillGroupArn

instance Prelude.NFData GetSkillGroup where
  rnf GetSkillGroup' {..} = Prelude.rnf skillGroupArn

instance Data.ToHeaders GetSkillGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.GetSkillGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetSkillGroup where
  toJSON GetSkillGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SkillGroupArn" Data..=)
              Prelude.<$> skillGroupArn
          ]
      )

instance Data.ToPath GetSkillGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery GetSkillGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSkillGroupResponse' smart constructor.
data GetSkillGroupResponse = GetSkillGroupResponse'
  { -- | The details of the skill group requested. Required.
    skillGroup :: Prelude.Maybe SkillGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSkillGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skillGroup', 'getSkillGroupResponse_skillGroup' - The details of the skill group requested. Required.
--
-- 'httpStatus', 'getSkillGroupResponse_httpStatus' - The response's http status code.
newGetSkillGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSkillGroupResponse
newGetSkillGroupResponse pHttpStatus_ =
  GetSkillGroupResponse'
    { skillGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the skill group requested. Required.
getSkillGroupResponse_skillGroup :: Lens.Lens' GetSkillGroupResponse (Prelude.Maybe SkillGroup)
getSkillGroupResponse_skillGroup = Lens.lens (\GetSkillGroupResponse' {skillGroup} -> skillGroup) (\s@GetSkillGroupResponse' {} a -> s {skillGroup = a} :: GetSkillGroupResponse)

-- | The response's http status code.
getSkillGroupResponse_httpStatus :: Lens.Lens' GetSkillGroupResponse Prelude.Int
getSkillGroupResponse_httpStatus = Lens.lens (\GetSkillGroupResponse' {httpStatus} -> httpStatus) (\s@GetSkillGroupResponse' {} a -> s {httpStatus = a} :: GetSkillGroupResponse)

instance Prelude.NFData GetSkillGroupResponse where
  rnf GetSkillGroupResponse' {..} =
    Prelude.rnf skillGroup
      `Prelude.seq` Prelude.rnf httpStatus
