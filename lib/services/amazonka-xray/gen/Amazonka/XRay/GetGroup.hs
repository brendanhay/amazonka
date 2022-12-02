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
-- Module      : Amazonka.XRay.GetGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves group resource details.
module Amazonka.XRay.GetGroup
  ( -- * Creating a Request
    GetGroup (..),
    newGetGroup,

    -- * Request Lenses
    getGroup_groupName,
    getGroup_groupARN,

    -- * Destructuring the Response
    GetGroupResponse (..),
    newGetGroupResponse,

    -- * Response Lenses
    getGroupResponse_group,
    getGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.XRay.Types

-- | /See:/ 'newGetGroup' smart constructor.
data GetGroup = GetGroup'
  { -- | The case-sensitive name of the group.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the group that was generated on creation.
    groupARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'getGroup_groupName' - The case-sensitive name of the group.
--
-- 'groupARN', 'getGroup_groupARN' - The ARN of the group that was generated on creation.
newGetGroup ::
  GetGroup
newGetGroup =
  GetGroup'
    { groupName = Prelude.Nothing,
      groupARN = Prelude.Nothing
    }

-- | The case-sensitive name of the group.
getGroup_groupName :: Lens.Lens' GetGroup (Prelude.Maybe Prelude.Text)
getGroup_groupName = Lens.lens (\GetGroup' {groupName} -> groupName) (\s@GetGroup' {} a -> s {groupName = a} :: GetGroup)

-- | The ARN of the group that was generated on creation.
getGroup_groupARN :: Lens.Lens' GetGroup (Prelude.Maybe Prelude.Text)
getGroup_groupARN = Lens.lens (\GetGroup' {groupARN} -> groupARN) (\s@GetGroup' {} a -> s {groupARN = a} :: GetGroup)

instance Core.AWSRequest GetGroup where
  type AWSResponse GetGroup = GetGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGroupResponse'
            Prelude.<$> (x Data..?> "Group")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetGroup where
  hashWithSalt _salt GetGroup' {..} =
    _salt `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` groupARN

instance Prelude.NFData GetGroup where
  rnf GetGroup' {..} =
    Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf groupARN

instance Data.ToHeaders GetGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON GetGroup where
  toJSON GetGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GroupName" Data..=) Prelude.<$> groupName,
            ("GroupARN" Data..=) Prelude.<$> groupARN
          ]
      )

instance Data.ToPath GetGroup where
  toPath = Prelude.const "/GetGroup"

instance Data.ToQuery GetGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetGroupResponse' smart constructor.
data GetGroupResponse = GetGroupResponse'
  { -- | The group that was requested. Contains the name of the group, the ARN of
    -- the group, the filter expression, and the insight configuration assigned
    -- to the group.
    group' :: Prelude.Maybe Group,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'group'', 'getGroupResponse_group' - The group that was requested. Contains the name of the group, the ARN of
-- the group, the filter expression, and the insight configuration assigned
-- to the group.
--
-- 'httpStatus', 'getGroupResponse_httpStatus' - The response's http status code.
newGetGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetGroupResponse
newGetGroupResponse pHttpStatus_ =
  GetGroupResponse'
    { group' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The group that was requested. Contains the name of the group, the ARN of
-- the group, the filter expression, and the insight configuration assigned
-- to the group.
getGroupResponse_group :: Lens.Lens' GetGroupResponse (Prelude.Maybe Group)
getGroupResponse_group = Lens.lens (\GetGroupResponse' {group'} -> group') (\s@GetGroupResponse' {} a -> s {group' = a} :: GetGroupResponse)

-- | The response's http status code.
getGroupResponse_httpStatus :: Lens.Lens' GetGroupResponse Prelude.Int
getGroupResponse_httpStatus = Lens.lens (\GetGroupResponse' {httpStatus} -> httpStatus) (\s@GetGroupResponse' {} a -> s {httpStatus = a} :: GetGroupResponse)

instance Prelude.NFData GetGroupResponse where
  rnf GetGroupResponse' {..} =
    Prelude.rnf group'
      `Prelude.seq` Prelude.rnf httpStatus
