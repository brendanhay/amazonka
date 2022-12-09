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
-- Module      : Amazonka.ResourceGroups.GetGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specified resource group.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
-- -   @resource-groups:GetGroup@
module Amazonka.ResourceGroups.GetGroup
  ( -- * Creating a Request
    GetGroup (..),
    newGetGroup,

    -- * Request Lenses
    getGroup_group,
    getGroup_groupName,

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
import Amazonka.ResourceGroups.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetGroup' smart constructor.
data GetGroup = GetGroup'
  { -- | The name or the ARN of the resource group to retrieve.
    group' :: Prelude.Maybe Prelude.Text,
    -- | Deprecated - don\'t use this parameter. Use @Group@ instead.
    groupName :: Prelude.Maybe Prelude.Text
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
-- 'group'', 'getGroup_group' - The name or the ARN of the resource group to retrieve.
--
-- 'groupName', 'getGroup_groupName' - Deprecated - don\'t use this parameter. Use @Group@ instead.
newGetGroup ::
  GetGroup
newGetGroup =
  GetGroup'
    { group' = Prelude.Nothing,
      groupName = Prelude.Nothing
    }

-- | The name or the ARN of the resource group to retrieve.
getGroup_group :: Lens.Lens' GetGroup (Prelude.Maybe Prelude.Text)
getGroup_group = Lens.lens (\GetGroup' {group'} -> group') (\s@GetGroup' {} a -> s {group' = a} :: GetGroup)

-- | Deprecated - don\'t use this parameter. Use @Group@ instead.
getGroup_groupName :: Lens.Lens' GetGroup (Prelude.Maybe Prelude.Text)
getGroup_groupName = Lens.lens (\GetGroup' {groupName} -> groupName) (\s@GetGroup' {} a -> s {groupName = a} :: GetGroup)

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
    _salt `Prelude.hashWithSalt` group'
      `Prelude.hashWithSalt` groupName

instance Prelude.NFData GetGroup where
  rnf GetGroup' {..} =
    Prelude.rnf group'
      `Prelude.seq` Prelude.rnf groupName

instance Data.ToHeaders GetGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON GetGroup where
  toJSON GetGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Group" Data..=) Prelude.<$> group',
            ("GroupName" Data..=) Prelude.<$> groupName
          ]
      )

instance Data.ToPath GetGroup where
  toPath = Prelude.const "/get-group"

instance Data.ToQuery GetGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetGroupResponse' smart constructor.
data GetGroupResponse = GetGroupResponse'
  { -- | A full description of the resource group.
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
-- 'group'', 'getGroupResponse_group' - A full description of the resource group.
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

-- | A full description of the resource group.
getGroupResponse_group :: Lens.Lens' GetGroupResponse (Prelude.Maybe Group)
getGroupResponse_group = Lens.lens (\GetGroupResponse' {group'} -> group') (\s@GetGroupResponse' {} a -> s {group' = a} :: GetGroupResponse)

-- | The response's http status code.
getGroupResponse_httpStatus :: Lens.Lens' GetGroupResponse Prelude.Int
getGroupResponse_httpStatus = Lens.lens (\GetGroupResponse' {httpStatus} -> httpStatus) (\s@GetGroupResponse' {} a -> s {httpStatus = a} :: GetGroupResponse)

instance Prelude.NFData GetGroupResponse where
  rnf GetGroupResponse' {..} =
    Prelude.rnf group'
      `Prelude.seq` Prelude.rnf httpStatus
