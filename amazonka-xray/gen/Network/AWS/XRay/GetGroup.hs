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
-- Module      : Network.AWS.XRay.GetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves group resource details.
module Network.AWS.XRay.GetGroup
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.XRay.Types

-- | /See:/ 'newGetGroup' smart constructor.
data GetGroup = GetGroup'
  { -- | The case-sensitive name of the group.
    groupName :: Core.Maybe Core.Text,
    -- | The ARN of the group that was generated on creation.
    groupARN :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { groupName = Core.Nothing,
      groupARN = Core.Nothing
    }

-- | The case-sensitive name of the group.
getGroup_groupName :: Lens.Lens' GetGroup (Core.Maybe Core.Text)
getGroup_groupName = Lens.lens (\GetGroup' {groupName} -> groupName) (\s@GetGroup' {} a -> s {groupName = a} :: GetGroup)

-- | The ARN of the group that was generated on creation.
getGroup_groupARN :: Lens.Lens' GetGroup (Core.Maybe Core.Text)
getGroup_groupARN = Lens.lens (\GetGroup' {groupARN} -> groupARN) (\s@GetGroup' {} a -> s {groupARN = a} :: GetGroup)

instance Core.AWSRequest GetGroup where
  type AWSResponse GetGroup = GetGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGroupResponse'
            Core.<$> (x Core..?> "Group")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetGroup

instance Core.NFData GetGroup

instance Core.ToHeaders GetGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON GetGroup where
  toJSON GetGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("GroupName" Core..=) Core.<$> groupName,
            ("GroupARN" Core..=) Core.<$> groupARN
          ]
      )

instance Core.ToPath GetGroup where
  toPath = Core.const "/GetGroup"

instance Core.ToQuery GetGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetGroupResponse' smart constructor.
data GetGroupResponse = GetGroupResponse'
  { -- | The group that was requested. Contains the name of the group, the ARN of
    -- the group, the filter expression, and the insight configuration assigned
    -- to the group.
    group' :: Core.Maybe Group,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetGroupResponse
newGetGroupResponse pHttpStatus_ =
  GetGroupResponse'
    { group' = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The group that was requested. Contains the name of the group, the ARN of
-- the group, the filter expression, and the insight configuration assigned
-- to the group.
getGroupResponse_group :: Lens.Lens' GetGroupResponse (Core.Maybe Group)
getGroupResponse_group = Lens.lens (\GetGroupResponse' {group'} -> group') (\s@GetGroupResponse' {} a -> s {group' = a} :: GetGroupResponse)

-- | The response's http status code.
getGroupResponse_httpStatus :: Lens.Lens' GetGroupResponse Core.Int
getGroupResponse_httpStatus = Lens.lens (\GetGroupResponse' {httpStatus} -> httpStatus) (\s@GetGroupResponse' {} a -> s {httpStatus = a} :: GetGroupResponse)

instance Core.NFData GetGroupResponse
