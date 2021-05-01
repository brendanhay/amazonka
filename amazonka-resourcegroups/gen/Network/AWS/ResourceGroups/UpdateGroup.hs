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
-- Module      : Network.AWS.ResourceGroups.UpdateGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the description for an existing group. You cannot update the
-- name of a resource group.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
-- -   @resource-groups:UpdateGroup@
module Network.AWS.ResourceGroups.UpdateGroup
  ( -- * Creating a Request
    UpdateGroup (..),
    newUpdateGroup,

    -- * Request Lenses
    updateGroup_groupName,
    updateGroup_group,
    updateGroup_description,

    -- * Destructuring the Response
    UpdateGroupResponse (..),
    newUpdateGroupResponse,

    -- * Response Lenses
    updateGroupResponse_group,
    updateGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import Network.AWS.ResourceGroups.Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateGroup' smart constructor.
data UpdateGroup = UpdateGroup'
  { -- | Don\'t use this parameter. Use @Group@ instead.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The name or the ARN of the resource group to modify.
    group' :: Prelude.Maybe Prelude.Text,
    -- | The new description that you want to update the resource group with.
    -- Descriptions can contain letters, numbers, hyphens, underscores,
    -- periods, and spaces.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'updateGroup_groupName' - Don\'t use this parameter. Use @Group@ instead.
--
-- 'group'', 'updateGroup_group' - The name or the ARN of the resource group to modify.
--
-- 'description', 'updateGroup_description' - The new description that you want to update the resource group with.
-- Descriptions can contain letters, numbers, hyphens, underscores,
-- periods, and spaces.
newUpdateGroup ::
  UpdateGroup
newUpdateGroup =
  UpdateGroup'
    { groupName = Prelude.Nothing,
      group' = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | Don\'t use this parameter. Use @Group@ instead.
updateGroup_groupName :: Lens.Lens' UpdateGroup (Prelude.Maybe Prelude.Text)
updateGroup_groupName = Lens.lens (\UpdateGroup' {groupName} -> groupName) (\s@UpdateGroup' {} a -> s {groupName = a} :: UpdateGroup)

-- | The name or the ARN of the resource group to modify.
updateGroup_group :: Lens.Lens' UpdateGroup (Prelude.Maybe Prelude.Text)
updateGroup_group = Lens.lens (\UpdateGroup' {group'} -> group') (\s@UpdateGroup' {} a -> s {group' = a} :: UpdateGroup)

-- | The new description that you want to update the resource group with.
-- Descriptions can contain letters, numbers, hyphens, underscores,
-- periods, and spaces.
updateGroup_description :: Lens.Lens' UpdateGroup (Prelude.Maybe Prelude.Text)
updateGroup_description = Lens.lens (\UpdateGroup' {description} -> description) (\s@UpdateGroup' {} a -> s {description = a} :: UpdateGroup)

instance Prelude.AWSRequest UpdateGroup where
  type Rs UpdateGroup = UpdateGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGroupResponse'
            Prelude.<$> (x Prelude..?> "Group")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateGroup

instance Prelude.NFData UpdateGroup

instance Prelude.ToHeaders UpdateGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON UpdateGroup where
  toJSON UpdateGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("GroupName" Prelude..=) Prelude.<$> groupName,
            ("Group" Prelude..=) Prelude.<$> group',
            ("Description" Prelude..=) Prelude.<$> description
          ]
      )

instance Prelude.ToPath UpdateGroup where
  toPath = Prelude.const "/update-group"

instance Prelude.ToQuery UpdateGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateGroupResponse' smart constructor.
data UpdateGroupResponse = UpdateGroupResponse'
  { -- | The update description of the resource group.
    group' :: Prelude.Maybe Group,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'group'', 'updateGroupResponse_group' - The update description of the resource group.
--
-- 'httpStatus', 'updateGroupResponse_httpStatus' - The response's http status code.
newUpdateGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateGroupResponse
newUpdateGroupResponse pHttpStatus_ =
  UpdateGroupResponse'
    { group' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The update description of the resource group.
updateGroupResponse_group :: Lens.Lens' UpdateGroupResponse (Prelude.Maybe Group)
updateGroupResponse_group = Lens.lens (\UpdateGroupResponse' {group'} -> group') (\s@UpdateGroupResponse' {} a -> s {group' = a} :: UpdateGroupResponse)

-- | The response's http status code.
updateGroupResponse_httpStatus :: Lens.Lens' UpdateGroupResponse Prelude.Int
updateGroupResponse_httpStatus = Lens.lens (\UpdateGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateGroupResponse' {} a -> s {httpStatus = a} :: UpdateGroupResponse)

instance Prelude.NFData UpdateGroupResponse
