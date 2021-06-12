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
-- Module      : Network.AWS.XRay.UpdateGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a group resource.
module Network.AWS.XRay.UpdateGroup
  ( -- * Creating a Request
    UpdateGroup (..),
    newUpdateGroup,

    -- * Request Lenses
    updateGroup_groupName,
    updateGroup_insightsConfiguration,
    updateGroup_filterExpression,
    updateGroup_groupARN,

    -- * Destructuring the Response
    UpdateGroupResponse (..),
    newUpdateGroupResponse,

    -- * Response Lenses
    updateGroupResponse_group,
    updateGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.XRay.Types

-- | /See:/ 'newUpdateGroup' smart constructor.
data UpdateGroup = UpdateGroup'
  { -- | The case-sensitive name of the group.
    groupName :: Core.Maybe Core.Text,
    -- | The structure containing configurations related to insights.
    --
    -- -   The InsightsEnabled boolean can be set to true to enable insights
    --     for the group or false to disable insights for the group.
    --
    -- -   The NotifcationsEnabled boolean can be set to true to enable
    --     insights notifications for the group. Notifications can only be
    --     enabled on a group with InsightsEnabled set to true.
    insightsConfiguration :: Core.Maybe InsightsConfiguration,
    -- | The updated filter expression defining criteria by which to group
    -- traces.
    filterExpression :: Core.Maybe Core.Text,
    -- | The ARN that was generated upon creation.
    groupARN :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'updateGroup_groupName' - The case-sensitive name of the group.
--
-- 'insightsConfiguration', 'updateGroup_insightsConfiguration' - The structure containing configurations related to insights.
--
-- -   The InsightsEnabled boolean can be set to true to enable insights
--     for the group or false to disable insights for the group.
--
-- -   The NotifcationsEnabled boolean can be set to true to enable
--     insights notifications for the group. Notifications can only be
--     enabled on a group with InsightsEnabled set to true.
--
-- 'filterExpression', 'updateGroup_filterExpression' - The updated filter expression defining criteria by which to group
-- traces.
--
-- 'groupARN', 'updateGroup_groupARN' - The ARN that was generated upon creation.
newUpdateGroup ::
  UpdateGroup
newUpdateGroup =
  UpdateGroup'
    { groupName = Core.Nothing,
      insightsConfiguration = Core.Nothing,
      filterExpression = Core.Nothing,
      groupARN = Core.Nothing
    }

-- | The case-sensitive name of the group.
updateGroup_groupName :: Lens.Lens' UpdateGroup (Core.Maybe Core.Text)
updateGroup_groupName = Lens.lens (\UpdateGroup' {groupName} -> groupName) (\s@UpdateGroup' {} a -> s {groupName = a} :: UpdateGroup)

-- | The structure containing configurations related to insights.
--
-- -   The InsightsEnabled boolean can be set to true to enable insights
--     for the group or false to disable insights for the group.
--
-- -   The NotifcationsEnabled boolean can be set to true to enable
--     insights notifications for the group. Notifications can only be
--     enabled on a group with InsightsEnabled set to true.
updateGroup_insightsConfiguration :: Lens.Lens' UpdateGroup (Core.Maybe InsightsConfiguration)
updateGroup_insightsConfiguration = Lens.lens (\UpdateGroup' {insightsConfiguration} -> insightsConfiguration) (\s@UpdateGroup' {} a -> s {insightsConfiguration = a} :: UpdateGroup)

-- | The updated filter expression defining criteria by which to group
-- traces.
updateGroup_filterExpression :: Lens.Lens' UpdateGroup (Core.Maybe Core.Text)
updateGroup_filterExpression = Lens.lens (\UpdateGroup' {filterExpression} -> filterExpression) (\s@UpdateGroup' {} a -> s {filterExpression = a} :: UpdateGroup)

-- | The ARN that was generated upon creation.
updateGroup_groupARN :: Lens.Lens' UpdateGroup (Core.Maybe Core.Text)
updateGroup_groupARN = Lens.lens (\UpdateGroup' {groupARN} -> groupARN) (\s@UpdateGroup' {} a -> s {groupARN = a} :: UpdateGroup)

instance Core.AWSRequest UpdateGroup where
  type AWSResponse UpdateGroup = UpdateGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGroupResponse'
            Core.<$> (x Core..?> "Group")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateGroup

instance Core.NFData UpdateGroup

instance Core.ToHeaders UpdateGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON UpdateGroup where
  toJSON UpdateGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("GroupName" Core..=) Core.<$> groupName,
            ("InsightsConfiguration" Core..=)
              Core.<$> insightsConfiguration,
            ("FilterExpression" Core..=)
              Core.<$> filterExpression,
            ("GroupARN" Core..=) Core.<$> groupARN
          ]
      )

instance Core.ToPath UpdateGroup where
  toPath = Core.const "/UpdateGroup"

instance Core.ToQuery UpdateGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateGroupResponse' smart constructor.
data UpdateGroupResponse = UpdateGroupResponse'
  { -- | The group that was updated. Contains the name of the group that was
    -- updated, the ARN of the group that was updated, the updated filter
    -- expression, and the updated insight configuration assigned to the group.
    group' :: Core.Maybe Group,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'group'', 'updateGroupResponse_group' - The group that was updated. Contains the name of the group that was
-- updated, the ARN of the group that was updated, the updated filter
-- expression, and the updated insight configuration assigned to the group.
--
-- 'httpStatus', 'updateGroupResponse_httpStatus' - The response's http status code.
newUpdateGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateGroupResponse
newUpdateGroupResponse pHttpStatus_ =
  UpdateGroupResponse'
    { group' = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The group that was updated. Contains the name of the group that was
-- updated, the ARN of the group that was updated, the updated filter
-- expression, and the updated insight configuration assigned to the group.
updateGroupResponse_group :: Lens.Lens' UpdateGroupResponse (Core.Maybe Group)
updateGroupResponse_group = Lens.lens (\UpdateGroupResponse' {group'} -> group') (\s@UpdateGroupResponse' {} a -> s {group' = a} :: UpdateGroupResponse)

-- | The response's http status code.
updateGroupResponse_httpStatus :: Lens.Lens' UpdateGroupResponse Core.Int
updateGroupResponse_httpStatus = Lens.lens (\UpdateGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateGroupResponse' {} a -> s {httpStatus = a} :: UpdateGroupResponse)

instance Core.NFData UpdateGroupResponse
