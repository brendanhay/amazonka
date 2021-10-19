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
    updateGroup_filterExpression,
    updateGroup_insightsConfiguration,
    updateGroup_groupARN,
    updateGroup_groupName,

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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.XRay.Types

-- | /See:/ 'newUpdateGroup' smart constructor.
data UpdateGroup = UpdateGroup'
  { -- | The updated filter expression defining criteria by which to group
    -- traces.
    filterExpression :: Prelude.Maybe Prelude.Text,
    -- | The structure containing configurations related to insights.
    --
    -- -   The InsightsEnabled boolean can be set to true to enable insights
    --     for the group or false to disable insights for the group.
    --
    -- -   The NotifcationsEnabled boolean can be set to true to enable
    --     insights notifications for the group. Notifications can only be
    --     enabled on a group with InsightsEnabled set to true.
    insightsConfiguration :: Prelude.Maybe InsightsConfiguration,
    -- | The ARN that was generated upon creation.
    groupARN :: Prelude.Maybe Prelude.Text,
    -- | The case-sensitive name of the group.
    groupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterExpression', 'updateGroup_filterExpression' - The updated filter expression defining criteria by which to group
-- traces.
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
-- 'groupARN', 'updateGroup_groupARN' - The ARN that was generated upon creation.
--
-- 'groupName', 'updateGroup_groupName' - The case-sensitive name of the group.
newUpdateGroup ::
  UpdateGroup
newUpdateGroup =
  UpdateGroup'
    { filterExpression = Prelude.Nothing,
      insightsConfiguration = Prelude.Nothing,
      groupARN = Prelude.Nothing,
      groupName = Prelude.Nothing
    }

-- | The updated filter expression defining criteria by which to group
-- traces.
updateGroup_filterExpression :: Lens.Lens' UpdateGroup (Prelude.Maybe Prelude.Text)
updateGroup_filterExpression = Lens.lens (\UpdateGroup' {filterExpression} -> filterExpression) (\s@UpdateGroup' {} a -> s {filterExpression = a} :: UpdateGroup)

-- | The structure containing configurations related to insights.
--
-- -   The InsightsEnabled boolean can be set to true to enable insights
--     for the group or false to disable insights for the group.
--
-- -   The NotifcationsEnabled boolean can be set to true to enable
--     insights notifications for the group. Notifications can only be
--     enabled on a group with InsightsEnabled set to true.
updateGroup_insightsConfiguration :: Lens.Lens' UpdateGroup (Prelude.Maybe InsightsConfiguration)
updateGroup_insightsConfiguration = Lens.lens (\UpdateGroup' {insightsConfiguration} -> insightsConfiguration) (\s@UpdateGroup' {} a -> s {insightsConfiguration = a} :: UpdateGroup)

-- | The ARN that was generated upon creation.
updateGroup_groupARN :: Lens.Lens' UpdateGroup (Prelude.Maybe Prelude.Text)
updateGroup_groupARN = Lens.lens (\UpdateGroup' {groupARN} -> groupARN) (\s@UpdateGroup' {} a -> s {groupARN = a} :: UpdateGroup)

-- | The case-sensitive name of the group.
updateGroup_groupName :: Lens.Lens' UpdateGroup (Prelude.Maybe Prelude.Text)
updateGroup_groupName = Lens.lens (\UpdateGroup' {groupName} -> groupName) (\s@UpdateGroup' {} a -> s {groupName = a} :: UpdateGroup)

instance Core.AWSRequest UpdateGroup where
  type AWSResponse UpdateGroup = UpdateGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGroupResponse'
            Prelude.<$> (x Core..?> "Group")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateGroup

instance Prelude.NFData UpdateGroup

instance Core.ToHeaders UpdateGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON UpdateGroup where
  toJSON UpdateGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FilterExpression" Core..=)
              Prelude.<$> filterExpression,
            ("InsightsConfiguration" Core..=)
              Prelude.<$> insightsConfiguration,
            ("GroupARN" Core..=) Prelude.<$> groupARN,
            ("GroupName" Core..=) Prelude.<$> groupName
          ]
      )

instance Core.ToPath UpdateGroup where
  toPath = Prelude.const "/UpdateGroup"

instance Core.ToQuery UpdateGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateGroupResponse' smart constructor.
data UpdateGroupResponse = UpdateGroupResponse'
  { -- | The group that was updated. Contains the name of the group that was
    -- updated, the ARN of the group that was updated, the updated filter
    -- expression, and the updated insight configuration assigned to the group.
    group' :: Prelude.Maybe Group,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateGroupResponse
newUpdateGroupResponse pHttpStatus_ =
  UpdateGroupResponse'
    { group' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The group that was updated. Contains the name of the group that was
-- updated, the ARN of the group that was updated, the updated filter
-- expression, and the updated insight configuration assigned to the group.
updateGroupResponse_group :: Lens.Lens' UpdateGroupResponse (Prelude.Maybe Group)
updateGroupResponse_group = Lens.lens (\UpdateGroupResponse' {group'} -> group') (\s@UpdateGroupResponse' {} a -> s {group' = a} :: UpdateGroupResponse)

-- | The response's http status code.
updateGroupResponse_httpStatus :: Lens.Lens' UpdateGroupResponse Prelude.Int
updateGroupResponse_httpStatus = Lens.lens (\UpdateGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateGroupResponse' {} a -> s {httpStatus = a} :: UpdateGroupResponse)

instance Prelude.NFData UpdateGroupResponse
