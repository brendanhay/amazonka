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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.XRay.Types

-- | /See:/ 'newUpdateGroup' smart constructor.
data UpdateGroup = UpdateGroup'
  { -- | The case-sensitive name of the group.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The structure containing configurations related to insights.
    --
    -- -   The InsightsEnabled boolean can be set to true to enable insights
    --     for the group or false to disable insights for the group.
    --
    -- -   The NotifcationsEnabled boolean can be set to true to enable
    --     insights notifications for the group. Notifications can only be
    --     enabled on a group with InsightsEnabled set to true.
    insightsConfiguration :: Prelude.Maybe InsightsConfiguration,
    -- | The updated filter expression defining criteria by which to group
    -- traces.
    filterExpression :: Prelude.Maybe Prelude.Text,
    -- | The ARN that was generated upon creation.
    groupARN :: Prelude.Maybe Prelude.Text
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
    { groupName = Prelude.Nothing,
      insightsConfiguration = Prelude.Nothing,
      filterExpression = Prelude.Nothing,
      groupARN = Prelude.Nothing
    }

-- | The case-sensitive name of the group.
updateGroup_groupName :: Lens.Lens' UpdateGroup (Prelude.Maybe Prelude.Text)
updateGroup_groupName = Lens.lens (\UpdateGroup' {groupName} -> groupName) (\s@UpdateGroup' {} a -> s {groupName = a} :: UpdateGroup)

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

-- | The updated filter expression defining criteria by which to group
-- traces.
updateGroup_filterExpression :: Lens.Lens' UpdateGroup (Prelude.Maybe Prelude.Text)
updateGroup_filterExpression = Lens.lens (\UpdateGroup' {filterExpression} -> filterExpression) (\s@UpdateGroup' {} a -> s {filterExpression = a} :: UpdateGroup)

-- | The ARN that was generated upon creation.
updateGroup_groupARN :: Lens.Lens' UpdateGroup (Prelude.Maybe Prelude.Text)
updateGroup_groupARN = Lens.lens (\UpdateGroup' {groupARN} -> groupARN) (\s@UpdateGroup' {} a -> s {groupARN = a} :: UpdateGroup)

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
            ("InsightsConfiguration" Prelude..=)
              Prelude.<$> insightsConfiguration,
            ("FilterExpression" Prelude..=)
              Prelude.<$> filterExpression,
            ("GroupARN" Prelude..=) Prelude.<$> groupARN
          ]
      )

instance Prelude.ToPath UpdateGroup where
  toPath = Prelude.const "/UpdateGroup"

instance Prelude.ToQuery UpdateGroup where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
