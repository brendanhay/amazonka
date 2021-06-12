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
-- Module      : Network.AWS.ResourceGroups.UpdateGroupQuery
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the resource query of a group. For more information about
-- resource queries, see
-- <https://docs.aws.amazon.com/ARG/latest/userguide/gettingstarted-query.html#gettingstarted-query-cli-tag Create a tag-based group in Resource Groups>.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
-- -   @resource-groups:UpdateGroupQuery@
module Network.AWS.ResourceGroups.UpdateGroupQuery
  ( -- * Creating a Request
    UpdateGroupQuery (..),
    newUpdateGroupQuery,

    -- * Request Lenses
    updateGroupQuery_groupName,
    updateGroupQuery_group,
    updateGroupQuery_resourceQuery,

    -- * Destructuring the Response
    UpdateGroupQueryResponse (..),
    newUpdateGroupQueryResponse,

    -- * Response Lenses
    updateGroupQueryResponse_groupQuery,
    updateGroupQueryResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import Network.AWS.ResourceGroups.Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateGroupQuery' smart constructor.
data UpdateGroupQuery = UpdateGroupQuery'
  { -- | Don\'t use this parameter. Use @Group@ instead.
    groupName :: Core.Maybe Core.Text,
    -- | The name or the ARN of the resource group to query.
    group' :: Core.Maybe Core.Text,
    -- | The resource query to determine which AWS resources are members of this
    -- resource group.
    --
    -- A resource group can contain either a @Configuration@ or a
    -- @ResourceQuery@, but not both.
    resourceQuery :: ResourceQuery
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateGroupQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'updateGroupQuery_groupName' - Don\'t use this parameter. Use @Group@ instead.
--
-- 'group'', 'updateGroupQuery_group' - The name or the ARN of the resource group to query.
--
-- 'resourceQuery', 'updateGroupQuery_resourceQuery' - The resource query to determine which AWS resources are members of this
-- resource group.
--
-- A resource group can contain either a @Configuration@ or a
-- @ResourceQuery@, but not both.
newUpdateGroupQuery ::
  -- | 'resourceQuery'
  ResourceQuery ->
  UpdateGroupQuery
newUpdateGroupQuery pResourceQuery_ =
  UpdateGroupQuery'
    { groupName = Core.Nothing,
      group' = Core.Nothing,
      resourceQuery = pResourceQuery_
    }

-- | Don\'t use this parameter. Use @Group@ instead.
updateGroupQuery_groupName :: Lens.Lens' UpdateGroupQuery (Core.Maybe Core.Text)
updateGroupQuery_groupName = Lens.lens (\UpdateGroupQuery' {groupName} -> groupName) (\s@UpdateGroupQuery' {} a -> s {groupName = a} :: UpdateGroupQuery)

-- | The name or the ARN of the resource group to query.
updateGroupQuery_group :: Lens.Lens' UpdateGroupQuery (Core.Maybe Core.Text)
updateGroupQuery_group = Lens.lens (\UpdateGroupQuery' {group'} -> group') (\s@UpdateGroupQuery' {} a -> s {group' = a} :: UpdateGroupQuery)

-- | The resource query to determine which AWS resources are members of this
-- resource group.
--
-- A resource group can contain either a @Configuration@ or a
-- @ResourceQuery@, but not both.
updateGroupQuery_resourceQuery :: Lens.Lens' UpdateGroupQuery ResourceQuery
updateGroupQuery_resourceQuery = Lens.lens (\UpdateGroupQuery' {resourceQuery} -> resourceQuery) (\s@UpdateGroupQuery' {} a -> s {resourceQuery = a} :: UpdateGroupQuery)

instance Core.AWSRequest UpdateGroupQuery where
  type
    AWSResponse UpdateGroupQuery =
      UpdateGroupQueryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGroupQueryResponse'
            Core.<$> (x Core..?> "GroupQuery")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateGroupQuery

instance Core.NFData UpdateGroupQuery

instance Core.ToHeaders UpdateGroupQuery where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON UpdateGroupQuery where
  toJSON UpdateGroupQuery' {..} =
    Core.object
      ( Core.catMaybes
          [ ("GroupName" Core..=) Core.<$> groupName,
            ("Group" Core..=) Core.<$> group',
            Core.Just ("ResourceQuery" Core..= resourceQuery)
          ]
      )

instance Core.ToPath UpdateGroupQuery where
  toPath = Core.const "/update-group-query"

instance Core.ToQuery UpdateGroupQuery where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateGroupQueryResponse' smart constructor.
data UpdateGroupQueryResponse = UpdateGroupQueryResponse'
  { -- | The updated resource query associated with the resource group after the
    -- update.
    groupQuery :: Core.Maybe GroupQuery,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateGroupQueryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupQuery', 'updateGroupQueryResponse_groupQuery' - The updated resource query associated with the resource group after the
-- update.
--
-- 'httpStatus', 'updateGroupQueryResponse_httpStatus' - The response's http status code.
newUpdateGroupQueryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateGroupQueryResponse
newUpdateGroupQueryResponse pHttpStatus_ =
  UpdateGroupQueryResponse'
    { groupQuery =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated resource query associated with the resource group after the
-- update.
updateGroupQueryResponse_groupQuery :: Lens.Lens' UpdateGroupQueryResponse (Core.Maybe GroupQuery)
updateGroupQueryResponse_groupQuery = Lens.lens (\UpdateGroupQueryResponse' {groupQuery} -> groupQuery) (\s@UpdateGroupQueryResponse' {} a -> s {groupQuery = a} :: UpdateGroupQueryResponse)

-- | The response's http status code.
updateGroupQueryResponse_httpStatus :: Lens.Lens' UpdateGroupQueryResponse Core.Int
updateGroupQueryResponse_httpStatus = Lens.lens (\UpdateGroupQueryResponse' {httpStatus} -> httpStatus) (\s@UpdateGroupQueryResponse' {} a -> s {httpStatus = a} :: UpdateGroupQueryResponse)

instance Core.NFData UpdateGroupQueryResponse
