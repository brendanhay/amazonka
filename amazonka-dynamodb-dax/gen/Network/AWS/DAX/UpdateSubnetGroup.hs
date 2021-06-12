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
-- Module      : Network.AWS.DAX.UpdateSubnetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing subnet group.
module Network.AWS.DAX.UpdateSubnetGroup
  ( -- * Creating a Request
    UpdateSubnetGroup (..),
    newUpdateSubnetGroup,

    -- * Request Lenses
    updateSubnetGroup_subnetIds,
    updateSubnetGroup_description,
    updateSubnetGroup_subnetGroupName,

    -- * Destructuring the Response
    UpdateSubnetGroupResponse (..),
    newUpdateSubnetGroupResponse,

    -- * Response Lenses
    updateSubnetGroupResponse_subnetGroup,
    updateSubnetGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateSubnetGroup' smart constructor.
data UpdateSubnetGroup = UpdateSubnetGroup'
  { -- | A list of subnet IDs in the subnet group.
    subnetIds :: Core.Maybe [Core.Text],
    -- | A description of the subnet group.
    description :: Core.Maybe Core.Text,
    -- | The name of the subnet group.
    subnetGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateSubnetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnetIds', 'updateSubnetGroup_subnetIds' - A list of subnet IDs in the subnet group.
--
-- 'description', 'updateSubnetGroup_description' - A description of the subnet group.
--
-- 'subnetGroupName', 'updateSubnetGroup_subnetGroupName' - The name of the subnet group.
newUpdateSubnetGroup ::
  -- | 'subnetGroupName'
  Core.Text ->
  UpdateSubnetGroup
newUpdateSubnetGroup pSubnetGroupName_ =
  UpdateSubnetGroup'
    { subnetIds = Core.Nothing,
      description = Core.Nothing,
      subnetGroupName = pSubnetGroupName_
    }

-- | A list of subnet IDs in the subnet group.
updateSubnetGroup_subnetIds :: Lens.Lens' UpdateSubnetGroup (Core.Maybe [Core.Text])
updateSubnetGroup_subnetIds = Lens.lens (\UpdateSubnetGroup' {subnetIds} -> subnetIds) (\s@UpdateSubnetGroup' {} a -> s {subnetIds = a} :: UpdateSubnetGroup) Core.. Lens.mapping Lens._Coerce

-- | A description of the subnet group.
updateSubnetGroup_description :: Lens.Lens' UpdateSubnetGroup (Core.Maybe Core.Text)
updateSubnetGroup_description = Lens.lens (\UpdateSubnetGroup' {description} -> description) (\s@UpdateSubnetGroup' {} a -> s {description = a} :: UpdateSubnetGroup)

-- | The name of the subnet group.
updateSubnetGroup_subnetGroupName :: Lens.Lens' UpdateSubnetGroup Core.Text
updateSubnetGroup_subnetGroupName = Lens.lens (\UpdateSubnetGroup' {subnetGroupName} -> subnetGroupName) (\s@UpdateSubnetGroup' {} a -> s {subnetGroupName = a} :: UpdateSubnetGroup)

instance Core.AWSRequest UpdateSubnetGroup where
  type
    AWSResponse UpdateSubnetGroup =
      UpdateSubnetGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSubnetGroupResponse'
            Core.<$> (x Core..?> "SubnetGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateSubnetGroup

instance Core.NFData UpdateSubnetGroup

instance Core.ToHeaders UpdateSubnetGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonDAXV3.UpdateSubnetGroup" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateSubnetGroup where
  toJSON UpdateSubnetGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SubnetIds" Core..=) Core.<$> subnetIds,
            ("Description" Core..=) Core.<$> description,
            Core.Just
              ("SubnetGroupName" Core..= subnetGroupName)
          ]
      )

instance Core.ToPath UpdateSubnetGroup where
  toPath = Core.const "/"

instance Core.ToQuery UpdateSubnetGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateSubnetGroupResponse' smart constructor.
data UpdateSubnetGroupResponse = UpdateSubnetGroupResponse'
  { -- | The subnet group that has been modified.
    subnetGroup :: Core.Maybe SubnetGroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateSubnetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnetGroup', 'updateSubnetGroupResponse_subnetGroup' - The subnet group that has been modified.
--
-- 'httpStatus', 'updateSubnetGroupResponse_httpStatus' - The response's http status code.
newUpdateSubnetGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateSubnetGroupResponse
newUpdateSubnetGroupResponse pHttpStatus_ =
  UpdateSubnetGroupResponse'
    { subnetGroup =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The subnet group that has been modified.
updateSubnetGroupResponse_subnetGroup :: Lens.Lens' UpdateSubnetGroupResponse (Core.Maybe SubnetGroup)
updateSubnetGroupResponse_subnetGroup = Lens.lens (\UpdateSubnetGroupResponse' {subnetGroup} -> subnetGroup) (\s@UpdateSubnetGroupResponse' {} a -> s {subnetGroup = a} :: UpdateSubnetGroupResponse)

-- | The response's http status code.
updateSubnetGroupResponse_httpStatus :: Lens.Lens' UpdateSubnetGroupResponse Core.Int
updateSubnetGroupResponse_httpStatus = Lens.lens (\UpdateSubnetGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateSubnetGroupResponse' {} a -> s {httpStatus = a} :: UpdateSubnetGroupResponse)

instance Core.NFData UpdateSubnetGroupResponse
