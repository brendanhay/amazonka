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
-- Module      : Network.AWS.DAX.CreateSubnetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new subnet group.
module Network.AWS.DAX.CreateSubnetGroup
  ( -- * Creating a Request
    CreateSubnetGroup (..),
    newCreateSubnetGroup,

    -- * Request Lenses
    createSubnetGroup_description,
    createSubnetGroup_subnetGroupName,
    createSubnetGroup_subnetIds,

    -- * Destructuring the Response
    CreateSubnetGroupResponse (..),
    newCreateSubnetGroupResponse,

    -- * Response Lenses
    createSubnetGroupResponse_subnetGroup,
    createSubnetGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateSubnetGroup' smart constructor.
data CreateSubnetGroup = CreateSubnetGroup'
  { -- | A description for the subnet group
    description :: Core.Maybe Core.Text,
    -- | A name for the subnet group. This value is stored as a lowercase string.
    subnetGroupName :: Core.Text,
    -- | A list of VPC subnet IDs for the subnet group.
    subnetIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateSubnetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createSubnetGroup_description' - A description for the subnet group
--
-- 'subnetGroupName', 'createSubnetGroup_subnetGroupName' - A name for the subnet group. This value is stored as a lowercase string.
--
-- 'subnetIds', 'createSubnetGroup_subnetIds' - A list of VPC subnet IDs for the subnet group.
newCreateSubnetGroup ::
  -- | 'subnetGroupName'
  Core.Text ->
  CreateSubnetGroup
newCreateSubnetGroup pSubnetGroupName_ =
  CreateSubnetGroup'
    { description = Core.Nothing,
      subnetGroupName = pSubnetGroupName_,
      subnetIds = Core.mempty
    }

-- | A description for the subnet group
createSubnetGroup_description :: Lens.Lens' CreateSubnetGroup (Core.Maybe Core.Text)
createSubnetGroup_description = Lens.lens (\CreateSubnetGroup' {description} -> description) (\s@CreateSubnetGroup' {} a -> s {description = a} :: CreateSubnetGroup)

-- | A name for the subnet group. This value is stored as a lowercase string.
createSubnetGroup_subnetGroupName :: Lens.Lens' CreateSubnetGroup Core.Text
createSubnetGroup_subnetGroupName = Lens.lens (\CreateSubnetGroup' {subnetGroupName} -> subnetGroupName) (\s@CreateSubnetGroup' {} a -> s {subnetGroupName = a} :: CreateSubnetGroup)

-- | A list of VPC subnet IDs for the subnet group.
createSubnetGroup_subnetIds :: Lens.Lens' CreateSubnetGroup [Core.Text]
createSubnetGroup_subnetIds = Lens.lens (\CreateSubnetGroup' {subnetIds} -> subnetIds) (\s@CreateSubnetGroup' {} a -> s {subnetIds = a} :: CreateSubnetGroup) Core.. Lens._Coerce

instance Core.AWSRequest CreateSubnetGroup where
  type
    AWSResponse CreateSubnetGroup =
      CreateSubnetGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSubnetGroupResponse'
            Core.<$> (x Core..?> "SubnetGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateSubnetGroup

instance Core.NFData CreateSubnetGroup

instance Core.ToHeaders CreateSubnetGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonDAXV3.CreateSubnetGroup" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateSubnetGroup where
  toJSON CreateSubnetGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Description" Core..=) Core.<$> description,
            Core.Just
              ("SubnetGroupName" Core..= subnetGroupName),
            Core.Just ("SubnetIds" Core..= subnetIds)
          ]
      )

instance Core.ToPath CreateSubnetGroup where
  toPath = Core.const "/"

instance Core.ToQuery CreateSubnetGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateSubnetGroupResponse' smart constructor.
data CreateSubnetGroupResponse = CreateSubnetGroupResponse'
  { -- | Represents the output of a /CreateSubnetGroup/ operation.
    subnetGroup :: Core.Maybe SubnetGroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateSubnetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnetGroup', 'createSubnetGroupResponse_subnetGroup' - Represents the output of a /CreateSubnetGroup/ operation.
--
-- 'httpStatus', 'createSubnetGroupResponse_httpStatus' - The response's http status code.
newCreateSubnetGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateSubnetGroupResponse
newCreateSubnetGroupResponse pHttpStatus_ =
  CreateSubnetGroupResponse'
    { subnetGroup =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the output of a /CreateSubnetGroup/ operation.
createSubnetGroupResponse_subnetGroup :: Lens.Lens' CreateSubnetGroupResponse (Core.Maybe SubnetGroup)
createSubnetGroupResponse_subnetGroup = Lens.lens (\CreateSubnetGroupResponse' {subnetGroup} -> subnetGroup) (\s@CreateSubnetGroupResponse' {} a -> s {subnetGroup = a} :: CreateSubnetGroupResponse)

-- | The response's http status code.
createSubnetGroupResponse_httpStatus :: Lens.Lens' CreateSubnetGroupResponse Core.Int
createSubnetGroupResponse_httpStatus = Lens.lens (\CreateSubnetGroupResponse' {httpStatus} -> httpStatus) (\s@CreateSubnetGroupResponse' {} a -> s {httpStatus = a} :: CreateSubnetGroupResponse)

instance Core.NFData CreateSubnetGroupResponse
