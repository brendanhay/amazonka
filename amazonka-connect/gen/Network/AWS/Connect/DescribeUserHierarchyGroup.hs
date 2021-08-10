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
-- Module      : Network.AWS.Connect.DescribeUserHierarchyGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified hierarchy group.
module Network.AWS.Connect.DescribeUserHierarchyGroup
  ( -- * Creating a Request
    DescribeUserHierarchyGroup (..),
    newDescribeUserHierarchyGroup,

    -- * Request Lenses
    describeUserHierarchyGroup_hierarchyGroupId,
    describeUserHierarchyGroup_instanceId,

    -- * Destructuring the Response
    DescribeUserHierarchyGroupResponse (..),
    newDescribeUserHierarchyGroupResponse,

    -- * Response Lenses
    describeUserHierarchyGroupResponse_hierarchyGroup,
    describeUserHierarchyGroupResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeUserHierarchyGroup' smart constructor.
data DescribeUserHierarchyGroup = DescribeUserHierarchyGroup'
  { -- | The identifier of the hierarchy group.
    hierarchyGroupId :: Prelude.Text,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeUserHierarchyGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hierarchyGroupId', 'describeUserHierarchyGroup_hierarchyGroupId' - The identifier of the hierarchy group.
--
-- 'instanceId', 'describeUserHierarchyGroup_instanceId' - The identifier of the Amazon Connect instance.
newDescribeUserHierarchyGroup ::
  -- | 'hierarchyGroupId'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  DescribeUserHierarchyGroup
newDescribeUserHierarchyGroup
  pHierarchyGroupId_
  pInstanceId_ =
    DescribeUserHierarchyGroup'
      { hierarchyGroupId =
          pHierarchyGroupId_,
        instanceId = pInstanceId_
      }

-- | The identifier of the hierarchy group.
describeUserHierarchyGroup_hierarchyGroupId :: Lens.Lens' DescribeUserHierarchyGroup Prelude.Text
describeUserHierarchyGroup_hierarchyGroupId = Lens.lens (\DescribeUserHierarchyGroup' {hierarchyGroupId} -> hierarchyGroupId) (\s@DescribeUserHierarchyGroup' {} a -> s {hierarchyGroupId = a} :: DescribeUserHierarchyGroup)

-- | The identifier of the Amazon Connect instance.
describeUserHierarchyGroup_instanceId :: Lens.Lens' DescribeUserHierarchyGroup Prelude.Text
describeUserHierarchyGroup_instanceId = Lens.lens (\DescribeUserHierarchyGroup' {instanceId} -> instanceId) (\s@DescribeUserHierarchyGroup' {} a -> s {instanceId = a} :: DescribeUserHierarchyGroup)

instance Core.AWSRequest DescribeUserHierarchyGroup where
  type
    AWSResponse DescribeUserHierarchyGroup =
      DescribeUserHierarchyGroupResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserHierarchyGroupResponse'
            Prelude.<$> (x Core..?> "HierarchyGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeUserHierarchyGroup

instance Prelude.NFData DescribeUserHierarchyGroup

instance Core.ToHeaders DescribeUserHierarchyGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeUserHierarchyGroup where
  toPath DescribeUserHierarchyGroup' {..} =
    Prelude.mconcat
      [ "/user-hierarchy-groups/",
        Core.toBS instanceId,
        "/",
        Core.toBS hierarchyGroupId
      ]

instance Core.ToQuery DescribeUserHierarchyGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeUserHierarchyGroupResponse' smart constructor.
data DescribeUserHierarchyGroupResponse = DescribeUserHierarchyGroupResponse'
  { -- | Information about the hierarchy group.
    hierarchyGroup :: Prelude.Maybe HierarchyGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeUserHierarchyGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hierarchyGroup', 'describeUserHierarchyGroupResponse_hierarchyGroup' - Information about the hierarchy group.
--
-- 'httpStatus', 'describeUserHierarchyGroupResponse_httpStatus' - The response's http status code.
newDescribeUserHierarchyGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeUserHierarchyGroupResponse
newDescribeUserHierarchyGroupResponse pHttpStatus_ =
  DescribeUserHierarchyGroupResponse'
    { hierarchyGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the hierarchy group.
describeUserHierarchyGroupResponse_hierarchyGroup :: Lens.Lens' DescribeUserHierarchyGroupResponse (Prelude.Maybe HierarchyGroup)
describeUserHierarchyGroupResponse_hierarchyGroup = Lens.lens (\DescribeUserHierarchyGroupResponse' {hierarchyGroup} -> hierarchyGroup) (\s@DescribeUserHierarchyGroupResponse' {} a -> s {hierarchyGroup = a} :: DescribeUserHierarchyGroupResponse)

-- | The response's http status code.
describeUserHierarchyGroupResponse_httpStatus :: Lens.Lens' DescribeUserHierarchyGroupResponse Prelude.Int
describeUserHierarchyGroupResponse_httpStatus = Lens.lens (\DescribeUserHierarchyGroupResponse' {httpStatus} -> httpStatus) (\s@DescribeUserHierarchyGroupResponse' {} a -> s {httpStatus = a} :: DescribeUserHierarchyGroupResponse)

instance
  Prelude.NFData
    DescribeUserHierarchyGroupResponse
