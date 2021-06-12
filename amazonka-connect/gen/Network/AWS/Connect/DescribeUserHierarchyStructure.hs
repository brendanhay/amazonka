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
-- Module      : Network.AWS.Connect.DescribeUserHierarchyStructure
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the hierarchy structure of the specified Amazon Connect
-- instance.
module Network.AWS.Connect.DescribeUserHierarchyStructure
  ( -- * Creating a Request
    DescribeUserHierarchyStructure (..),
    newDescribeUserHierarchyStructure,

    -- * Request Lenses
    describeUserHierarchyStructure_instanceId,

    -- * Destructuring the Response
    DescribeUserHierarchyStructureResponse (..),
    newDescribeUserHierarchyStructureResponse,

    -- * Response Lenses
    describeUserHierarchyStructureResponse_hierarchyStructure,
    describeUserHierarchyStructureResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeUserHierarchyStructure' smart constructor.
data DescribeUserHierarchyStructure = DescribeUserHierarchyStructure'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeUserHierarchyStructure' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'describeUserHierarchyStructure_instanceId' - The identifier of the Amazon Connect instance.
newDescribeUserHierarchyStructure ::
  -- | 'instanceId'
  Core.Text ->
  DescribeUserHierarchyStructure
newDescribeUserHierarchyStructure pInstanceId_ =
  DescribeUserHierarchyStructure'
    { instanceId =
        pInstanceId_
    }

-- | The identifier of the Amazon Connect instance.
describeUserHierarchyStructure_instanceId :: Lens.Lens' DescribeUserHierarchyStructure Core.Text
describeUserHierarchyStructure_instanceId = Lens.lens (\DescribeUserHierarchyStructure' {instanceId} -> instanceId) (\s@DescribeUserHierarchyStructure' {} a -> s {instanceId = a} :: DescribeUserHierarchyStructure)

instance
  Core.AWSRequest
    DescribeUserHierarchyStructure
  where
  type
    AWSResponse DescribeUserHierarchyStructure =
      DescribeUserHierarchyStructureResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserHierarchyStructureResponse'
            Core.<$> (x Core..?> "HierarchyStructure")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeUserHierarchyStructure

instance Core.NFData DescribeUserHierarchyStructure

instance
  Core.ToHeaders
    DescribeUserHierarchyStructure
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DescribeUserHierarchyStructure where
  toPath DescribeUserHierarchyStructure' {..} =
    Core.mconcat
      ["/user-hierarchy-structure/", Core.toBS instanceId]

instance Core.ToQuery DescribeUserHierarchyStructure where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeUserHierarchyStructureResponse' smart constructor.
data DescribeUserHierarchyStructureResponse = DescribeUserHierarchyStructureResponse'
  { -- | Information about the hierarchy structure.
    hierarchyStructure :: Core.Maybe HierarchyStructure,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeUserHierarchyStructureResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hierarchyStructure', 'describeUserHierarchyStructureResponse_hierarchyStructure' - Information about the hierarchy structure.
--
-- 'httpStatus', 'describeUserHierarchyStructureResponse_httpStatus' - The response's http status code.
newDescribeUserHierarchyStructureResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeUserHierarchyStructureResponse
newDescribeUserHierarchyStructureResponse
  pHttpStatus_ =
    DescribeUserHierarchyStructureResponse'
      { hierarchyStructure =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the hierarchy structure.
describeUserHierarchyStructureResponse_hierarchyStructure :: Lens.Lens' DescribeUserHierarchyStructureResponse (Core.Maybe HierarchyStructure)
describeUserHierarchyStructureResponse_hierarchyStructure = Lens.lens (\DescribeUserHierarchyStructureResponse' {hierarchyStructure} -> hierarchyStructure) (\s@DescribeUserHierarchyStructureResponse' {} a -> s {hierarchyStructure = a} :: DescribeUserHierarchyStructureResponse)

-- | The response's http status code.
describeUserHierarchyStructureResponse_httpStatus :: Lens.Lens' DescribeUserHierarchyStructureResponse Core.Int
describeUserHierarchyStructureResponse_httpStatus = Lens.lens (\DescribeUserHierarchyStructureResponse' {httpStatus} -> httpStatus) (\s@DescribeUserHierarchyStructureResponse' {} a -> s {httpStatus = a} :: DescribeUserHierarchyStructureResponse)

instance
  Core.NFData
    DescribeUserHierarchyStructureResponse
