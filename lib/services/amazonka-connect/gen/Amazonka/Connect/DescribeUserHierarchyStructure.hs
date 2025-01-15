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
-- Module      : Amazonka.Connect.DescribeUserHierarchyStructure
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the hierarchy structure of the specified Amazon Connect
-- instance.
module Amazonka.Connect.DescribeUserHierarchyStructure
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

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeUserHierarchyStructure' smart constructor.
data DescribeUserHierarchyStructure = DescribeUserHierarchyStructure'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeUserHierarchyStructure' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'describeUserHierarchyStructure_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
newDescribeUserHierarchyStructure ::
  -- | 'instanceId'
  Prelude.Text ->
  DescribeUserHierarchyStructure
newDescribeUserHierarchyStructure pInstanceId_ =
  DescribeUserHierarchyStructure'
    { instanceId =
        pInstanceId_
    }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
describeUserHierarchyStructure_instanceId :: Lens.Lens' DescribeUserHierarchyStructure Prelude.Text
describeUserHierarchyStructure_instanceId = Lens.lens (\DescribeUserHierarchyStructure' {instanceId} -> instanceId) (\s@DescribeUserHierarchyStructure' {} a -> s {instanceId = a} :: DescribeUserHierarchyStructure)

instance
  Core.AWSRequest
    DescribeUserHierarchyStructure
  where
  type
    AWSResponse DescribeUserHierarchyStructure =
      DescribeUserHierarchyStructureResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserHierarchyStructureResponse'
            Prelude.<$> (x Data..?> "HierarchyStructure")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeUserHierarchyStructure
  where
  hashWithSalt
    _salt
    DescribeUserHierarchyStructure' {..} =
      _salt `Prelude.hashWithSalt` instanceId

instance
  Prelude.NFData
    DescribeUserHierarchyStructure
  where
  rnf DescribeUserHierarchyStructure' {..} =
    Prelude.rnf instanceId

instance
  Data.ToHeaders
    DescribeUserHierarchyStructure
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeUserHierarchyStructure where
  toPath DescribeUserHierarchyStructure' {..} =
    Prelude.mconcat
      ["/user-hierarchy-structure/", Data.toBS instanceId]

instance Data.ToQuery DescribeUserHierarchyStructure where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeUserHierarchyStructureResponse' smart constructor.
data DescribeUserHierarchyStructureResponse = DescribeUserHierarchyStructureResponse'
  { -- | Information about the hierarchy structure.
    hierarchyStructure :: Prelude.Maybe HierarchyStructure,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeUserHierarchyStructureResponse
newDescribeUserHierarchyStructureResponse
  pHttpStatus_ =
    DescribeUserHierarchyStructureResponse'
      { hierarchyStructure =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the hierarchy structure.
describeUserHierarchyStructureResponse_hierarchyStructure :: Lens.Lens' DescribeUserHierarchyStructureResponse (Prelude.Maybe HierarchyStructure)
describeUserHierarchyStructureResponse_hierarchyStructure = Lens.lens (\DescribeUserHierarchyStructureResponse' {hierarchyStructure} -> hierarchyStructure) (\s@DescribeUserHierarchyStructureResponse' {} a -> s {hierarchyStructure = a} :: DescribeUserHierarchyStructureResponse)

-- | The response's http status code.
describeUserHierarchyStructureResponse_httpStatus :: Lens.Lens' DescribeUserHierarchyStructureResponse Prelude.Int
describeUserHierarchyStructureResponse_httpStatus = Lens.lens (\DescribeUserHierarchyStructureResponse' {httpStatus} -> httpStatus) (\s@DescribeUserHierarchyStructureResponse' {} a -> s {httpStatus = a} :: DescribeUserHierarchyStructureResponse)

instance
  Prelude.NFData
    DescribeUserHierarchyStructureResponse
  where
  rnf DescribeUserHierarchyStructureResponse' {..} =
    Prelude.rnf hierarchyStructure `Prelude.seq`
      Prelude.rnf httpStatus
