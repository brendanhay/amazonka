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
-- Module      : Network.AWS.Connect.DescribeInstanceAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Describes the specified instance attribute.
module Network.AWS.Connect.DescribeInstanceAttribute
  ( -- * Creating a Request
    DescribeInstanceAttribute (..),
    newDescribeInstanceAttribute,

    -- * Request Lenses
    describeInstanceAttribute_instanceId,
    describeInstanceAttribute_attributeType,

    -- * Destructuring the Response
    DescribeInstanceAttributeResponse (..),
    newDescribeInstanceAttributeResponse,

    -- * Response Lenses
    describeInstanceAttributeResponse_attribute,
    describeInstanceAttributeResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeInstanceAttribute' smart constructor.
data DescribeInstanceAttribute = DescribeInstanceAttribute'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text,
    -- | The type of attribute.
    attributeType :: InstanceAttributeType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeInstanceAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'describeInstanceAttribute_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'attributeType', 'describeInstanceAttribute_attributeType' - The type of attribute.
newDescribeInstanceAttribute ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'attributeType'
  InstanceAttributeType ->
  DescribeInstanceAttribute
newDescribeInstanceAttribute
  pInstanceId_
  pAttributeType_ =
    DescribeInstanceAttribute'
      { instanceId =
          pInstanceId_,
        attributeType = pAttributeType_
      }

-- | The identifier of the Amazon Connect instance.
describeInstanceAttribute_instanceId :: Lens.Lens' DescribeInstanceAttribute Core.Text
describeInstanceAttribute_instanceId = Lens.lens (\DescribeInstanceAttribute' {instanceId} -> instanceId) (\s@DescribeInstanceAttribute' {} a -> s {instanceId = a} :: DescribeInstanceAttribute)

-- | The type of attribute.
describeInstanceAttribute_attributeType :: Lens.Lens' DescribeInstanceAttribute InstanceAttributeType
describeInstanceAttribute_attributeType = Lens.lens (\DescribeInstanceAttribute' {attributeType} -> attributeType) (\s@DescribeInstanceAttribute' {} a -> s {attributeType = a} :: DescribeInstanceAttribute)

instance Core.AWSRequest DescribeInstanceAttribute where
  type
    AWSResponse DescribeInstanceAttribute =
      DescribeInstanceAttributeResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInstanceAttributeResponse'
            Core.<$> (x Core..?> "Attribute")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeInstanceAttribute

instance Core.NFData DescribeInstanceAttribute

instance Core.ToHeaders DescribeInstanceAttribute where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DescribeInstanceAttribute where
  toPath DescribeInstanceAttribute' {..} =
    Core.mconcat
      [ "/instance/",
        Core.toBS instanceId,
        "/attribute/",
        Core.toBS attributeType
      ]

instance Core.ToQuery DescribeInstanceAttribute where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeInstanceAttributeResponse' smart constructor.
data DescribeInstanceAttributeResponse = DescribeInstanceAttributeResponse'
  { -- | The type of attribute.
    attribute :: Core.Maybe Attribute,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeInstanceAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attribute', 'describeInstanceAttributeResponse_attribute' - The type of attribute.
--
-- 'httpStatus', 'describeInstanceAttributeResponse_httpStatus' - The response's http status code.
newDescribeInstanceAttributeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeInstanceAttributeResponse
newDescribeInstanceAttributeResponse pHttpStatus_ =
  DescribeInstanceAttributeResponse'
    { attribute =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The type of attribute.
describeInstanceAttributeResponse_attribute :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe Attribute)
describeInstanceAttributeResponse_attribute = Lens.lens (\DescribeInstanceAttributeResponse' {attribute} -> attribute) (\s@DescribeInstanceAttributeResponse' {} a -> s {attribute = a} :: DescribeInstanceAttributeResponse)

-- | The response's http status code.
describeInstanceAttributeResponse_httpStatus :: Lens.Lens' DescribeInstanceAttributeResponse Core.Int
describeInstanceAttributeResponse_httpStatus = Lens.lens (\DescribeInstanceAttributeResponse' {httpStatus} -> httpStatus) (\s@DescribeInstanceAttributeResponse' {} a -> s {httpStatus = a} :: DescribeInstanceAttributeResponse)

instance
  Core.NFData
    DescribeInstanceAttributeResponse
