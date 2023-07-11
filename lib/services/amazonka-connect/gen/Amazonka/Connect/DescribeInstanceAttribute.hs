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
-- Module      : Amazonka.Connect.DescribeInstanceAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Describes the specified instance attribute.
module Amazonka.Connect.DescribeInstanceAttribute
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

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeInstanceAttribute' smart constructor.
data DescribeInstanceAttribute = DescribeInstanceAttribute'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The type of attribute.
    attributeType :: InstanceAttributeType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstanceAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'describeInstanceAttribute_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'attributeType', 'describeInstanceAttribute_attributeType' - The type of attribute.
newDescribeInstanceAttribute ::
  -- | 'instanceId'
  Prelude.Text ->
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

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
describeInstanceAttribute_instanceId :: Lens.Lens' DescribeInstanceAttribute Prelude.Text
describeInstanceAttribute_instanceId = Lens.lens (\DescribeInstanceAttribute' {instanceId} -> instanceId) (\s@DescribeInstanceAttribute' {} a -> s {instanceId = a} :: DescribeInstanceAttribute)

-- | The type of attribute.
describeInstanceAttribute_attributeType :: Lens.Lens' DescribeInstanceAttribute InstanceAttributeType
describeInstanceAttribute_attributeType = Lens.lens (\DescribeInstanceAttribute' {attributeType} -> attributeType) (\s@DescribeInstanceAttribute' {} a -> s {attributeType = a} :: DescribeInstanceAttribute)

instance Core.AWSRequest DescribeInstanceAttribute where
  type
    AWSResponse DescribeInstanceAttribute =
      DescribeInstanceAttributeResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInstanceAttributeResponse'
            Prelude.<$> (x Data..?> "Attribute")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInstanceAttribute where
  hashWithSalt _salt DescribeInstanceAttribute' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` attributeType

instance Prelude.NFData DescribeInstanceAttribute where
  rnf DescribeInstanceAttribute' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf attributeType

instance Data.ToHeaders DescribeInstanceAttribute where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeInstanceAttribute where
  toPath DescribeInstanceAttribute' {..} =
    Prelude.mconcat
      [ "/instance/",
        Data.toBS instanceId,
        "/attribute/",
        Data.toBS attributeType
      ]

instance Data.ToQuery DescribeInstanceAttribute where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeInstanceAttributeResponse' smart constructor.
data DescribeInstanceAttributeResponse = DescribeInstanceAttributeResponse'
  { -- | The type of attribute.
    attribute :: Prelude.Maybe Attribute,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeInstanceAttributeResponse
newDescribeInstanceAttributeResponse pHttpStatus_ =
  DescribeInstanceAttributeResponse'
    { attribute =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The type of attribute.
describeInstanceAttributeResponse_attribute :: Lens.Lens' DescribeInstanceAttributeResponse (Prelude.Maybe Attribute)
describeInstanceAttributeResponse_attribute = Lens.lens (\DescribeInstanceAttributeResponse' {attribute} -> attribute) (\s@DescribeInstanceAttributeResponse' {} a -> s {attribute = a} :: DescribeInstanceAttributeResponse)

-- | The response's http status code.
describeInstanceAttributeResponse_httpStatus :: Lens.Lens' DescribeInstanceAttributeResponse Prelude.Int
describeInstanceAttributeResponse_httpStatus = Lens.lens (\DescribeInstanceAttributeResponse' {httpStatus} -> httpStatus) (\s@DescribeInstanceAttributeResponse' {} a -> s {httpStatus = a} :: DescribeInstanceAttributeResponse)

instance
  Prelude.NFData
    DescribeInstanceAttributeResponse
  where
  rnf DescribeInstanceAttributeResponse' {..} =
    Prelude.rnf attribute
      `Prelude.seq` Prelude.rnf httpStatus
