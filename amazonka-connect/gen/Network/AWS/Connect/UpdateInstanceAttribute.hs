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
-- Module      : Network.AWS.Connect.UpdateInstanceAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Updates the value for the specified attribute type.
module Network.AWS.Connect.UpdateInstanceAttribute
  ( -- * Creating a Request
    UpdateInstanceAttribute (..),
    newUpdateInstanceAttribute,

    -- * Request Lenses
    updateInstanceAttribute_instanceId,
    updateInstanceAttribute_attributeType,
    updateInstanceAttribute_value,

    -- * Destructuring the Response
    UpdateInstanceAttributeResponse (..),
    newUpdateInstanceAttributeResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateInstanceAttribute' smart constructor.
data UpdateInstanceAttribute = UpdateInstanceAttribute'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text,
    -- | The type of attribute.
    attributeType :: InstanceAttributeType,
    -- | The value for the attribute. Maximum character limit is 100.
    value :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateInstanceAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'updateInstanceAttribute_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'attributeType', 'updateInstanceAttribute_attributeType' - The type of attribute.
--
-- 'value', 'updateInstanceAttribute_value' - The value for the attribute. Maximum character limit is 100.
newUpdateInstanceAttribute ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'attributeType'
  InstanceAttributeType ->
  -- | 'value'
  Core.Text ->
  UpdateInstanceAttribute
newUpdateInstanceAttribute
  pInstanceId_
  pAttributeType_
  pValue_ =
    UpdateInstanceAttribute'
      { instanceId = pInstanceId_,
        attributeType = pAttributeType_,
        value = pValue_
      }

-- | The identifier of the Amazon Connect instance.
updateInstanceAttribute_instanceId :: Lens.Lens' UpdateInstanceAttribute Core.Text
updateInstanceAttribute_instanceId = Lens.lens (\UpdateInstanceAttribute' {instanceId} -> instanceId) (\s@UpdateInstanceAttribute' {} a -> s {instanceId = a} :: UpdateInstanceAttribute)

-- | The type of attribute.
updateInstanceAttribute_attributeType :: Lens.Lens' UpdateInstanceAttribute InstanceAttributeType
updateInstanceAttribute_attributeType = Lens.lens (\UpdateInstanceAttribute' {attributeType} -> attributeType) (\s@UpdateInstanceAttribute' {} a -> s {attributeType = a} :: UpdateInstanceAttribute)

-- | The value for the attribute. Maximum character limit is 100.
updateInstanceAttribute_value :: Lens.Lens' UpdateInstanceAttribute Core.Text
updateInstanceAttribute_value = Lens.lens (\UpdateInstanceAttribute' {value} -> value) (\s@UpdateInstanceAttribute' {} a -> s {value = a} :: UpdateInstanceAttribute)

instance Core.AWSRequest UpdateInstanceAttribute where
  type
    AWSResponse UpdateInstanceAttribute =
      UpdateInstanceAttributeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      UpdateInstanceAttributeResponse'

instance Core.Hashable UpdateInstanceAttribute

instance Core.NFData UpdateInstanceAttribute

instance Core.ToHeaders UpdateInstanceAttribute where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateInstanceAttribute where
  toJSON UpdateInstanceAttribute' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Value" Core..= value)])

instance Core.ToPath UpdateInstanceAttribute where
  toPath UpdateInstanceAttribute' {..} =
    Core.mconcat
      [ "/instance/",
        Core.toBS instanceId,
        "/attribute/",
        Core.toBS attributeType
      ]

instance Core.ToQuery UpdateInstanceAttribute where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateInstanceAttributeResponse' smart constructor.
data UpdateInstanceAttributeResponse = UpdateInstanceAttributeResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateInstanceAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateInstanceAttributeResponse ::
  UpdateInstanceAttributeResponse
newUpdateInstanceAttributeResponse =
  UpdateInstanceAttributeResponse'

instance Core.NFData UpdateInstanceAttributeResponse
