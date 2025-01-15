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
-- Module      : Amazonka.Connect.UpdateInstanceAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Updates the value for the specified attribute type.
module Amazonka.Connect.UpdateInstanceAttribute
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

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateInstanceAttribute' smart constructor.
data UpdateInstanceAttribute = UpdateInstanceAttribute'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The type of attribute.
    --
    -- Only allowlisted customers can consume USE_CUSTOM_TTS_VOICES. To access
    -- this feature, contact Amazon Web Services Support for allowlisting.
    attributeType :: InstanceAttributeType,
    -- | The value for the attribute. Maximum character limit is 100.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateInstanceAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'updateInstanceAttribute_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'attributeType', 'updateInstanceAttribute_attributeType' - The type of attribute.
--
-- Only allowlisted customers can consume USE_CUSTOM_TTS_VOICES. To access
-- this feature, contact Amazon Web Services Support for allowlisting.
--
-- 'value', 'updateInstanceAttribute_value' - The value for the attribute. Maximum character limit is 100.
newUpdateInstanceAttribute ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'attributeType'
  InstanceAttributeType ->
  -- | 'value'
  Prelude.Text ->
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

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
updateInstanceAttribute_instanceId :: Lens.Lens' UpdateInstanceAttribute Prelude.Text
updateInstanceAttribute_instanceId = Lens.lens (\UpdateInstanceAttribute' {instanceId} -> instanceId) (\s@UpdateInstanceAttribute' {} a -> s {instanceId = a} :: UpdateInstanceAttribute)

-- | The type of attribute.
--
-- Only allowlisted customers can consume USE_CUSTOM_TTS_VOICES. To access
-- this feature, contact Amazon Web Services Support for allowlisting.
updateInstanceAttribute_attributeType :: Lens.Lens' UpdateInstanceAttribute InstanceAttributeType
updateInstanceAttribute_attributeType = Lens.lens (\UpdateInstanceAttribute' {attributeType} -> attributeType) (\s@UpdateInstanceAttribute' {} a -> s {attributeType = a} :: UpdateInstanceAttribute)

-- | The value for the attribute. Maximum character limit is 100.
updateInstanceAttribute_value :: Lens.Lens' UpdateInstanceAttribute Prelude.Text
updateInstanceAttribute_value = Lens.lens (\UpdateInstanceAttribute' {value} -> value) (\s@UpdateInstanceAttribute' {} a -> s {value = a} :: UpdateInstanceAttribute)

instance Core.AWSRequest UpdateInstanceAttribute where
  type
    AWSResponse UpdateInstanceAttribute =
      UpdateInstanceAttributeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      UpdateInstanceAttributeResponse'

instance Prelude.Hashable UpdateInstanceAttribute where
  hashWithSalt _salt UpdateInstanceAttribute' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` attributeType
      `Prelude.hashWithSalt` value

instance Prelude.NFData UpdateInstanceAttribute where
  rnf UpdateInstanceAttribute' {..} =
    Prelude.rnf instanceId `Prelude.seq`
      Prelude.rnf attributeType `Prelude.seq`
        Prelude.rnf value

instance Data.ToHeaders UpdateInstanceAttribute where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateInstanceAttribute where
  toJSON UpdateInstanceAttribute' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Value" Data..= value)]
      )

instance Data.ToPath UpdateInstanceAttribute where
  toPath UpdateInstanceAttribute' {..} =
    Prelude.mconcat
      [ "/instance/",
        Data.toBS instanceId,
        "/attribute/",
        Data.toBS attributeType
      ]

instance Data.ToQuery UpdateInstanceAttribute where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateInstanceAttributeResponse' smart constructor.
data UpdateInstanceAttributeResponse = UpdateInstanceAttributeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateInstanceAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateInstanceAttributeResponse ::
  UpdateInstanceAttributeResponse
newUpdateInstanceAttributeResponse =
  UpdateInstanceAttributeResponse'

instance
  Prelude.NFData
    UpdateInstanceAttributeResponse
  where
  rnf _ = ()
