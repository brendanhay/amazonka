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
-- Module      : Amazonka.CloudDirectory.UpdateObjectAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a given object\'s attributes.
module Amazonka.CloudDirectory.UpdateObjectAttributes
  ( -- * Creating a Request
    UpdateObjectAttributes (..),
    newUpdateObjectAttributes,

    -- * Request Lenses
    updateObjectAttributes_directoryArn,
    updateObjectAttributes_objectReference,
    updateObjectAttributes_attributeUpdates,

    -- * Destructuring the Response
    UpdateObjectAttributesResponse (..),
    newUpdateObjectAttributesResponse,

    -- * Response Lenses
    updateObjectAttributesResponse_objectIdentifier,
    updateObjectAttributesResponse_httpStatus,
  )
where

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateObjectAttributes' smart constructor.
data UpdateObjectAttributes = UpdateObjectAttributes'
  { -- | The Amazon Resource Name (ARN) that is associated with the Directory
    -- where the object resides. For more information, see arns.
    directoryArn :: Prelude.Text,
    -- | The reference that identifies the object.
    objectReference :: ObjectReference,
    -- | The attributes update structure.
    attributeUpdates :: [ObjectAttributeUpdate]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateObjectAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryArn', 'updateObjectAttributes_directoryArn' - The Amazon Resource Name (ARN) that is associated with the Directory
-- where the object resides. For more information, see arns.
--
-- 'objectReference', 'updateObjectAttributes_objectReference' - The reference that identifies the object.
--
-- 'attributeUpdates', 'updateObjectAttributes_attributeUpdates' - The attributes update structure.
newUpdateObjectAttributes ::
  -- | 'directoryArn'
  Prelude.Text ->
  -- | 'objectReference'
  ObjectReference ->
  UpdateObjectAttributes
newUpdateObjectAttributes
  pDirectoryArn_
  pObjectReference_ =
    UpdateObjectAttributes'
      { directoryArn =
          pDirectoryArn_,
        objectReference = pObjectReference_,
        attributeUpdates = Prelude.mempty
      }

-- | The Amazon Resource Name (ARN) that is associated with the Directory
-- where the object resides. For more information, see arns.
updateObjectAttributes_directoryArn :: Lens.Lens' UpdateObjectAttributes Prelude.Text
updateObjectAttributes_directoryArn = Lens.lens (\UpdateObjectAttributes' {directoryArn} -> directoryArn) (\s@UpdateObjectAttributes' {} a -> s {directoryArn = a} :: UpdateObjectAttributes)

-- | The reference that identifies the object.
updateObjectAttributes_objectReference :: Lens.Lens' UpdateObjectAttributes ObjectReference
updateObjectAttributes_objectReference = Lens.lens (\UpdateObjectAttributes' {objectReference} -> objectReference) (\s@UpdateObjectAttributes' {} a -> s {objectReference = a} :: UpdateObjectAttributes)

-- | The attributes update structure.
updateObjectAttributes_attributeUpdates :: Lens.Lens' UpdateObjectAttributes [ObjectAttributeUpdate]
updateObjectAttributes_attributeUpdates = Lens.lens (\UpdateObjectAttributes' {attributeUpdates} -> attributeUpdates) (\s@UpdateObjectAttributes' {} a -> s {attributeUpdates = a} :: UpdateObjectAttributes) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateObjectAttributes where
  type
    AWSResponse UpdateObjectAttributes =
      UpdateObjectAttributesResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateObjectAttributesResponse'
            Prelude.<$> (x Core..?> "ObjectIdentifier")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateObjectAttributes where
  hashWithSalt _salt UpdateObjectAttributes' {..} =
    _salt `Prelude.hashWithSalt` directoryArn
      `Prelude.hashWithSalt` objectReference
      `Prelude.hashWithSalt` attributeUpdates

instance Prelude.NFData UpdateObjectAttributes where
  rnf UpdateObjectAttributes' {..} =
    Prelude.rnf directoryArn
      `Prelude.seq` Prelude.rnf objectReference
      `Prelude.seq` Prelude.rnf attributeUpdates

instance Core.ToHeaders UpdateObjectAttributes where
  toHeaders UpdateObjectAttributes' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Core.=# directoryArn]

instance Core.ToJSON UpdateObjectAttributes where
  toJSON UpdateObjectAttributes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ObjectReference" Core..= objectReference),
            Prelude.Just
              ("AttributeUpdates" Core..= attributeUpdates)
          ]
      )

instance Core.ToPath UpdateObjectAttributes where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/object/update"

instance Core.ToQuery UpdateObjectAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateObjectAttributesResponse' smart constructor.
data UpdateObjectAttributesResponse = UpdateObjectAttributesResponse'
  { -- | The @ObjectIdentifier@ of the updated object.
    objectIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateObjectAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectIdentifier', 'updateObjectAttributesResponse_objectIdentifier' - The @ObjectIdentifier@ of the updated object.
--
-- 'httpStatus', 'updateObjectAttributesResponse_httpStatus' - The response's http status code.
newUpdateObjectAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateObjectAttributesResponse
newUpdateObjectAttributesResponse pHttpStatus_ =
  UpdateObjectAttributesResponse'
    { objectIdentifier =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ObjectIdentifier@ of the updated object.
updateObjectAttributesResponse_objectIdentifier :: Lens.Lens' UpdateObjectAttributesResponse (Prelude.Maybe Prelude.Text)
updateObjectAttributesResponse_objectIdentifier = Lens.lens (\UpdateObjectAttributesResponse' {objectIdentifier} -> objectIdentifier) (\s@UpdateObjectAttributesResponse' {} a -> s {objectIdentifier = a} :: UpdateObjectAttributesResponse)

-- | The response's http status code.
updateObjectAttributesResponse_httpStatus :: Lens.Lens' UpdateObjectAttributesResponse Prelude.Int
updateObjectAttributesResponse_httpStatus = Lens.lens (\UpdateObjectAttributesResponse' {httpStatus} -> httpStatus) (\s@UpdateObjectAttributesResponse' {} a -> s {httpStatus = a} :: UpdateObjectAttributesResponse)

instance
  Prelude.NFData
    UpdateObjectAttributesResponse
  where
  rnf UpdateObjectAttributesResponse' {..} =
    Prelude.rnf objectIdentifier
      `Prelude.seq` Prelude.rnf httpStatus
