{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudDirectory.UpdateObjectAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a given object\'s attributes.
module Network.AWS.CloudDirectory.UpdateObjectAttributes
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

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
updateObjectAttributes_attributeUpdates = Lens.lens (\UpdateObjectAttributes' {attributeUpdates} -> attributeUpdates) (\s@UpdateObjectAttributes' {} a -> s {attributeUpdates = a} :: UpdateObjectAttributes) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest UpdateObjectAttributes where
  type
    Rs UpdateObjectAttributes =
      UpdateObjectAttributesResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateObjectAttributesResponse'
            Prelude.<$> (x Prelude..?> "ObjectIdentifier")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateObjectAttributes

instance Prelude.NFData UpdateObjectAttributes

instance Prelude.ToHeaders UpdateObjectAttributes where
  toHeaders UpdateObjectAttributes' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Prelude.=# directoryArn]

instance Prelude.ToJSON UpdateObjectAttributes where
  toJSON UpdateObjectAttributes' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ObjectReference" Prelude..= objectReference),
            Prelude.Just
              ("AttributeUpdates" Prelude..= attributeUpdates)
          ]
      )

instance Prelude.ToPath UpdateObjectAttributes where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/object/update"

instance Prelude.ToQuery UpdateObjectAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateObjectAttributesResponse' smart constructor.
data UpdateObjectAttributesResponse = UpdateObjectAttributesResponse'
  { -- | The @ObjectIdentifier@ of the updated object.
    objectIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
