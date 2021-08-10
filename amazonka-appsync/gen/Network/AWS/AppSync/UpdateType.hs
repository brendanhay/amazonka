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
-- Module      : Network.AWS.AppSync.UpdateType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a @Type@ object.
module Network.AWS.AppSync.UpdateType
  ( -- * Creating a Request
    UpdateType (..),
    newUpdateType,

    -- * Request Lenses
    updateType_definition,
    updateType_apiId,
    updateType_typeName,
    updateType_format,

    -- * Destructuring the Response
    UpdateTypeResponse (..),
    newUpdateTypeResponse,

    -- * Response Lenses
    updateTypeResponse_type,
    updateTypeResponse_httpStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateType' smart constructor.
data UpdateType = UpdateType'
  { -- | The new definition.
    definition :: Prelude.Maybe Prelude.Text,
    -- | The API ID.
    apiId :: Prelude.Text,
    -- | The new type name.
    typeName :: Prelude.Text,
    -- | The new type format: SDL or JSON.
    format :: TypeDefinitionFormat
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'definition', 'updateType_definition' - The new definition.
--
-- 'apiId', 'updateType_apiId' - The API ID.
--
-- 'typeName', 'updateType_typeName' - The new type name.
--
-- 'format', 'updateType_format' - The new type format: SDL or JSON.
newUpdateType ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'typeName'
  Prelude.Text ->
  -- | 'format'
  TypeDefinitionFormat ->
  UpdateType
newUpdateType pApiId_ pTypeName_ pFormat_ =
  UpdateType'
    { definition = Prelude.Nothing,
      apiId = pApiId_,
      typeName = pTypeName_,
      format = pFormat_
    }

-- | The new definition.
updateType_definition :: Lens.Lens' UpdateType (Prelude.Maybe Prelude.Text)
updateType_definition = Lens.lens (\UpdateType' {definition} -> definition) (\s@UpdateType' {} a -> s {definition = a} :: UpdateType)

-- | The API ID.
updateType_apiId :: Lens.Lens' UpdateType Prelude.Text
updateType_apiId = Lens.lens (\UpdateType' {apiId} -> apiId) (\s@UpdateType' {} a -> s {apiId = a} :: UpdateType)

-- | The new type name.
updateType_typeName :: Lens.Lens' UpdateType Prelude.Text
updateType_typeName = Lens.lens (\UpdateType' {typeName} -> typeName) (\s@UpdateType' {} a -> s {typeName = a} :: UpdateType)

-- | The new type format: SDL or JSON.
updateType_format :: Lens.Lens' UpdateType TypeDefinitionFormat
updateType_format = Lens.lens (\UpdateType' {format} -> format) (\s@UpdateType' {} a -> s {format = a} :: UpdateType)

instance Core.AWSRequest UpdateType where
  type AWSResponse UpdateType = UpdateTypeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTypeResponse'
            Prelude.<$> (x Core..?> "type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateType

instance Prelude.NFData UpdateType

instance Core.ToHeaders UpdateType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateType where
  toJSON UpdateType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("definition" Core..=) Prelude.<$> definition,
            Prelude.Just ("format" Core..= format)
          ]
      )

instance Core.ToPath UpdateType where
  toPath UpdateType' {..} =
    Prelude.mconcat
      [ "/v1/apis/",
        Core.toBS apiId,
        "/types/",
        Core.toBS typeName
      ]

instance Core.ToQuery UpdateType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTypeResponse' smart constructor.
data UpdateTypeResponse = UpdateTypeResponse'
  { -- | The updated @Type@ object.
    type' :: Prelude.Maybe Type,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'updateTypeResponse_type' - The updated @Type@ object.
--
-- 'httpStatus', 'updateTypeResponse_httpStatus' - The response's http status code.
newUpdateTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateTypeResponse
newUpdateTypeResponse pHttpStatus_ =
  UpdateTypeResponse'
    { type' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated @Type@ object.
updateTypeResponse_type :: Lens.Lens' UpdateTypeResponse (Prelude.Maybe Type)
updateTypeResponse_type = Lens.lens (\UpdateTypeResponse' {type'} -> type') (\s@UpdateTypeResponse' {} a -> s {type' = a} :: UpdateTypeResponse)

-- | The response's http status code.
updateTypeResponse_httpStatus :: Lens.Lens' UpdateTypeResponse Prelude.Int
updateTypeResponse_httpStatus = Lens.lens (\UpdateTypeResponse' {httpStatus} -> httpStatus) (\s@UpdateTypeResponse' {} a -> s {httpStatus = a} :: UpdateTypeResponse)

instance Prelude.NFData UpdateTypeResponse
