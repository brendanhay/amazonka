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
-- Module      : Amazonka.IoTWireless.UpdatePosition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the position information of a resource.
module Amazonka.IoTWireless.UpdatePosition
  ( -- * Creating a Request
    UpdatePosition (..),
    newUpdatePosition,

    -- * Request Lenses
    updatePosition_resourceIdentifier,
    updatePosition_resourceType,
    updatePosition_position,

    -- * Destructuring the Response
    UpdatePositionResponse (..),
    newUpdatePositionResponse,

    -- * Response Lenses
    updatePositionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdatePosition' smart constructor.
data UpdatePosition = UpdatePosition'
  { -- | Resource identifier of the resource for which position is updated.
    resourceIdentifier :: Prelude.Text,
    -- | Resource type of the resource for which position is updated.
    resourceType :: PositionResourceType,
    -- | The position information of the resource.
    position :: [Prelude.Double]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePosition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceIdentifier', 'updatePosition_resourceIdentifier' - Resource identifier of the resource for which position is updated.
--
-- 'resourceType', 'updatePosition_resourceType' - Resource type of the resource for which position is updated.
--
-- 'position', 'updatePosition_position' - The position information of the resource.
newUpdatePosition ::
  -- | 'resourceIdentifier'
  Prelude.Text ->
  -- | 'resourceType'
  PositionResourceType ->
  UpdatePosition
newUpdatePosition pResourceIdentifier_ pResourceType_ =
  UpdatePosition'
    { resourceIdentifier =
        pResourceIdentifier_,
      resourceType = pResourceType_,
      position = Prelude.mempty
    }

-- | Resource identifier of the resource for which position is updated.
updatePosition_resourceIdentifier :: Lens.Lens' UpdatePosition Prelude.Text
updatePosition_resourceIdentifier = Lens.lens (\UpdatePosition' {resourceIdentifier} -> resourceIdentifier) (\s@UpdatePosition' {} a -> s {resourceIdentifier = a} :: UpdatePosition)

-- | Resource type of the resource for which position is updated.
updatePosition_resourceType :: Lens.Lens' UpdatePosition PositionResourceType
updatePosition_resourceType = Lens.lens (\UpdatePosition' {resourceType} -> resourceType) (\s@UpdatePosition' {} a -> s {resourceType = a} :: UpdatePosition)

-- | The position information of the resource.
updatePosition_position :: Lens.Lens' UpdatePosition [Prelude.Double]
updatePosition_position = Lens.lens (\UpdatePosition' {position} -> position) (\s@UpdatePosition' {} a -> s {position = a} :: UpdatePosition) Prelude.. Lens.coerced

instance Core.AWSRequest UpdatePosition where
  type
    AWSResponse UpdatePosition =
      UpdatePositionResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdatePositionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePosition where
  hashWithSalt _salt UpdatePosition' {..} =
    _salt `Prelude.hashWithSalt` resourceIdentifier
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` position

instance Prelude.NFData UpdatePosition where
  rnf UpdatePosition' {..} =
    Prelude.rnf resourceIdentifier
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf position

instance Data.ToHeaders UpdatePosition where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdatePosition where
  toJSON UpdatePosition' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Position" Data..= position)]
      )

instance Data.ToPath UpdatePosition where
  toPath UpdatePosition' {..} =
    Prelude.mconcat
      ["/positions/", Data.toBS resourceIdentifier]

instance Data.ToQuery UpdatePosition where
  toQuery UpdatePosition' {..} =
    Prelude.mconcat
      ["resourceType" Data.=: resourceType]

-- | /See:/ 'newUpdatePositionResponse' smart constructor.
data UpdatePositionResponse = UpdatePositionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePositionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updatePositionResponse_httpStatus' - The response's http status code.
newUpdatePositionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdatePositionResponse
newUpdatePositionResponse pHttpStatus_ =
  UpdatePositionResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updatePositionResponse_httpStatus :: Lens.Lens' UpdatePositionResponse Prelude.Int
updatePositionResponse_httpStatus = Lens.lens (\UpdatePositionResponse' {httpStatus} -> httpStatus) (\s@UpdatePositionResponse' {} a -> s {httpStatus = a} :: UpdatePositionResponse)

instance Prelude.NFData UpdatePositionResponse where
  rnf UpdatePositionResponse' {..} =
    Prelude.rnf httpStatus
