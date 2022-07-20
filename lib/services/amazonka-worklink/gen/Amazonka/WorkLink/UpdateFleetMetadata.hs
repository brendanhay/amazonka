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
-- Module      : Amazonka.WorkLink.UpdateFleetMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates fleet metadata, such as DisplayName.
module Amazonka.WorkLink.UpdateFleetMetadata
  ( -- * Creating a Request
    UpdateFleetMetadata (..),
    newUpdateFleetMetadata,

    -- * Request Lenses
    updateFleetMetadata_displayName,
    updateFleetMetadata_optimizeForEndUserLocation,
    updateFleetMetadata_fleetArn,

    -- * Destructuring the Response
    UpdateFleetMetadataResponse (..),
    newUpdateFleetMetadataResponse,

    -- * Response Lenses
    updateFleetMetadataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkLink.Types

-- | /See:/ 'newUpdateFleetMetadata' smart constructor.
data UpdateFleetMetadata = UpdateFleetMetadata'
  { -- | The fleet name to display. The existing DisplayName is unset if null is
    -- passed.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The option to optimize for better performance by routing traffic through
    -- the closest AWS Region to users, which may be outside of your home
    -- Region.
    optimizeForEndUserLocation :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the fleet.
    fleetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFleetMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayName', 'updateFleetMetadata_displayName' - The fleet name to display. The existing DisplayName is unset if null is
-- passed.
--
-- 'optimizeForEndUserLocation', 'updateFleetMetadata_optimizeForEndUserLocation' - The option to optimize for better performance by routing traffic through
-- the closest AWS Region to users, which may be outside of your home
-- Region.
--
-- 'fleetArn', 'updateFleetMetadata_fleetArn' - The ARN of the fleet.
newUpdateFleetMetadata ::
  -- | 'fleetArn'
  Prelude.Text ->
  UpdateFleetMetadata
newUpdateFleetMetadata pFleetArn_ =
  UpdateFleetMetadata'
    { displayName = Prelude.Nothing,
      optimizeForEndUserLocation = Prelude.Nothing,
      fleetArn = pFleetArn_
    }

-- | The fleet name to display. The existing DisplayName is unset if null is
-- passed.
updateFleetMetadata_displayName :: Lens.Lens' UpdateFleetMetadata (Prelude.Maybe Prelude.Text)
updateFleetMetadata_displayName = Lens.lens (\UpdateFleetMetadata' {displayName} -> displayName) (\s@UpdateFleetMetadata' {} a -> s {displayName = a} :: UpdateFleetMetadata)

-- | The option to optimize for better performance by routing traffic through
-- the closest AWS Region to users, which may be outside of your home
-- Region.
updateFleetMetadata_optimizeForEndUserLocation :: Lens.Lens' UpdateFleetMetadata (Prelude.Maybe Prelude.Bool)
updateFleetMetadata_optimizeForEndUserLocation = Lens.lens (\UpdateFleetMetadata' {optimizeForEndUserLocation} -> optimizeForEndUserLocation) (\s@UpdateFleetMetadata' {} a -> s {optimizeForEndUserLocation = a} :: UpdateFleetMetadata)

-- | The ARN of the fleet.
updateFleetMetadata_fleetArn :: Lens.Lens' UpdateFleetMetadata Prelude.Text
updateFleetMetadata_fleetArn = Lens.lens (\UpdateFleetMetadata' {fleetArn} -> fleetArn) (\s@UpdateFleetMetadata' {} a -> s {fleetArn = a} :: UpdateFleetMetadata)

instance Core.AWSRequest UpdateFleetMetadata where
  type
    AWSResponse UpdateFleetMetadata =
      UpdateFleetMetadataResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateFleetMetadataResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFleetMetadata where
  hashWithSalt _salt UpdateFleetMetadata' {..} =
    _salt `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` optimizeForEndUserLocation
      `Prelude.hashWithSalt` fleetArn

instance Prelude.NFData UpdateFleetMetadata where
  rnf UpdateFleetMetadata' {..} =
    Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf optimizeForEndUserLocation
      `Prelude.seq` Prelude.rnf fleetArn

instance Core.ToHeaders UpdateFleetMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateFleetMetadata where
  toJSON UpdateFleetMetadata' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DisplayName" Core..=) Prelude.<$> displayName,
            ("OptimizeForEndUserLocation" Core..=)
              Prelude.<$> optimizeForEndUserLocation,
            Prelude.Just ("FleetArn" Core..= fleetArn)
          ]
      )

instance Core.ToPath UpdateFleetMetadata where
  toPath = Prelude.const "/UpdateFleetMetadata"

instance Core.ToQuery UpdateFleetMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFleetMetadataResponse' smart constructor.
data UpdateFleetMetadataResponse = UpdateFleetMetadataResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFleetMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateFleetMetadataResponse_httpStatus' - The response's http status code.
newUpdateFleetMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFleetMetadataResponse
newUpdateFleetMetadataResponse pHttpStatus_ =
  UpdateFleetMetadataResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateFleetMetadataResponse_httpStatus :: Lens.Lens' UpdateFleetMetadataResponse Prelude.Int
updateFleetMetadataResponse_httpStatus = Lens.lens (\UpdateFleetMetadataResponse' {httpStatus} -> httpStatus) (\s@UpdateFleetMetadataResponse' {} a -> s {httpStatus = a} :: UpdateFleetMetadataResponse)

instance Prelude.NFData UpdateFleetMetadataResponse where
  rnf UpdateFleetMetadataResponse' {..} =
    Prelude.rnf httpStatus
