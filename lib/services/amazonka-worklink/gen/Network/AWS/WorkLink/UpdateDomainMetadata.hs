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
-- Module      : Network.AWS.WorkLink.UpdateDomainMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates domain metadata, such as DisplayName.
module Network.AWS.WorkLink.UpdateDomainMetadata
  ( -- * Creating a Request
    UpdateDomainMetadata (..),
    newUpdateDomainMetadata,

    -- * Request Lenses
    updateDomainMetadata_displayName,
    updateDomainMetadata_fleetArn,
    updateDomainMetadata_domainName,

    -- * Destructuring the Response
    UpdateDomainMetadataResponse (..),
    newUpdateDomainMetadataResponse,

    -- * Response Lenses
    updateDomainMetadataResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkLink.Types

-- | /See:/ 'newUpdateDomainMetadata' smart constructor.
data UpdateDomainMetadata = UpdateDomainMetadata'
  { -- | The name to display.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the fleet.
    fleetArn :: Prelude.Text,
    -- | The name of the domain.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDomainMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayName', 'updateDomainMetadata_displayName' - The name to display.
--
-- 'fleetArn', 'updateDomainMetadata_fleetArn' - The ARN of the fleet.
--
-- 'domainName', 'updateDomainMetadata_domainName' - The name of the domain.
newUpdateDomainMetadata ::
  -- | 'fleetArn'
  Prelude.Text ->
  -- | 'domainName'
  Prelude.Text ->
  UpdateDomainMetadata
newUpdateDomainMetadata pFleetArn_ pDomainName_ =
  UpdateDomainMetadata'
    { displayName =
        Prelude.Nothing,
      fleetArn = pFleetArn_,
      domainName = pDomainName_
    }

-- | The name to display.
updateDomainMetadata_displayName :: Lens.Lens' UpdateDomainMetadata (Prelude.Maybe Prelude.Text)
updateDomainMetadata_displayName = Lens.lens (\UpdateDomainMetadata' {displayName} -> displayName) (\s@UpdateDomainMetadata' {} a -> s {displayName = a} :: UpdateDomainMetadata)

-- | The ARN of the fleet.
updateDomainMetadata_fleetArn :: Lens.Lens' UpdateDomainMetadata Prelude.Text
updateDomainMetadata_fleetArn = Lens.lens (\UpdateDomainMetadata' {fleetArn} -> fleetArn) (\s@UpdateDomainMetadata' {} a -> s {fleetArn = a} :: UpdateDomainMetadata)

-- | The name of the domain.
updateDomainMetadata_domainName :: Lens.Lens' UpdateDomainMetadata Prelude.Text
updateDomainMetadata_domainName = Lens.lens (\UpdateDomainMetadata' {domainName} -> domainName) (\s@UpdateDomainMetadata' {} a -> s {domainName = a} :: UpdateDomainMetadata)

instance Core.AWSRequest UpdateDomainMetadata where
  type
    AWSResponse UpdateDomainMetadata =
      UpdateDomainMetadataResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDomainMetadataResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDomainMetadata

instance Prelude.NFData UpdateDomainMetadata

instance Core.ToHeaders UpdateDomainMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateDomainMetadata where
  toJSON UpdateDomainMetadata' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DisplayName" Core..=) Prelude.<$> displayName,
            Prelude.Just ("FleetArn" Core..= fleetArn),
            Prelude.Just ("DomainName" Core..= domainName)
          ]
      )

instance Core.ToPath UpdateDomainMetadata where
  toPath = Prelude.const "/updateDomainMetadata"

instance Core.ToQuery UpdateDomainMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDomainMetadataResponse' smart constructor.
data UpdateDomainMetadataResponse = UpdateDomainMetadataResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDomainMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDomainMetadataResponse_httpStatus' - The response's http status code.
newUpdateDomainMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDomainMetadataResponse
newUpdateDomainMetadataResponse pHttpStatus_ =
  UpdateDomainMetadataResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateDomainMetadataResponse_httpStatus :: Lens.Lens' UpdateDomainMetadataResponse Prelude.Int
updateDomainMetadataResponse_httpStatus = Lens.lens (\UpdateDomainMetadataResponse' {httpStatus} -> httpStatus) (\s@UpdateDomainMetadataResponse' {} a -> s {httpStatus = a} :: UpdateDomainMetadataResponse)

instance Prelude.NFData UpdateDomainMetadataResponse
