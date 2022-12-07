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
-- Module      : Amazonka.NetworkManager.UpdateNetworkResourceMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the resource metadata for the specified global network.
module Amazonka.NetworkManager.UpdateNetworkResourceMetadata
  ( -- * Creating a Request
    UpdateNetworkResourceMetadata (..),
    newUpdateNetworkResourceMetadata,

    -- * Request Lenses
    updateNetworkResourceMetadata_globalNetworkId,
    updateNetworkResourceMetadata_resourceArn,
    updateNetworkResourceMetadata_metadata,

    -- * Destructuring the Response
    UpdateNetworkResourceMetadataResponse (..),
    newUpdateNetworkResourceMetadataResponse,

    -- * Response Lenses
    updateNetworkResourceMetadataResponse_metadata,
    updateNetworkResourceMetadataResponse_resourceArn,
    updateNetworkResourceMetadataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateNetworkResourceMetadata' smart constructor.
data UpdateNetworkResourceMetadata = UpdateNetworkResourceMetadata'
  { -- | The ID of the global network.
    globalNetworkId :: Prelude.Text,
    -- | The ARN of the resource.
    resourceArn :: Prelude.Text,
    -- | The resource metadata.
    metadata :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNetworkResourceMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalNetworkId', 'updateNetworkResourceMetadata_globalNetworkId' - The ID of the global network.
--
-- 'resourceArn', 'updateNetworkResourceMetadata_resourceArn' - The ARN of the resource.
--
-- 'metadata', 'updateNetworkResourceMetadata_metadata' - The resource metadata.
newUpdateNetworkResourceMetadata ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  -- | 'resourceArn'
  Prelude.Text ->
  UpdateNetworkResourceMetadata
newUpdateNetworkResourceMetadata
  pGlobalNetworkId_
  pResourceArn_ =
    UpdateNetworkResourceMetadata'
      { globalNetworkId =
          pGlobalNetworkId_,
        resourceArn = pResourceArn_,
        metadata = Prelude.mempty
      }

-- | The ID of the global network.
updateNetworkResourceMetadata_globalNetworkId :: Lens.Lens' UpdateNetworkResourceMetadata Prelude.Text
updateNetworkResourceMetadata_globalNetworkId = Lens.lens (\UpdateNetworkResourceMetadata' {globalNetworkId} -> globalNetworkId) (\s@UpdateNetworkResourceMetadata' {} a -> s {globalNetworkId = a} :: UpdateNetworkResourceMetadata)

-- | The ARN of the resource.
updateNetworkResourceMetadata_resourceArn :: Lens.Lens' UpdateNetworkResourceMetadata Prelude.Text
updateNetworkResourceMetadata_resourceArn = Lens.lens (\UpdateNetworkResourceMetadata' {resourceArn} -> resourceArn) (\s@UpdateNetworkResourceMetadata' {} a -> s {resourceArn = a} :: UpdateNetworkResourceMetadata)

-- | The resource metadata.
updateNetworkResourceMetadata_metadata :: Lens.Lens' UpdateNetworkResourceMetadata (Prelude.HashMap Prelude.Text Prelude.Text)
updateNetworkResourceMetadata_metadata = Lens.lens (\UpdateNetworkResourceMetadata' {metadata} -> metadata) (\s@UpdateNetworkResourceMetadata' {} a -> s {metadata = a} :: UpdateNetworkResourceMetadata) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    UpdateNetworkResourceMetadata
  where
  type
    AWSResponse UpdateNetworkResourceMetadata =
      UpdateNetworkResourceMetadataResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateNetworkResourceMetadataResponse'
            Prelude.<$> (x Data..?> "Metadata" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "ResourceArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateNetworkResourceMetadata
  where
  hashWithSalt _salt UpdateNetworkResourceMetadata' {..} =
    _salt `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` metadata

instance Prelude.NFData UpdateNetworkResourceMetadata where
  rnf UpdateNetworkResourceMetadata' {..} =
    Prelude.rnf globalNetworkId
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf metadata

instance Data.ToHeaders UpdateNetworkResourceMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateNetworkResourceMetadata where
  toJSON UpdateNetworkResourceMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Metadata" Data..= metadata)]
      )

instance Data.ToPath UpdateNetworkResourceMetadata where
  toPath UpdateNetworkResourceMetadata' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Data.toBS globalNetworkId,
        "/network-resources/",
        Data.toBS resourceArn,
        "/metadata"
      ]

instance Data.ToQuery UpdateNetworkResourceMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateNetworkResourceMetadataResponse' smart constructor.
data UpdateNetworkResourceMetadataResponse = UpdateNetworkResourceMetadataResponse'
  { -- | The updated resource metadata.
    metadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ARN of the resource.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNetworkResourceMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadata', 'updateNetworkResourceMetadataResponse_metadata' - The updated resource metadata.
--
-- 'resourceArn', 'updateNetworkResourceMetadataResponse_resourceArn' - The ARN of the resource.
--
-- 'httpStatus', 'updateNetworkResourceMetadataResponse_httpStatus' - The response's http status code.
newUpdateNetworkResourceMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateNetworkResourceMetadataResponse
newUpdateNetworkResourceMetadataResponse pHttpStatus_ =
  UpdateNetworkResourceMetadataResponse'
    { metadata =
        Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated resource metadata.
updateNetworkResourceMetadataResponse_metadata :: Lens.Lens' UpdateNetworkResourceMetadataResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateNetworkResourceMetadataResponse_metadata = Lens.lens (\UpdateNetworkResourceMetadataResponse' {metadata} -> metadata) (\s@UpdateNetworkResourceMetadataResponse' {} a -> s {metadata = a} :: UpdateNetworkResourceMetadataResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the resource.
updateNetworkResourceMetadataResponse_resourceArn :: Lens.Lens' UpdateNetworkResourceMetadataResponse (Prelude.Maybe Prelude.Text)
updateNetworkResourceMetadataResponse_resourceArn = Lens.lens (\UpdateNetworkResourceMetadataResponse' {resourceArn} -> resourceArn) (\s@UpdateNetworkResourceMetadataResponse' {} a -> s {resourceArn = a} :: UpdateNetworkResourceMetadataResponse)

-- | The response's http status code.
updateNetworkResourceMetadataResponse_httpStatus :: Lens.Lens' UpdateNetworkResourceMetadataResponse Prelude.Int
updateNetworkResourceMetadataResponse_httpStatus = Lens.lens (\UpdateNetworkResourceMetadataResponse' {httpStatus} -> httpStatus) (\s@UpdateNetworkResourceMetadataResponse' {} a -> s {httpStatus = a} :: UpdateNetworkResourceMetadataResponse)

instance
  Prelude.NFData
    UpdateNetworkResourceMetadataResponse
  where
  rnf UpdateNetworkResourceMetadataResponse' {..} =
    Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf httpStatus
