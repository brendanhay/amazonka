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
-- Module      : Amazonka.IoTSiteWise.DisassociateAssets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a child asset from the given parent asset through a
-- hierarchy defined in the parent asset\'s model.
module Amazonka.IoTSiteWise.DisassociateAssets
  ( -- * Creating a Request
    DisassociateAssets (..),
    newDisassociateAssets,

    -- * Request Lenses
    disassociateAssets_clientToken,
    disassociateAssets_assetId,
    disassociateAssets_hierarchyId,
    disassociateAssets_childAssetId,

    -- * Destructuring the Response
    DisassociateAssetsResponse (..),
    newDisassociateAssetsResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateAssets' smart constructor.
data DisassociateAssets = DisassociateAssets'
  { -- | A unique case-sensitive identifier that you can provide to ensure the
    -- idempotency of the request. Don\'t reuse this client token if a new
    -- idempotent request is required.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the parent asset from which to disassociate the child asset.
    assetId :: Prelude.Text,
    -- | The ID of a hierarchy in the parent asset\'s model. Hierarchies allow
    -- different groupings of assets to be formed that all come from the same
    -- asset model. You can use the hierarchy ID to identify the correct asset
    -- to disassociate. For more information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/asset-hierarchies.html Asset hierarchies>
    -- in the /IoT SiteWise User Guide/.
    hierarchyId :: Prelude.Text,
    -- | The ID of the child asset to disassociate.
    childAssetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateAssets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'disassociateAssets_clientToken' - A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
--
-- 'assetId', 'disassociateAssets_assetId' - The ID of the parent asset from which to disassociate the child asset.
--
-- 'hierarchyId', 'disassociateAssets_hierarchyId' - The ID of a hierarchy in the parent asset\'s model. Hierarchies allow
-- different groupings of assets to be formed that all come from the same
-- asset model. You can use the hierarchy ID to identify the correct asset
-- to disassociate. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/asset-hierarchies.html Asset hierarchies>
-- in the /IoT SiteWise User Guide/.
--
-- 'childAssetId', 'disassociateAssets_childAssetId' - The ID of the child asset to disassociate.
newDisassociateAssets ::
  -- | 'assetId'
  Prelude.Text ->
  -- | 'hierarchyId'
  Prelude.Text ->
  -- | 'childAssetId'
  Prelude.Text ->
  DisassociateAssets
newDisassociateAssets
  pAssetId_
  pHierarchyId_
  pChildAssetId_ =
    DisassociateAssets'
      { clientToken = Prelude.Nothing,
        assetId = pAssetId_,
        hierarchyId = pHierarchyId_,
        childAssetId = pChildAssetId_
      }

-- | A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
disassociateAssets_clientToken :: Lens.Lens' DisassociateAssets (Prelude.Maybe Prelude.Text)
disassociateAssets_clientToken = Lens.lens (\DisassociateAssets' {clientToken} -> clientToken) (\s@DisassociateAssets' {} a -> s {clientToken = a} :: DisassociateAssets)

-- | The ID of the parent asset from which to disassociate the child asset.
disassociateAssets_assetId :: Lens.Lens' DisassociateAssets Prelude.Text
disassociateAssets_assetId = Lens.lens (\DisassociateAssets' {assetId} -> assetId) (\s@DisassociateAssets' {} a -> s {assetId = a} :: DisassociateAssets)

-- | The ID of a hierarchy in the parent asset\'s model. Hierarchies allow
-- different groupings of assets to be formed that all come from the same
-- asset model. You can use the hierarchy ID to identify the correct asset
-- to disassociate. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/asset-hierarchies.html Asset hierarchies>
-- in the /IoT SiteWise User Guide/.
disassociateAssets_hierarchyId :: Lens.Lens' DisassociateAssets Prelude.Text
disassociateAssets_hierarchyId = Lens.lens (\DisassociateAssets' {hierarchyId} -> hierarchyId) (\s@DisassociateAssets' {} a -> s {hierarchyId = a} :: DisassociateAssets)

-- | The ID of the child asset to disassociate.
disassociateAssets_childAssetId :: Lens.Lens' DisassociateAssets Prelude.Text
disassociateAssets_childAssetId = Lens.lens (\DisassociateAssets' {childAssetId} -> childAssetId) (\s@DisassociateAssets' {} a -> s {childAssetId = a} :: DisassociateAssets)

instance Core.AWSRequest DisassociateAssets where
  type
    AWSResponse DisassociateAssets =
      DisassociateAssetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DisassociateAssetsResponse'

instance Prelude.Hashable DisassociateAssets where
  hashWithSalt _salt DisassociateAssets' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` assetId
      `Prelude.hashWithSalt` hierarchyId
      `Prelude.hashWithSalt` childAssetId

instance Prelude.NFData DisassociateAssets where
  rnf DisassociateAssets' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf assetId
      `Prelude.seq` Prelude.rnf hierarchyId
      `Prelude.seq` Prelude.rnf childAssetId

instance Data.ToHeaders DisassociateAssets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateAssets where
  toJSON DisassociateAssets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("hierarchyId" Data..= hierarchyId),
            Prelude.Just ("childAssetId" Data..= childAssetId)
          ]
      )

instance Data.ToPath DisassociateAssets where
  toPath DisassociateAssets' {..} =
    Prelude.mconcat
      ["/assets/", Data.toBS assetId, "/disassociate"]

instance Data.ToQuery DisassociateAssets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateAssetsResponse' smart constructor.
data DisassociateAssetsResponse = DisassociateAssetsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateAssetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateAssetsResponse ::
  DisassociateAssetsResponse
newDisassociateAssetsResponse =
  DisassociateAssetsResponse'

instance Prelude.NFData DisassociateAssetsResponse where
  rnf _ = ()
