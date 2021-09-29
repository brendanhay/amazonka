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
-- Module      : Network.AWS.DirectConnect.UpdateLag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the attributes of the specified link aggregation group (LAG).
--
-- You can update the following LAG attributes:
--
-- -   The name of the LAG.
--
-- -   The value for the minimum number of connections that must be
--     operational for the LAG itself to be operational.
--
-- -   The LAG\'s MACsec encryption mode.
--
--     Amazon Web Services assigns this value to each connection which is
--     part of the LAG.
--
-- -   The tags
--
-- If you adjust the threshold value for the minimum number of operational
-- connections, ensure that the new value does not cause the LAG to fall
-- below the threshold and become non-operational.
module Network.AWS.DirectConnect.UpdateLag
  ( -- * Creating a Request
    UpdateLag (..),
    newUpdateLag,

    -- * Request Lenses
    updateLag_lagName,
    updateLag_encryptionMode,
    updateLag_minimumLinks,
    updateLag_lagId,

    -- * Destructuring the Response
    Lag (..),
    newLag,

    -- * Response Lenses
    lag_numberOfConnections,
    lag_awsDeviceV2,
    lag_allowsHostedConnections,
    lag_macSecKeys,
    lag_providerName,
    lag_awsLogicalDeviceId,
    lag_hasLogicalRedundancy,
    lag_lagName,
    lag_connections,
    lag_awsDevice,
    lag_lagState,
    lag_jumboFrameCapable,
    lag_connectionsBandwidth,
    lag_lagId,
    lag_encryptionMode,
    lag_tags,
    lag_ownerAccount,
    lag_region,
    lag_location,
    lag_minimumLinks,
    lag_macSecCapable,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateLag' smart constructor.
data UpdateLag = UpdateLag'
  { -- | The name of the LAG.
    lagName :: Prelude.Maybe Prelude.Text,
    -- | The LAG MAC Security (MACsec) encryption mode.
    --
    -- Amazon Web Services applies the value to all connections which are part
    -- of the LAG.
    encryptionMode :: Prelude.Maybe Prelude.Text,
    -- | The minimum number of physical connections that must be operational for
    -- the LAG itself to be operational.
    minimumLinks :: Prelude.Maybe Prelude.Int,
    -- | The ID of the LAG.
    lagId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lagName', 'updateLag_lagName' - The name of the LAG.
--
-- 'encryptionMode', 'updateLag_encryptionMode' - The LAG MAC Security (MACsec) encryption mode.
--
-- Amazon Web Services applies the value to all connections which are part
-- of the LAG.
--
-- 'minimumLinks', 'updateLag_minimumLinks' - The minimum number of physical connections that must be operational for
-- the LAG itself to be operational.
--
-- 'lagId', 'updateLag_lagId' - The ID of the LAG.
newUpdateLag ::
  -- | 'lagId'
  Prelude.Text ->
  UpdateLag
newUpdateLag pLagId_ =
  UpdateLag'
    { lagName = Prelude.Nothing,
      encryptionMode = Prelude.Nothing,
      minimumLinks = Prelude.Nothing,
      lagId = pLagId_
    }

-- | The name of the LAG.
updateLag_lagName :: Lens.Lens' UpdateLag (Prelude.Maybe Prelude.Text)
updateLag_lagName = Lens.lens (\UpdateLag' {lagName} -> lagName) (\s@UpdateLag' {} a -> s {lagName = a} :: UpdateLag)

-- | The LAG MAC Security (MACsec) encryption mode.
--
-- Amazon Web Services applies the value to all connections which are part
-- of the LAG.
updateLag_encryptionMode :: Lens.Lens' UpdateLag (Prelude.Maybe Prelude.Text)
updateLag_encryptionMode = Lens.lens (\UpdateLag' {encryptionMode} -> encryptionMode) (\s@UpdateLag' {} a -> s {encryptionMode = a} :: UpdateLag)

-- | The minimum number of physical connections that must be operational for
-- the LAG itself to be operational.
updateLag_minimumLinks :: Lens.Lens' UpdateLag (Prelude.Maybe Prelude.Int)
updateLag_minimumLinks = Lens.lens (\UpdateLag' {minimumLinks} -> minimumLinks) (\s@UpdateLag' {} a -> s {minimumLinks = a} :: UpdateLag)

-- | The ID of the LAG.
updateLag_lagId :: Lens.Lens' UpdateLag Prelude.Text
updateLag_lagId = Lens.lens (\UpdateLag' {lagId} -> lagId) (\s@UpdateLag' {} a -> s {lagId = a} :: UpdateLag)

instance Core.AWSRequest UpdateLag where
  type AWSResponse UpdateLag = Lag
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable UpdateLag

instance Prelude.NFData UpdateLag

instance Core.ToHeaders UpdateLag where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("OvertureService.UpdateLag" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateLag where
  toJSON UpdateLag' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("lagName" Core..=) Prelude.<$> lagName,
            ("encryptionMode" Core..=)
              Prelude.<$> encryptionMode,
            ("minimumLinks" Core..=) Prelude.<$> minimumLinks,
            Prelude.Just ("lagId" Core..= lagId)
          ]
      )

instance Core.ToPath UpdateLag where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateLag where
  toQuery = Prelude.const Prelude.mempty
