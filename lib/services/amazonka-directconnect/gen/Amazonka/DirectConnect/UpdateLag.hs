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
-- Module      : Amazonka.DirectConnect.UpdateLag
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.DirectConnect.UpdateLag
  ( -- * Creating a Request
    UpdateLag (..),
    newUpdateLag,

    -- * Request Lenses
    updateLag_encryptionMode,
    updateLag_lagName,
    updateLag_minimumLinks,
    updateLag_lagId,

    -- * Destructuring the Response
    Lag (..),
    newLag,

    -- * Response Lenses
    lag_allowsHostedConnections,
    lag_awsDevice,
    lag_awsDeviceV2,
    lag_awsLogicalDeviceId,
    lag_connections,
    lag_connectionsBandwidth,
    lag_encryptionMode,
    lag_hasLogicalRedundancy,
    lag_jumboFrameCapable,
    lag_lagId,
    lag_lagName,
    lag_lagState,
    lag_location,
    lag_macSecCapable,
    lag_macSecKeys,
    lag_minimumLinks,
    lag_numberOfConnections,
    lag_ownerAccount,
    lag_providerName,
    lag_region,
    lag_tags,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateLag' smart constructor.
data UpdateLag = UpdateLag'
  { -- | The LAG MAC Security (MACsec) encryption mode.
    --
    -- Amazon Web Services applies the value to all connections which are part
    -- of the LAG.
    encryptionMode :: Prelude.Maybe Prelude.Text,
    -- | The name of the LAG.
    lagName :: Prelude.Maybe Prelude.Text,
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
-- 'encryptionMode', 'updateLag_encryptionMode' - The LAG MAC Security (MACsec) encryption mode.
--
-- Amazon Web Services applies the value to all connections which are part
-- of the LAG.
--
-- 'lagName', 'updateLag_lagName' - The name of the LAG.
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
    { encryptionMode = Prelude.Nothing,
      lagName = Prelude.Nothing,
      minimumLinks = Prelude.Nothing,
      lagId = pLagId_
    }

-- | The LAG MAC Security (MACsec) encryption mode.
--
-- Amazon Web Services applies the value to all connections which are part
-- of the LAG.
updateLag_encryptionMode :: Lens.Lens' UpdateLag (Prelude.Maybe Prelude.Text)
updateLag_encryptionMode = Lens.lens (\UpdateLag' {encryptionMode} -> encryptionMode) (\s@UpdateLag' {} a -> s {encryptionMode = a} :: UpdateLag)

-- | The name of the LAG.
updateLag_lagName :: Lens.Lens' UpdateLag (Prelude.Maybe Prelude.Text)
updateLag_lagName = Lens.lens (\UpdateLag' {lagName} -> lagName) (\s@UpdateLag' {} a -> s {lagName = a} :: UpdateLag)

-- | The minimum number of physical connections that must be operational for
-- the LAG itself to be operational.
updateLag_minimumLinks :: Lens.Lens' UpdateLag (Prelude.Maybe Prelude.Int)
updateLag_minimumLinks = Lens.lens (\UpdateLag' {minimumLinks} -> minimumLinks) (\s@UpdateLag' {} a -> s {minimumLinks = a} :: UpdateLag)

-- | The ID of the LAG.
updateLag_lagId :: Lens.Lens' UpdateLag Prelude.Text
updateLag_lagId = Lens.lens (\UpdateLag' {lagId} -> lagId) (\s@UpdateLag' {} a -> s {lagId = a} :: UpdateLag)

instance Core.AWSRequest UpdateLag where
  type AWSResponse UpdateLag = Lag
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable UpdateLag where
  hashWithSalt _salt UpdateLag' {..} =
    _salt `Prelude.hashWithSalt` encryptionMode
      `Prelude.hashWithSalt` lagName
      `Prelude.hashWithSalt` minimumLinks
      `Prelude.hashWithSalt` lagId

instance Prelude.NFData UpdateLag where
  rnf UpdateLag' {..} =
    Prelude.rnf encryptionMode
      `Prelude.seq` Prelude.rnf lagName
      `Prelude.seq` Prelude.rnf minimumLinks
      `Prelude.seq` Prelude.rnf lagId

instance Data.ToHeaders UpdateLag where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("OvertureService.UpdateLag" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateLag where
  toJSON UpdateLag' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("encryptionMode" Data..=)
              Prelude.<$> encryptionMode,
            ("lagName" Data..=) Prelude.<$> lagName,
            ("minimumLinks" Data..=) Prelude.<$> minimumLinks,
            Prelude.Just ("lagId" Data..= lagId)
          ]
      )

instance Data.ToPath UpdateLag where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateLag where
  toQuery = Prelude.const Prelude.mempty
