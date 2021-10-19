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
-- Module      : Network.AWS.DirectConnect.DeleteLag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified link aggregation group (LAG). You cannot delete a
-- LAG if it has active virtual interfaces or hosted connections.
module Network.AWS.DirectConnect.DeleteLag
  ( -- * Creating a Request
    DeleteLag (..),
    newDeleteLag,

    -- * Request Lenses
    deleteLag_lagId,

    -- * Destructuring the Response
    Lag (..),
    newLag,

    -- * Response Lenses
    lag_lagId,
    lag_macSecCapable,
    lag_connectionsBandwidth,
    lag_minimumLinks,
    lag_lagName,
    lag_location,
    lag_connections,
    lag_awsDevice,
    lag_hasLogicalRedundancy,
    lag_awsLogicalDeviceId,
    lag_allowsHostedConnections,
    lag_encryptionMode,
    lag_numberOfConnections,
    lag_jumboFrameCapable,
    lag_lagState,
    lag_ownerAccount,
    lag_region,
    lag_macSecKeys,
    lag_providerName,
    lag_awsDeviceV2,
    lag_tags,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteLag' smart constructor.
data DeleteLag = DeleteLag'
  { -- | The ID of the LAG.
    lagId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lagId', 'deleteLag_lagId' - The ID of the LAG.
newDeleteLag ::
  -- | 'lagId'
  Prelude.Text ->
  DeleteLag
newDeleteLag pLagId_ = DeleteLag' {lagId = pLagId_}

-- | The ID of the LAG.
deleteLag_lagId :: Lens.Lens' DeleteLag Prelude.Text
deleteLag_lagId = Lens.lens (\DeleteLag' {lagId} -> lagId) (\s@DeleteLag' {} a -> s {lagId = a} :: DeleteLag)

instance Core.AWSRequest DeleteLag where
  type AWSResponse DeleteLag = Lag
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable DeleteLag

instance Prelude.NFData DeleteLag

instance Core.ToHeaders DeleteLag where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("OvertureService.DeleteLag" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteLag where
  toJSON DeleteLag' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("lagId" Core..= lagId)]
      )

instance Core.ToPath DeleteLag where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteLag where
  toQuery = Prelude.const Prelude.mempty
