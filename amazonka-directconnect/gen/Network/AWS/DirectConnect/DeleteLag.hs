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
    lag_numberOfConnections,
    lag_awsDeviceV2,
    lag_allowsHostedConnections,
    lag_providerName,
    lag_hasLogicalRedundancy,
    lag_connections,
    lag_awsDevice,
    lag_lagName,
    lag_lagState,
    lag_jumboFrameCapable,
    lag_connectionsBandwidth,
    lag_lagId,
    lag_tags,
    lag_ownerAccount,
    lag_region,
    lag_location,
    lag_minimumLinks,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteLag' smart constructor.
data DeleteLag = DeleteLag'
  { -- | The ID of the LAG.
    lagId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeleteLag
newDeleteLag pLagId_ = DeleteLag' {lagId = pLagId_}

-- | The ID of the LAG.
deleteLag_lagId :: Lens.Lens' DeleteLag Core.Text
deleteLag_lagId = Lens.lens (\DeleteLag' {lagId} -> lagId) (\s@DeleteLag' {} a -> s {lagId = a} :: DeleteLag)

instance Core.AWSRequest DeleteLag where
  type AWSResponse DeleteLag = Lag
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable DeleteLag

instance Core.NFData DeleteLag

instance Core.ToHeaders DeleteLag where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("OvertureService.DeleteLag" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteLag where
  toJSON DeleteLag' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("lagId" Core..= lagId)])

instance Core.ToPath DeleteLag where
  toPath = Core.const "/"

instance Core.ToQuery DeleteLag where
  toQuery = Core.const Core.mempty
