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
-- Module      : Network.AWS.StorageGateway.ListVolumeRecoveryPoints
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the recovery points for a specified gateway. This operation is
-- only supported in the cached volume gateway type.
--
-- Each cache volume has one recovery point. A volume recovery point is a
-- point in time at which all data of the volume is consistent and from
-- which you can create a snapshot or clone a new cached volume from a
-- source volume. To create a snapshot from a volume recovery point use the
-- CreateSnapshotFromVolumeRecoveryPoint operation.
module Network.AWS.StorageGateway.ListVolumeRecoveryPoints
  ( -- * Creating a Request
    ListVolumeRecoveryPoints (..),
    newListVolumeRecoveryPoints,

    -- * Request Lenses
    listVolumeRecoveryPoints_gatewayARN,

    -- * Destructuring the Response
    ListVolumeRecoveryPointsResponse (..),
    newListVolumeRecoveryPointsResponse,

    -- * Response Lenses
    listVolumeRecoveryPointsResponse_volumeRecoveryPointInfos,
    listVolumeRecoveryPointsResponse_gatewayARN,
    listVolumeRecoveryPointsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newListVolumeRecoveryPoints' smart constructor.
data ListVolumeRecoveryPoints = ListVolumeRecoveryPoints'
  { gatewayARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListVolumeRecoveryPoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'listVolumeRecoveryPoints_gatewayARN' - Undocumented member.
newListVolumeRecoveryPoints ::
  -- | 'gatewayARN'
  Core.Text ->
  ListVolumeRecoveryPoints
newListVolumeRecoveryPoints pGatewayARN_ =
  ListVolumeRecoveryPoints'
    { gatewayARN =
        pGatewayARN_
    }

-- | Undocumented member.
listVolumeRecoveryPoints_gatewayARN :: Lens.Lens' ListVolumeRecoveryPoints Core.Text
listVolumeRecoveryPoints_gatewayARN = Lens.lens (\ListVolumeRecoveryPoints' {gatewayARN} -> gatewayARN) (\s@ListVolumeRecoveryPoints' {} a -> s {gatewayARN = a} :: ListVolumeRecoveryPoints)

instance Core.AWSRequest ListVolumeRecoveryPoints where
  type
    AWSResponse ListVolumeRecoveryPoints =
      ListVolumeRecoveryPointsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVolumeRecoveryPointsResponse'
            Core.<$> ( x Core..?> "VolumeRecoveryPointInfos"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "GatewayARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListVolumeRecoveryPoints

instance Core.NFData ListVolumeRecoveryPoints

instance Core.ToHeaders ListVolumeRecoveryPoints where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.ListVolumeRecoveryPoints" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListVolumeRecoveryPoints where
  toJSON ListVolumeRecoveryPoints' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("GatewayARN" Core..= gatewayARN)]
      )

instance Core.ToPath ListVolumeRecoveryPoints where
  toPath = Core.const "/"

instance Core.ToQuery ListVolumeRecoveryPoints where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListVolumeRecoveryPointsResponse' smart constructor.
data ListVolumeRecoveryPointsResponse = ListVolumeRecoveryPointsResponse'
  { -- | An array of VolumeRecoveryPointInfo objects.
    volumeRecoveryPointInfos :: Core.Maybe [VolumeRecoveryPointInfo],
    gatewayARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListVolumeRecoveryPointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeRecoveryPointInfos', 'listVolumeRecoveryPointsResponse_volumeRecoveryPointInfos' - An array of VolumeRecoveryPointInfo objects.
--
-- 'gatewayARN', 'listVolumeRecoveryPointsResponse_gatewayARN' - Undocumented member.
--
-- 'httpStatus', 'listVolumeRecoveryPointsResponse_httpStatus' - The response's http status code.
newListVolumeRecoveryPointsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListVolumeRecoveryPointsResponse
newListVolumeRecoveryPointsResponse pHttpStatus_ =
  ListVolumeRecoveryPointsResponse'
    { volumeRecoveryPointInfos =
        Core.Nothing,
      gatewayARN = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of VolumeRecoveryPointInfo objects.
listVolumeRecoveryPointsResponse_volumeRecoveryPointInfos :: Lens.Lens' ListVolumeRecoveryPointsResponse (Core.Maybe [VolumeRecoveryPointInfo])
listVolumeRecoveryPointsResponse_volumeRecoveryPointInfos = Lens.lens (\ListVolumeRecoveryPointsResponse' {volumeRecoveryPointInfos} -> volumeRecoveryPointInfos) (\s@ListVolumeRecoveryPointsResponse' {} a -> s {volumeRecoveryPointInfos = a} :: ListVolumeRecoveryPointsResponse) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
listVolumeRecoveryPointsResponse_gatewayARN :: Lens.Lens' ListVolumeRecoveryPointsResponse (Core.Maybe Core.Text)
listVolumeRecoveryPointsResponse_gatewayARN = Lens.lens (\ListVolumeRecoveryPointsResponse' {gatewayARN} -> gatewayARN) (\s@ListVolumeRecoveryPointsResponse' {} a -> s {gatewayARN = a} :: ListVolumeRecoveryPointsResponse)

-- | The response's http status code.
listVolumeRecoveryPointsResponse_httpStatus :: Lens.Lens' ListVolumeRecoveryPointsResponse Core.Int
listVolumeRecoveryPointsResponse_httpStatus = Lens.lens (\ListVolumeRecoveryPointsResponse' {httpStatus} -> httpStatus) (\s@ListVolumeRecoveryPointsResponse' {} a -> s {httpStatus = a} :: ListVolumeRecoveryPointsResponse)

instance Core.NFData ListVolumeRecoveryPointsResponse
