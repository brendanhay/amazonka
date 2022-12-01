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
-- Module      : Amazonka.StorageGateway.ListVolumeRecoveryPoints
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.StorageGateway.ListVolumeRecoveryPoints
  ( -- * Creating a Request
    ListVolumeRecoveryPoints (..),
    newListVolumeRecoveryPoints,

    -- * Request Lenses
    listVolumeRecoveryPoints_gatewayARN,

    -- * Destructuring the Response
    ListVolumeRecoveryPointsResponse (..),
    newListVolumeRecoveryPointsResponse,

    -- * Response Lenses
    listVolumeRecoveryPointsResponse_gatewayARN,
    listVolumeRecoveryPointsResponse_volumeRecoveryPointInfos,
    listVolumeRecoveryPointsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | /See:/ 'newListVolumeRecoveryPoints' smart constructor.
data ListVolumeRecoveryPoints = ListVolumeRecoveryPoints'
  { gatewayARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ListVolumeRecoveryPoints
newListVolumeRecoveryPoints pGatewayARN_ =
  ListVolumeRecoveryPoints'
    { gatewayARN =
        pGatewayARN_
    }

-- | Undocumented member.
listVolumeRecoveryPoints_gatewayARN :: Lens.Lens' ListVolumeRecoveryPoints Prelude.Text
listVolumeRecoveryPoints_gatewayARN = Lens.lens (\ListVolumeRecoveryPoints' {gatewayARN} -> gatewayARN) (\s@ListVolumeRecoveryPoints' {} a -> s {gatewayARN = a} :: ListVolumeRecoveryPoints)

instance Core.AWSRequest ListVolumeRecoveryPoints where
  type
    AWSResponse ListVolumeRecoveryPoints =
      ListVolumeRecoveryPointsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVolumeRecoveryPointsResponse'
            Prelude.<$> (x Core..?> "GatewayARN")
            Prelude.<*> ( x Core..?> "VolumeRecoveryPointInfos"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListVolumeRecoveryPoints where
  hashWithSalt _salt ListVolumeRecoveryPoints' {..} =
    _salt `Prelude.hashWithSalt` gatewayARN

instance Prelude.NFData ListVolumeRecoveryPoints where
  rnf ListVolumeRecoveryPoints' {..} =
    Prelude.rnf gatewayARN

instance Core.ToHeaders ListVolumeRecoveryPoints where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.ListVolumeRecoveryPoints" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListVolumeRecoveryPoints where
  toJSON ListVolumeRecoveryPoints' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("GatewayARN" Core..= gatewayARN)]
      )

instance Core.ToPath ListVolumeRecoveryPoints where
  toPath = Prelude.const "/"

instance Core.ToQuery ListVolumeRecoveryPoints where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListVolumeRecoveryPointsResponse' smart constructor.
data ListVolumeRecoveryPointsResponse = ListVolumeRecoveryPointsResponse'
  { gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | An array of VolumeRecoveryPointInfo objects.
    volumeRecoveryPointInfos :: Prelude.Maybe [VolumeRecoveryPointInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVolumeRecoveryPointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'listVolumeRecoveryPointsResponse_gatewayARN' - Undocumented member.
--
-- 'volumeRecoveryPointInfos', 'listVolumeRecoveryPointsResponse_volumeRecoveryPointInfos' - An array of VolumeRecoveryPointInfo objects.
--
-- 'httpStatus', 'listVolumeRecoveryPointsResponse_httpStatus' - The response's http status code.
newListVolumeRecoveryPointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVolumeRecoveryPointsResponse
newListVolumeRecoveryPointsResponse pHttpStatus_ =
  ListVolumeRecoveryPointsResponse'
    { gatewayARN =
        Prelude.Nothing,
      volumeRecoveryPointInfos =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listVolumeRecoveryPointsResponse_gatewayARN :: Lens.Lens' ListVolumeRecoveryPointsResponse (Prelude.Maybe Prelude.Text)
listVolumeRecoveryPointsResponse_gatewayARN = Lens.lens (\ListVolumeRecoveryPointsResponse' {gatewayARN} -> gatewayARN) (\s@ListVolumeRecoveryPointsResponse' {} a -> s {gatewayARN = a} :: ListVolumeRecoveryPointsResponse)

-- | An array of VolumeRecoveryPointInfo objects.
listVolumeRecoveryPointsResponse_volumeRecoveryPointInfos :: Lens.Lens' ListVolumeRecoveryPointsResponse (Prelude.Maybe [VolumeRecoveryPointInfo])
listVolumeRecoveryPointsResponse_volumeRecoveryPointInfos = Lens.lens (\ListVolumeRecoveryPointsResponse' {volumeRecoveryPointInfos} -> volumeRecoveryPointInfos) (\s@ListVolumeRecoveryPointsResponse' {} a -> s {volumeRecoveryPointInfos = a} :: ListVolumeRecoveryPointsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listVolumeRecoveryPointsResponse_httpStatus :: Lens.Lens' ListVolumeRecoveryPointsResponse Prelude.Int
listVolumeRecoveryPointsResponse_httpStatus = Lens.lens (\ListVolumeRecoveryPointsResponse' {httpStatus} -> httpStatus) (\s@ListVolumeRecoveryPointsResponse' {} a -> s {httpStatus = a} :: ListVolumeRecoveryPointsResponse)

instance
  Prelude.NFData
    ListVolumeRecoveryPointsResponse
  where
  rnf ListVolumeRecoveryPointsResponse' {..} =
    Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf volumeRecoveryPointInfos
      `Prelude.seq` Prelude.rnf httpStatus
