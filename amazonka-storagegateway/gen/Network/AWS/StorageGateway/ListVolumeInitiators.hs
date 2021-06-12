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
-- Module      : Network.AWS.StorageGateway.ListVolumeInitiators
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists iSCSI initiators that are connected to a volume. You can use this
-- operation to determine whether a volume is being used or not. This
-- operation is only supported in the cached volume and stored volume
-- gateway types.
module Network.AWS.StorageGateway.ListVolumeInitiators
  ( -- * Creating a Request
    ListVolumeInitiators (..),
    newListVolumeInitiators,

    -- * Request Lenses
    listVolumeInitiators_volumeARN,

    -- * Destructuring the Response
    ListVolumeInitiatorsResponse (..),
    newListVolumeInitiatorsResponse,

    -- * Response Lenses
    listVolumeInitiatorsResponse_initiators,
    listVolumeInitiatorsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | ListVolumeInitiatorsInput
--
-- /See:/ 'newListVolumeInitiators' smart constructor.
data ListVolumeInitiators = ListVolumeInitiators'
  { -- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
    -- operation to return a list of gateway volumes for the gateway.
    volumeARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListVolumeInitiators' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeARN', 'listVolumeInitiators_volumeARN' - The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
-- operation to return a list of gateway volumes for the gateway.
newListVolumeInitiators ::
  -- | 'volumeARN'
  Core.Text ->
  ListVolumeInitiators
newListVolumeInitiators pVolumeARN_ =
  ListVolumeInitiators' {volumeARN = pVolumeARN_}

-- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
-- operation to return a list of gateway volumes for the gateway.
listVolumeInitiators_volumeARN :: Lens.Lens' ListVolumeInitiators Core.Text
listVolumeInitiators_volumeARN = Lens.lens (\ListVolumeInitiators' {volumeARN} -> volumeARN) (\s@ListVolumeInitiators' {} a -> s {volumeARN = a} :: ListVolumeInitiators)

instance Core.AWSRequest ListVolumeInitiators where
  type
    AWSResponse ListVolumeInitiators =
      ListVolumeInitiatorsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVolumeInitiatorsResponse'
            Core.<$> (x Core..?> "Initiators" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListVolumeInitiators

instance Core.NFData ListVolumeInitiators

instance Core.ToHeaders ListVolumeInitiators where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.ListVolumeInitiators" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListVolumeInitiators where
  toJSON ListVolumeInitiators' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("VolumeARN" Core..= volumeARN)]
      )

instance Core.ToPath ListVolumeInitiators where
  toPath = Core.const "/"

instance Core.ToQuery ListVolumeInitiators where
  toQuery = Core.const Core.mempty

-- | ListVolumeInitiatorsOutput
--
-- /See:/ 'newListVolumeInitiatorsResponse' smart constructor.
data ListVolumeInitiatorsResponse = ListVolumeInitiatorsResponse'
  { -- | The host names and port numbers of all iSCSI initiators that are
    -- connected to the gateway.
    initiators :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListVolumeInitiatorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'initiators', 'listVolumeInitiatorsResponse_initiators' - The host names and port numbers of all iSCSI initiators that are
-- connected to the gateway.
--
-- 'httpStatus', 'listVolumeInitiatorsResponse_httpStatus' - The response's http status code.
newListVolumeInitiatorsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListVolumeInitiatorsResponse
newListVolumeInitiatorsResponse pHttpStatus_ =
  ListVolumeInitiatorsResponse'
    { initiators =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The host names and port numbers of all iSCSI initiators that are
-- connected to the gateway.
listVolumeInitiatorsResponse_initiators :: Lens.Lens' ListVolumeInitiatorsResponse (Core.Maybe [Core.Text])
listVolumeInitiatorsResponse_initiators = Lens.lens (\ListVolumeInitiatorsResponse' {initiators} -> initiators) (\s@ListVolumeInitiatorsResponse' {} a -> s {initiators = a} :: ListVolumeInitiatorsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listVolumeInitiatorsResponse_httpStatus :: Lens.Lens' ListVolumeInitiatorsResponse Core.Int
listVolumeInitiatorsResponse_httpStatus = Lens.lens (\ListVolumeInitiatorsResponse' {httpStatus} -> httpStatus) (\s@ListVolumeInitiatorsResponse' {} a -> s {httpStatus = a} :: ListVolumeInitiatorsResponse)

instance Core.NFData ListVolumeInitiatorsResponse
