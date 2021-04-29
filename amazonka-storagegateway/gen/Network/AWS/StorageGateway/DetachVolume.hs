{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.StorageGateway.DetachVolume
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disconnects a volume from an iSCSI connection and then detaches the
-- volume from the specified gateway. Detaching and attaching a volume
-- enables you to recover your data from one gateway to a different gateway
-- without creating a snapshot. It also makes it easier to move your
-- volumes from an on-premises gateway to a gateway hosted on an Amazon EC2
-- instance. This operation is only supported in the volume gateway type.
module Network.AWS.StorageGateway.DetachVolume
  ( -- * Creating a Request
    DetachVolume (..),
    newDetachVolume,

    -- * Request Lenses
    detachVolume_forceDetach,
    detachVolume_volumeARN,

    -- * Destructuring the Response
    DetachVolumeResponse (..),
    newDetachVolumeResponse,

    -- * Response Lenses
    detachVolumeResponse_volumeARN,
    detachVolumeResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | AttachVolumeInput
--
-- /See:/ 'newDetachVolume' smart constructor.
data DetachVolume = DetachVolume'
  { -- | Set to @true@ to forcibly remove the iSCSI connection of the target
    -- volume and detach the volume. The default is @false@. If this value is
    -- set to @false@, you must manually disconnect the iSCSI connection from
    -- the target volume.
    --
    -- Valid Values: @true@ | @false@
    forceDetach :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the volume to detach from the gateway.
    volumeARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DetachVolume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forceDetach', 'detachVolume_forceDetach' - Set to @true@ to forcibly remove the iSCSI connection of the target
-- volume and detach the volume. The default is @false@. If this value is
-- set to @false@, you must manually disconnect the iSCSI connection from
-- the target volume.
--
-- Valid Values: @true@ | @false@
--
-- 'volumeARN', 'detachVolume_volumeARN' - The Amazon Resource Name (ARN) of the volume to detach from the gateway.
newDetachVolume ::
  -- | 'volumeARN'
  Prelude.Text ->
  DetachVolume
newDetachVolume pVolumeARN_ =
  DetachVolume'
    { forceDetach = Prelude.Nothing,
      volumeARN = pVolumeARN_
    }

-- | Set to @true@ to forcibly remove the iSCSI connection of the target
-- volume and detach the volume. The default is @false@. If this value is
-- set to @false@, you must manually disconnect the iSCSI connection from
-- the target volume.
--
-- Valid Values: @true@ | @false@
detachVolume_forceDetach :: Lens.Lens' DetachVolume (Prelude.Maybe Prelude.Bool)
detachVolume_forceDetach = Lens.lens (\DetachVolume' {forceDetach} -> forceDetach) (\s@DetachVolume' {} a -> s {forceDetach = a} :: DetachVolume)

-- | The Amazon Resource Name (ARN) of the volume to detach from the gateway.
detachVolume_volumeARN :: Lens.Lens' DetachVolume Prelude.Text
detachVolume_volumeARN = Lens.lens (\DetachVolume' {volumeARN} -> volumeARN) (\s@DetachVolume' {} a -> s {volumeARN = a} :: DetachVolume)

instance Prelude.AWSRequest DetachVolume where
  type Rs DetachVolume = DetachVolumeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DetachVolumeResponse'
            Prelude.<$> (x Prelude..?> "VolumeARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DetachVolume

instance Prelude.NFData DetachVolume

instance Prelude.ToHeaders DetachVolume where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StorageGateway_20130630.DetachVolume" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DetachVolume where
  toJSON DetachVolume' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ForceDetach" Prelude..=) Prelude.<$> forceDetach,
            Prelude.Just ("VolumeARN" Prelude..= volumeARN)
          ]
      )

instance Prelude.ToPath DetachVolume where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DetachVolume where
  toQuery = Prelude.const Prelude.mempty

-- | AttachVolumeOutput
--
-- /See:/ 'newDetachVolumeResponse' smart constructor.
data DetachVolumeResponse = DetachVolumeResponse'
  { -- | The Amazon Resource Name (ARN) of the volume that was detached.
    volumeARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DetachVolumeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeARN', 'detachVolumeResponse_volumeARN' - The Amazon Resource Name (ARN) of the volume that was detached.
--
-- 'httpStatus', 'detachVolumeResponse_httpStatus' - The response's http status code.
newDetachVolumeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DetachVolumeResponse
newDetachVolumeResponse pHttpStatus_ =
  DetachVolumeResponse'
    { volumeARN = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the volume that was detached.
detachVolumeResponse_volumeARN :: Lens.Lens' DetachVolumeResponse (Prelude.Maybe Prelude.Text)
detachVolumeResponse_volumeARN = Lens.lens (\DetachVolumeResponse' {volumeARN} -> volumeARN) (\s@DetachVolumeResponse' {} a -> s {volumeARN = a} :: DetachVolumeResponse)

-- | The response's http status code.
detachVolumeResponse_httpStatus :: Lens.Lens' DetachVolumeResponse Prelude.Int
detachVolumeResponse_httpStatus = Lens.lens (\DetachVolumeResponse' {httpStatus} -> httpStatus) (\s@DetachVolumeResponse' {} a -> s {httpStatus = a} :: DetachVolumeResponse)

instance Prelude.NFData DetachVolumeResponse
