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
-- Module      : Network.AWS.CloudHSMv2.DeleteHsm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified HSM. To specify an HSM, you can use its identifier
-- (ID), the IP address of the HSM\'s elastic network interface (ENI), or
-- the ID of the HSM\'s ENI. You need to specify only one of these values.
-- To find these values, use DescribeClusters.
module Network.AWS.CloudHSMv2.DeleteHsm
  ( -- * Creating a Request
    DeleteHsm (..),
    newDeleteHsm,

    -- * Request Lenses
    deleteHsm_eniIp,
    deleteHsm_eniId,
    deleteHsm_hsmId,
    deleteHsm_clusterId,

    -- * Destructuring the Response
    DeleteHsmResponse (..),
    newDeleteHsmResponse,

    -- * Response Lenses
    deleteHsmResponse_hsmId,
    deleteHsmResponse_httpStatus,
  )
where

import Network.AWS.CloudHSMv2.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteHsm' smart constructor.
data DeleteHsm = DeleteHsm'
  { -- | The IP address of the elastic network interface (ENI) of the HSM that
    -- you are deleting.
    eniIp :: Core.Maybe Core.Text,
    -- | The identifier (ID) of the elastic network interface (ENI) of the HSM
    -- that you are deleting.
    eniId :: Core.Maybe Core.Text,
    -- | The identifier (ID) of the HSM that you are deleting.
    hsmId :: Core.Maybe Core.Text,
    -- | The identifier (ID) of the cluster that contains the HSM that you are
    -- deleting.
    clusterId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteHsm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eniIp', 'deleteHsm_eniIp' - The IP address of the elastic network interface (ENI) of the HSM that
-- you are deleting.
--
-- 'eniId', 'deleteHsm_eniId' - The identifier (ID) of the elastic network interface (ENI) of the HSM
-- that you are deleting.
--
-- 'hsmId', 'deleteHsm_hsmId' - The identifier (ID) of the HSM that you are deleting.
--
-- 'clusterId', 'deleteHsm_clusterId' - The identifier (ID) of the cluster that contains the HSM that you are
-- deleting.
newDeleteHsm ::
  -- | 'clusterId'
  Core.Text ->
  DeleteHsm
newDeleteHsm pClusterId_ =
  DeleteHsm'
    { eniIp = Core.Nothing,
      eniId = Core.Nothing,
      hsmId = Core.Nothing,
      clusterId = pClusterId_
    }

-- | The IP address of the elastic network interface (ENI) of the HSM that
-- you are deleting.
deleteHsm_eniIp :: Lens.Lens' DeleteHsm (Core.Maybe Core.Text)
deleteHsm_eniIp = Lens.lens (\DeleteHsm' {eniIp} -> eniIp) (\s@DeleteHsm' {} a -> s {eniIp = a} :: DeleteHsm)

-- | The identifier (ID) of the elastic network interface (ENI) of the HSM
-- that you are deleting.
deleteHsm_eniId :: Lens.Lens' DeleteHsm (Core.Maybe Core.Text)
deleteHsm_eniId = Lens.lens (\DeleteHsm' {eniId} -> eniId) (\s@DeleteHsm' {} a -> s {eniId = a} :: DeleteHsm)

-- | The identifier (ID) of the HSM that you are deleting.
deleteHsm_hsmId :: Lens.Lens' DeleteHsm (Core.Maybe Core.Text)
deleteHsm_hsmId = Lens.lens (\DeleteHsm' {hsmId} -> hsmId) (\s@DeleteHsm' {} a -> s {hsmId = a} :: DeleteHsm)

-- | The identifier (ID) of the cluster that contains the HSM that you are
-- deleting.
deleteHsm_clusterId :: Lens.Lens' DeleteHsm Core.Text
deleteHsm_clusterId = Lens.lens (\DeleteHsm' {clusterId} -> clusterId) (\s@DeleteHsm' {} a -> s {clusterId = a} :: DeleteHsm)

instance Core.AWSRequest DeleteHsm where
  type AWSResponse DeleteHsm = DeleteHsmResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteHsmResponse'
            Core.<$> (x Core..?> "HsmId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteHsm

instance Core.NFData DeleteHsm

instance Core.ToHeaders DeleteHsm where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("BaldrApiService.DeleteHsm" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteHsm where
  toJSON DeleteHsm' {..} =
    Core.object
      ( Core.catMaybes
          [ ("EniIp" Core..=) Core.<$> eniIp,
            ("EniId" Core..=) Core.<$> eniId,
            ("HsmId" Core..=) Core.<$> hsmId,
            Core.Just ("ClusterId" Core..= clusterId)
          ]
      )

instance Core.ToPath DeleteHsm where
  toPath = Core.const "/"

instance Core.ToQuery DeleteHsm where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteHsmResponse' smart constructor.
data DeleteHsmResponse = DeleteHsmResponse'
  { -- | The identifier (ID) of the HSM that was deleted.
    hsmId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteHsmResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hsmId', 'deleteHsmResponse_hsmId' - The identifier (ID) of the HSM that was deleted.
--
-- 'httpStatus', 'deleteHsmResponse_httpStatus' - The response's http status code.
newDeleteHsmResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteHsmResponse
newDeleteHsmResponse pHttpStatus_ =
  DeleteHsmResponse'
    { hsmId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier (ID) of the HSM that was deleted.
deleteHsmResponse_hsmId :: Lens.Lens' DeleteHsmResponse (Core.Maybe Core.Text)
deleteHsmResponse_hsmId = Lens.lens (\DeleteHsmResponse' {hsmId} -> hsmId) (\s@DeleteHsmResponse' {} a -> s {hsmId = a} :: DeleteHsmResponse)

-- | The response's http status code.
deleteHsmResponse_httpStatus :: Lens.Lens' DeleteHsmResponse Core.Int
deleteHsmResponse_httpStatus = Lens.lens (\DeleteHsmResponse' {httpStatus} -> httpStatus) (\s@DeleteHsmResponse' {} a -> s {httpStatus = a} :: DeleteHsmResponse)

instance Core.NFData DeleteHsmResponse
