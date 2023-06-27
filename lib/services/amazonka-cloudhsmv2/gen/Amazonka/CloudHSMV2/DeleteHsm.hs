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
-- Module      : Amazonka.CloudHSMV2.DeleteHsm
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified HSM. To specify an HSM, you can use its identifier
-- (ID), the IP address of the HSM\'s elastic network interface (ENI), or
-- the ID of the HSM\'s ENI. You need to specify only one of these values.
-- To find these values, use DescribeClusters.
module Amazonka.CloudHSMV2.DeleteHsm
  ( -- * Creating a Request
    DeleteHsm (..),
    newDeleteHsm,

    -- * Request Lenses
    deleteHsm_eniId,
    deleteHsm_eniIp,
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

import Amazonka.CloudHSMV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteHsm' smart constructor.
data DeleteHsm = DeleteHsm'
  { -- | The identifier (ID) of the elastic network interface (ENI) of the HSM
    -- that you are deleting.
    eniId :: Prelude.Maybe Prelude.Text,
    -- | The IP address of the elastic network interface (ENI) of the HSM that
    -- you are deleting.
    eniIp :: Prelude.Maybe Prelude.Text,
    -- | The identifier (ID) of the HSM that you are deleting.
    hsmId :: Prelude.Maybe Prelude.Text,
    -- | The identifier (ID) of the cluster that contains the HSM that you are
    -- deleting.
    clusterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteHsm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eniId', 'deleteHsm_eniId' - The identifier (ID) of the elastic network interface (ENI) of the HSM
-- that you are deleting.
--
-- 'eniIp', 'deleteHsm_eniIp' - The IP address of the elastic network interface (ENI) of the HSM that
-- you are deleting.
--
-- 'hsmId', 'deleteHsm_hsmId' - The identifier (ID) of the HSM that you are deleting.
--
-- 'clusterId', 'deleteHsm_clusterId' - The identifier (ID) of the cluster that contains the HSM that you are
-- deleting.
newDeleteHsm ::
  -- | 'clusterId'
  Prelude.Text ->
  DeleteHsm
newDeleteHsm pClusterId_ =
  DeleteHsm'
    { eniId = Prelude.Nothing,
      eniIp = Prelude.Nothing,
      hsmId = Prelude.Nothing,
      clusterId = pClusterId_
    }

-- | The identifier (ID) of the elastic network interface (ENI) of the HSM
-- that you are deleting.
deleteHsm_eniId :: Lens.Lens' DeleteHsm (Prelude.Maybe Prelude.Text)
deleteHsm_eniId = Lens.lens (\DeleteHsm' {eniId} -> eniId) (\s@DeleteHsm' {} a -> s {eniId = a} :: DeleteHsm)

-- | The IP address of the elastic network interface (ENI) of the HSM that
-- you are deleting.
deleteHsm_eniIp :: Lens.Lens' DeleteHsm (Prelude.Maybe Prelude.Text)
deleteHsm_eniIp = Lens.lens (\DeleteHsm' {eniIp} -> eniIp) (\s@DeleteHsm' {} a -> s {eniIp = a} :: DeleteHsm)

-- | The identifier (ID) of the HSM that you are deleting.
deleteHsm_hsmId :: Lens.Lens' DeleteHsm (Prelude.Maybe Prelude.Text)
deleteHsm_hsmId = Lens.lens (\DeleteHsm' {hsmId} -> hsmId) (\s@DeleteHsm' {} a -> s {hsmId = a} :: DeleteHsm)

-- | The identifier (ID) of the cluster that contains the HSM that you are
-- deleting.
deleteHsm_clusterId :: Lens.Lens' DeleteHsm Prelude.Text
deleteHsm_clusterId = Lens.lens (\DeleteHsm' {clusterId} -> clusterId) (\s@DeleteHsm' {} a -> s {clusterId = a} :: DeleteHsm)

instance Core.AWSRequest DeleteHsm where
  type AWSResponse DeleteHsm = DeleteHsmResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteHsmResponse'
            Prelude.<$> (x Data..?> "HsmId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteHsm where
  hashWithSalt _salt DeleteHsm' {..} =
    _salt
      `Prelude.hashWithSalt` eniId
      `Prelude.hashWithSalt` eniIp
      `Prelude.hashWithSalt` hsmId
      `Prelude.hashWithSalt` clusterId

instance Prelude.NFData DeleteHsm where
  rnf DeleteHsm' {..} =
    Prelude.rnf eniId
      `Prelude.seq` Prelude.rnf eniIp
      `Prelude.seq` Prelude.rnf hsmId
      `Prelude.seq` Prelude.rnf clusterId

instance Data.ToHeaders DeleteHsm where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("BaldrApiService.DeleteHsm" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteHsm where
  toJSON DeleteHsm' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EniId" Data..=) Prelude.<$> eniId,
            ("EniIp" Data..=) Prelude.<$> eniIp,
            ("HsmId" Data..=) Prelude.<$> hsmId,
            Prelude.Just ("ClusterId" Data..= clusterId)
          ]
      )

instance Data.ToPath DeleteHsm where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteHsm where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteHsmResponse' smart constructor.
data DeleteHsmResponse = DeleteHsmResponse'
  { -- | The identifier (ID) of the HSM that was deleted.
    hsmId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteHsmResponse
newDeleteHsmResponse pHttpStatus_ =
  DeleteHsmResponse'
    { hsmId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier (ID) of the HSM that was deleted.
deleteHsmResponse_hsmId :: Lens.Lens' DeleteHsmResponse (Prelude.Maybe Prelude.Text)
deleteHsmResponse_hsmId = Lens.lens (\DeleteHsmResponse' {hsmId} -> hsmId) (\s@DeleteHsmResponse' {} a -> s {hsmId = a} :: DeleteHsmResponse)

-- | The response's http status code.
deleteHsmResponse_httpStatus :: Lens.Lens' DeleteHsmResponse Prelude.Int
deleteHsmResponse_httpStatus = Lens.lens (\DeleteHsmResponse' {httpStatus} -> httpStatus) (\s@DeleteHsmResponse' {} a -> s {httpStatus = a} :: DeleteHsmResponse)

instance Prelude.NFData DeleteHsmResponse where
  rnf DeleteHsmResponse' {..} =
    Prelude.rnf hsmId
      `Prelude.seq` Prelude.rnf httpStatus
