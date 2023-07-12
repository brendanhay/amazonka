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
-- Module      : Amazonka.StorageGateway.DeleteChapCredentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes Challenge-Handshake Authentication Protocol (CHAP) credentials
-- for a specified iSCSI target and initiator pair. This operation is
-- supported in volume and tape gateway types.
module Amazonka.StorageGateway.DeleteChapCredentials
  ( -- * Creating a Request
    DeleteChapCredentials (..),
    newDeleteChapCredentials,

    -- * Request Lenses
    deleteChapCredentials_targetARN,
    deleteChapCredentials_initiatorName,

    -- * Destructuring the Response
    DeleteChapCredentialsResponse (..),
    newDeleteChapCredentialsResponse,

    -- * Response Lenses
    deleteChapCredentialsResponse_initiatorName,
    deleteChapCredentialsResponse_targetARN,
    deleteChapCredentialsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | A JSON object containing one or more of the following fields:
--
-- -   DeleteChapCredentialsInput$InitiatorName
--
-- -   DeleteChapCredentialsInput$TargetARN
--
-- /See:/ 'newDeleteChapCredentials' smart constructor.
data DeleteChapCredentials = DeleteChapCredentials'
  { -- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the
    -- DescribeStorediSCSIVolumes operation to return to retrieve the TargetARN
    -- for specified VolumeARN.
    targetARN :: Prelude.Text,
    -- | The iSCSI initiator that connects to the target.
    initiatorName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteChapCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetARN', 'deleteChapCredentials_targetARN' - The Amazon Resource Name (ARN) of the iSCSI volume target. Use the
-- DescribeStorediSCSIVolumes operation to return to retrieve the TargetARN
-- for specified VolumeARN.
--
-- 'initiatorName', 'deleteChapCredentials_initiatorName' - The iSCSI initiator that connects to the target.
newDeleteChapCredentials ::
  -- | 'targetARN'
  Prelude.Text ->
  -- | 'initiatorName'
  Prelude.Text ->
  DeleteChapCredentials
newDeleteChapCredentials pTargetARN_ pInitiatorName_ =
  DeleteChapCredentials'
    { targetARN = pTargetARN_,
      initiatorName = pInitiatorName_
    }

-- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the
-- DescribeStorediSCSIVolumes operation to return to retrieve the TargetARN
-- for specified VolumeARN.
deleteChapCredentials_targetARN :: Lens.Lens' DeleteChapCredentials Prelude.Text
deleteChapCredentials_targetARN = Lens.lens (\DeleteChapCredentials' {targetARN} -> targetARN) (\s@DeleteChapCredentials' {} a -> s {targetARN = a} :: DeleteChapCredentials)

-- | The iSCSI initiator that connects to the target.
deleteChapCredentials_initiatorName :: Lens.Lens' DeleteChapCredentials Prelude.Text
deleteChapCredentials_initiatorName = Lens.lens (\DeleteChapCredentials' {initiatorName} -> initiatorName) (\s@DeleteChapCredentials' {} a -> s {initiatorName = a} :: DeleteChapCredentials)

instance Core.AWSRequest DeleteChapCredentials where
  type
    AWSResponse DeleteChapCredentials =
      DeleteChapCredentialsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteChapCredentialsResponse'
            Prelude.<$> (x Data..?> "InitiatorName")
            Prelude.<*> (x Data..?> "TargetARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteChapCredentials where
  hashWithSalt _salt DeleteChapCredentials' {..} =
    _salt
      `Prelude.hashWithSalt` targetARN
      `Prelude.hashWithSalt` initiatorName

instance Prelude.NFData DeleteChapCredentials where
  rnf DeleteChapCredentials' {..} =
    Prelude.rnf targetARN
      `Prelude.seq` Prelude.rnf initiatorName

instance Data.ToHeaders DeleteChapCredentials where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.DeleteChapCredentials" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteChapCredentials where
  toJSON DeleteChapCredentials' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("TargetARN" Data..= targetARN),
            Prelude.Just
              ("InitiatorName" Data..= initiatorName)
          ]
      )

instance Data.ToPath DeleteChapCredentials where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteChapCredentials where
  toQuery = Prelude.const Prelude.mempty

-- | A JSON object containing the following fields:
--
-- /See:/ 'newDeleteChapCredentialsResponse' smart constructor.
data DeleteChapCredentialsResponse = DeleteChapCredentialsResponse'
  { -- | The iSCSI initiator that connects to the target.
    initiatorName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the target.
    targetARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteChapCredentialsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'initiatorName', 'deleteChapCredentialsResponse_initiatorName' - The iSCSI initiator that connects to the target.
--
-- 'targetARN', 'deleteChapCredentialsResponse_targetARN' - The Amazon Resource Name (ARN) of the target.
--
-- 'httpStatus', 'deleteChapCredentialsResponse_httpStatus' - The response's http status code.
newDeleteChapCredentialsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteChapCredentialsResponse
newDeleteChapCredentialsResponse pHttpStatus_ =
  DeleteChapCredentialsResponse'
    { initiatorName =
        Prelude.Nothing,
      targetARN = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The iSCSI initiator that connects to the target.
deleteChapCredentialsResponse_initiatorName :: Lens.Lens' DeleteChapCredentialsResponse (Prelude.Maybe Prelude.Text)
deleteChapCredentialsResponse_initiatorName = Lens.lens (\DeleteChapCredentialsResponse' {initiatorName} -> initiatorName) (\s@DeleteChapCredentialsResponse' {} a -> s {initiatorName = a} :: DeleteChapCredentialsResponse)

-- | The Amazon Resource Name (ARN) of the target.
deleteChapCredentialsResponse_targetARN :: Lens.Lens' DeleteChapCredentialsResponse (Prelude.Maybe Prelude.Text)
deleteChapCredentialsResponse_targetARN = Lens.lens (\DeleteChapCredentialsResponse' {targetARN} -> targetARN) (\s@DeleteChapCredentialsResponse' {} a -> s {targetARN = a} :: DeleteChapCredentialsResponse)

-- | The response's http status code.
deleteChapCredentialsResponse_httpStatus :: Lens.Lens' DeleteChapCredentialsResponse Prelude.Int
deleteChapCredentialsResponse_httpStatus = Lens.lens (\DeleteChapCredentialsResponse' {httpStatus} -> httpStatus) (\s@DeleteChapCredentialsResponse' {} a -> s {httpStatus = a} :: DeleteChapCredentialsResponse)

instance Prelude.NFData DeleteChapCredentialsResponse where
  rnf DeleteChapCredentialsResponse' {..} =
    Prelude.rnf initiatorName
      `Prelude.seq` Prelude.rnf targetARN
      `Prelude.seq` Prelude.rnf httpStatus
