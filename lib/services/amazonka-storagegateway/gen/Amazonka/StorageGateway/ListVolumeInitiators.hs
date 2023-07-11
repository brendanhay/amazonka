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
-- Module      : Amazonka.StorageGateway.ListVolumeInitiators
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists iSCSI initiators that are connected to a volume. You can use this
-- operation to determine whether a volume is being used or not. This
-- operation is only supported in the cached volume and stored volume
-- gateway types.
module Amazonka.StorageGateway.ListVolumeInitiators
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | ListVolumeInitiatorsInput
--
-- /See:/ 'newListVolumeInitiators' smart constructor.
data ListVolumeInitiators = ListVolumeInitiators'
  { -- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
    -- operation to return a list of gateway volumes for the gateway.
    volumeARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ListVolumeInitiators
newListVolumeInitiators pVolumeARN_ =
  ListVolumeInitiators' {volumeARN = pVolumeARN_}

-- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
-- operation to return a list of gateway volumes for the gateway.
listVolumeInitiators_volumeARN :: Lens.Lens' ListVolumeInitiators Prelude.Text
listVolumeInitiators_volumeARN = Lens.lens (\ListVolumeInitiators' {volumeARN} -> volumeARN) (\s@ListVolumeInitiators' {} a -> s {volumeARN = a} :: ListVolumeInitiators)

instance Core.AWSRequest ListVolumeInitiators where
  type
    AWSResponse ListVolumeInitiators =
      ListVolumeInitiatorsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVolumeInitiatorsResponse'
            Prelude.<$> (x Data..?> "Initiators" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListVolumeInitiators where
  hashWithSalt _salt ListVolumeInitiators' {..} =
    _salt `Prelude.hashWithSalt` volumeARN

instance Prelude.NFData ListVolumeInitiators where
  rnf ListVolumeInitiators' {..} = Prelude.rnf volumeARN

instance Data.ToHeaders ListVolumeInitiators where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.ListVolumeInitiators" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListVolumeInitiators where
  toJSON ListVolumeInitiators' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("VolumeARN" Data..= volumeARN)]
      )

instance Data.ToPath ListVolumeInitiators where
  toPath = Prelude.const "/"

instance Data.ToQuery ListVolumeInitiators where
  toQuery = Prelude.const Prelude.mempty

-- | ListVolumeInitiatorsOutput
--
-- /See:/ 'newListVolumeInitiatorsResponse' smart constructor.
data ListVolumeInitiatorsResponse = ListVolumeInitiatorsResponse'
  { -- | The host names and port numbers of all iSCSI initiators that are
    -- connected to the gateway.
    initiators :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListVolumeInitiatorsResponse
newListVolumeInitiatorsResponse pHttpStatus_ =
  ListVolumeInitiatorsResponse'
    { initiators =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The host names and port numbers of all iSCSI initiators that are
-- connected to the gateway.
listVolumeInitiatorsResponse_initiators :: Lens.Lens' ListVolumeInitiatorsResponse (Prelude.Maybe [Prelude.Text])
listVolumeInitiatorsResponse_initiators = Lens.lens (\ListVolumeInitiatorsResponse' {initiators} -> initiators) (\s@ListVolumeInitiatorsResponse' {} a -> s {initiators = a} :: ListVolumeInitiatorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listVolumeInitiatorsResponse_httpStatus :: Lens.Lens' ListVolumeInitiatorsResponse Prelude.Int
listVolumeInitiatorsResponse_httpStatus = Lens.lens (\ListVolumeInitiatorsResponse' {httpStatus} -> httpStatus) (\s@ListVolumeInitiatorsResponse' {} a -> s {httpStatus = a} :: ListVolumeInitiatorsResponse)

instance Prelude.NFData ListVolumeInitiatorsResponse where
  rnf ListVolumeInitiatorsResponse' {..} =
    Prelude.rnf initiators
      `Prelude.seq` Prelude.rnf httpStatus
