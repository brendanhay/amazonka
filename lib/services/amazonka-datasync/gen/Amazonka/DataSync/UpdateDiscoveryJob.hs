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
-- Module      : Amazonka.DataSync.UpdateDiscoveryJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Edits a DataSync discovery job configuration.
module Amazonka.DataSync.UpdateDiscoveryJob
  ( -- * Creating a Request
    UpdateDiscoveryJob (..),
    newUpdateDiscoveryJob,

    -- * Request Lenses
    updateDiscoveryJob_discoveryJobArn,
    updateDiscoveryJob_collectionDurationMinutes,

    -- * Destructuring the Response
    UpdateDiscoveryJobResponse (..),
    newUpdateDiscoveryJobResponse,

    -- * Response Lenses
    updateDiscoveryJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDiscoveryJob' smart constructor.
data UpdateDiscoveryJob = UpdateDiscoveryJob'
  { -- | Specifies the Amazon Resource Name (ARN) of the discovery job that you
    -- want to update.
    discoveryJobArn :: Prelude.Text,
    -- | Specifies in minutes how long that you want the discovery job to run.
    -- (You can\'t set this parameter to less than the number of minutes that
    -- the job has already run for.)
    collectionDurationMinutes :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDiscoveryJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'discoveryJobArn', 'updateDiscoveryJob_discoveryJobArn' - Specifies the Amazon Resource Name (ARN) of the discovery job that you
-- want to update.
--
-- 'collectionDurationMinutes', 'updateDiscoveryJob_collectionDurationMinutes' - Specifies in minutes how long that you want the discovery job to run.
-- (You can\'t set this parameter to less than the number of minutes that
-- the job has already run for.)
newUpdateDiscoveryJob ::
  -- | 'discoveryJobArn'
  Prelude.Text ->
  -- | 'collectionDurationMinutes'
  Prelude.Natural ->
  UpdateDiscoveryJob
newUpdateDiscoveryJob
  pDiscoveryJobArn_
  pCollectionDurationMinutes_ =
    UpdateDiscoveryJob'
      { discoveryJobArn =
          pDiscoveryJobArn_,
        collectionDurationMinutes =
          pCollectionDurationMinutes_
      }

-- | Specifies the Amazon Resource Name (ARN) of the discovery job that you
-- want to update.
updateDiscoveryJob_discoveryJobArn :: Lens.Lens' UpdateDiscoveryJob Prelude.Text
updateDiscoveryJob_discoveryJobArn = Lens.lens (\UpdateDiscoveryJob' {discoveryJobArn} -> discoveryJobArn) (\s@UpdateDiscoveryJob' {} a -> s {discoveryJobArn = a} :: UpdateDiscoveryJob)

-- | Specifies in minutes how long that you want the discovery job to run.
-- (You can\'t set this parameter to less than the number of minutes that
-- the job has already run for.)
updateDiscoveryJob_collectionDurationMinutes :: Lens.Lens' UpdateDiscoveryJob Prelude.Natural
updateDiscoveryJob_collectionDurationMinutes = Lens.lens (\UpdateDiscoveryJob' {collectionDurationMinutes} -> collectionDurationMinutes) (\s@UpdateDiscoveryJob' {} a -> s {collectionDurationMinutes = a} :: UpdateDiscoveryJob)

instance Core.AWSRequest UpdateDiscoveryJob where
  type
    AWSResponse UpdateDiscoveryJob =
      UpdateDiscoveryJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDiscoveryJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDiscoveryJob where
  hashWithSalt _salt UpdateDiscoveryJob' {..} =
    _salt
      `Prelude.hashWithSalt` discoveryJobArn
      `Prelude.hashWithSalt` collectionDurationMinutes

instance Prelude.NFData UpdateDiscoveryJob where
  rnf UpdateDiscoveryJob' {..} =
    Prelude.rnf discoveryJobArn
      `Prelude.seq` Prelude.rnf collectionDurationMinutes

instance Data.ToHeaders UpdateDiscoveryJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "FmrsService.UpdateDiscoveryJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDiscoveryJob where
  toJSON UpdateDiscoveryJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DiscoveryJobArn" Data..= discoveryJobArn),
            Prelude.Just
              ( "CollectionDurationMinutes"
                  Data..= collectionDurationMinutes
              )
          ]
      )

instance Data.ToPath UpdateDiscoveryJob where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateDiscoveryJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDiscoveryJobResponse' smart constructor.
data UpdateDiscoveryJobResponse = UpdateDiscoveryJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDiscoveryJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDiscoveryJobResponse_httpStatus' - The response's http status code.
newUpdateDiscoveryJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDiscoveryJobResponse
newUpdateDiscoveryJobResponse pHttpStatus_ =
  UpdateDiscoveryJobResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateDiscoveryJobResponse_httpStatus :: Lens.Lens' UpdateDiscoveryJobResponse Prelude.Int
updateDiscoveryJobResponse_httpStatus = Lens.lens (\UpdateDiscoveryJobResponse' {httpStatus} -> httpStatus) (\s@UpdateDiscoveryJobResponse' {} a -> s {httpStatus = a} :: UpdateDiscoveryJobResponse)

instance Prelude.NFData UpdateDiscoveryJobResponse where
  rnf UpdateDiscoveryJobResponse' {..} =
    Prelude.rnf httpStatus
