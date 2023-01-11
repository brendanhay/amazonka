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
-- Module      : Amazonka.Connect.UpdateTrafficDistribution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the traffic distribution for a given traffic distribution group.
--
-- For more information about updating a traffic distribution group, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/update-telephony-traffic-distribution.html Update telephony traffic distribution across Amazon Web Services Regions>
-- in the /Amazon Connect Administrator Guide/.
module Amazonka.Connect.UpdateTrafficDistribution
  ( -- * Creating a Request
    UpdateTrafficDistribution (..),
    newUpdateTrafficDistribution,

    -- * Request Lenses
    updateTrafficDistribution_telephonyConfig,
    updateTrafficDistribution_id,

    -- * Destructuring the Response
    UpdateTrafficDistributionResponse (..),
    newUpdateTrafficDistributionResponse,

    -- * Response Lenses
    updateTrafficDistributionResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateTrafficDistribution' smart constructor.
data UpdateTrafficDistribution = UpdateTrafficDistribution'
  { -- | The distribution of traffic between the instance and its replica(s).
    telephonyConfig :: Prelude.Maybe TelephonyConfig,
    -- | The identifier of the traffic distribution group. This can be the ID or
    -- the ARN if the API is being called in the Region where the traffic
    -- distribution group was created. The ARN must be provided if the call is
    -- from the replicated Region.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTrafficDistribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'telephonyConfig', 'updateTrafficDistribution_telephonyConfig' - The distribution of traffic between the instance and its replica(s).
--
-- 'id', 'updateTrafficDistribution_id' - The identifier of the traffic distribution group. This can be the ID or
-- the ARN if the API is being called in the Region where the traffic
-- distribution group was created. The ARN must be provided if the call is
-- from the replicated Region.
newUpdateTrafficDistribution ::
  -- | 'id'
  Prelude.Text ->
  UpdateTrafficDistribution
newUpdateTrafficDistribution pId_ =
  UpdateTrafficDistribution'
    { telephonyConfig =
        Prelude.Nothing,
      id = pId_
    }

-- | The distribution of traffic between the instance and its replica(s).
updateTrafficDistribution_telephonyConfig :: Lens.Lens' UpdateTrafficDistribution (Prelude.Maybe TelephonyConfig)
updateTrafficDistribution_telephonyConfig = Lens.lens (\UpdateTrafficDistribution' {telephonyConfig} -> telephonyConfig) (\s@UpdateTrafficDistribution' {} a -> s {telephonyConfig = a} :: UpdateTrafficDistribution)

-- | The identifier of the traffic distribution group. This can be the ID or
-- the ARN if the API is being called in the Region where the traffic
-- distribution group was created. The ARN must be provided if the call is
-- from the replicated Region.
updateTrafficDistribution_id :: Lens.Lens' UpdateTrafficDistribution Prelude.Text
updateTrafficDistribution_id = Lens.lens (\UpdateTrafficDistribution' {id} -> id) (\s@UpdateTrafficDistribution' {} a -> s {id = a} :: UpdateTrafficDistribution)

instance Core.AWSRequest UpdateTrafficDistribution where
  type
    AWSResponse UpdateTrafficDistribution =
      UpdateTrafficDistributionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateTrafficDistributionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTrafficDistribution where
  hashWithSalt _salt UpdateTrafficDistribution' {..} =
    _salt `Prelude.hashWithSalt` telephonyConfig
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateTrafficDistribution where
  rnf UpdateTrafficDistribution' {..} =
    Prelude.rnf telephonyConfig
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders UpdateTrafficDistribution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateTrafficDistribution where
  toJSON UpdateTrafficDistribution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TelephonyConfig" Data..=)
              Prelude.<$> telephonyConfig
          ]
      )

instance Data.ToPath UpdateTrafficDistribution where
  toPath UpdateTrafficDistribution' {..} =
    Prelude.mconcat
      ["/traffic-distribution/", Data.toBS id]

instance Data.ToQuery UpdateTrafficDistribution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTrafficDistributionResponse' smart constructor.
data UpdateTrafficDistributionResponse = UpdateTrafficDistributionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTrafficDistributionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateTrafficDistributionResponse_httpStatus' - The response's http status code.
newUpdateTrafficDistributionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateTrafficDistributionResponse
newUpdateTrafficDistributionResponse pHttpStatus_ =
  UpdateTrafficDistributionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateTrafficDistributionResponse_httpStatus :: Lens.Lens' UpdateTrafficDistributionResponse Prelude.Int
updateTrafficDistributionResponse_httpStatus = Lens.lens (\UpdateTrafficDistributionResponse' {httpStatus} -> httpStatus) (\s@UpdateTrafficDistributionResponse' {} a -> s {httpStatus = a} :: UpdateTrafficDistributionResponse)

instance
  Prelude.NFData
    UpdateTrafficDistributionResponse
  where
  rnf UpdateTrafficDistributionResponse' {..} =
    Prelude.rnf httpStatus
