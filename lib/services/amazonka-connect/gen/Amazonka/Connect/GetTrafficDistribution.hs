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
-- Module      : Amazonka.Connect.GetTrafficDistribution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current traffic distribution for a given traffic
-- distribution group.
module Amazonka.Connect.GetTrafficDistribution
  ( -- * Creating a Request
    GetTrafficDistribution (..),
    newGetTrafficDistribution,

    -- * Request Lenses
    getTrafficDistribution_id,

    -- * Destructuring the Response
    GetTrafficDistributionResponse (..),
    newGetTrafficDistributionResponse,

    -- * Response Lenses
    getTrafficDistributionResponse_arn,
    getTrafficDistributionResponse_id,
    getTrafficDistributionResponse_telephonyConfig,
    getTrafficDistributionResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetTrafficDistribution' smart constructor.
data GetTrafficDistribution = GetTrafficDistribution'
  { -- | The identifier of the traffic distribution group.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTrafficDistribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getTrafficDistribution_id' - The identifier of the traffic distribution group.
newGetTrafficDistribution ::
  -- | 'id'
  Prelude.Text ->
  GetTrafficDistribution
newGetTrafficDistribution pId_ =
  GetTrafficDistribution' {id = pId_}

-- | The identifier of the traffic distribution group.
getTrafficDistribution_id :: Lens.Lens' GetTrafficDistribution Prelude.Text
getTrafficDistribution_id = Lens.lens (\GetTrafficDistribution' {id} -> id) (\s@GetTrafficDistribution' {} a -> s {id = a} :: GetTrafficDistribution)

instance Core.AWSRequest GetTrafficDistribution where
  type
    AWSResponse GetTrafficDistribution =
      GetTrafficDistributionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTrafficDistributionResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "TelephonyConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTrafficDistribution where
  hashWithSalt _salt GetTrafficDistribution' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetTrafficDistribution where
  rnf GetTrafficDistribution' {..} = Prelude.rnf id

instance Data.ToHeaders GetTrafficDistribution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetTrafficDistribution where
  toPath GetTrafficDistribution' {..} =
    Prelude.mconcat
      ["/traffic-distribution/", Data.toBS id]

instance Data.ToQuery GetTrafficDistribution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTrafficDistributionResponse' smart constructor.
data GetTrafficDistributionResponse = GetTrafficDistributionResponse'
  { -- | The Amazon Resource Name (ARN) of the traffic distribution group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the traffic distribution group. This can be the ID or
    -- the ARN if the API is being called in the Region where the traffic
    -- distribution group was created. The ARN must be provided if the call is
    -- from the replicated Region.
    id :: Prelude.Maybe Prelude.Text,
    -- | The distribution of traffic between the instance and its replicas.
    telephonyConfig :: Prelude.Maybe TelephonyConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTrafficDistributionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getTrafficDistributionResponse_arn' - The Amazon Resource Name (ARN) of the traffic distribution group.
--
-- 'id', 'getTrafficDistributionResponse_id' - The identifier of the traffic distribution group. This can be the ID or
-- the ARN if the API is being called in the Region where the traffic
-- distribution group was created. The ARN must be provided if the call is
-- from the replicated Region.
--
-- 'telephonyConfig', 'getTrafficDistributionResponse_telephonyConfig' - The distribution of traffic between the instance and its replicas.
--
-- 'httpStatus', 'getTrafficDistributionResponse_httpStatus' - The response's http status code.
newGetTrafficDistributionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTrafficDistributionResponse
newGetTrafficDistributionResponse pHttpStatus_ =
  GetTrafficDistributionResponse'
    { arn =
        Prelude.Nothing,
      id = Prelude.Nothing,
      telephonyConfig = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the traffic distribution group.
getTrafficDistributionResponse_arn :: Lens.Lens' GetTrafficDistributionResponse (Prelude.Maybe Prelude.Text)
getTrafficDistributionResponse_arn = Lens.lens (\GetTrafficDistributionResponse' {arn} -> arn) (\s@GetTrafficDistributionResponse' {} a -> s {arn = a} :: GetTrafficDistributionResponse)

-- | The identifier of the traffic distribution group. This can be the ID or
-- the ARN if the API is being called in the Region where the traffic
-- distribution group was created. The ARN must be provided if the call is
-- from the replicated Region.
getTrafficDistributionResponse_id :: Lens.Lens' GetTrafficDistributionResponse (Prelude.Maybe Prelude.Text)
getTrafficDistributionResponse_id = Lens.lens (\GetTrafficDistributionResponse' {id} -> id) (\s@GetTrafficDistributionResponse' {} a -> s {id = a} :: GetTrafficDistributionResponse)

-- | The distribution of traffic between the instance and its replicas.
getTrafficDistributionResponse_telephonyConfig :: Lens.Lens' GetTrafficDistributionResponse (Prelude.Maybe TelephonyConfig)
getTrafficDistributionResponse_telephonyConfig = Lens.lens (\GetTrafficDistributionResponse' {telephonyConfig} -> telephonyConfig) (\s@GetTrafficDistributionResponse' {} a -> s {telephonyConfig = a} :: GetTrafficDistributionResponse)

-- | The response's http status code.
getTrafficDistributionResponse_httpStatus :: Lens.Lens' GetTrafficDistributionResponse Prelude.Int
getTrafficDistributionResponse_httpStatus = Lens.lens (\GetTrafficDistributionResponse' {httpStatus} -> httpStatus) (\s@GetTrafficDistributionResponse' {} a -> s {httpStatus = a} :: GetTrafficDistributionResponse)

instance
  Prelude.NFData
    GetTrafficDistributionResponse
  where
  rnf GetTrafficDistributionResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf telephonyConfig
      `Prelude.seq` Prelude.rnf httpStatus
