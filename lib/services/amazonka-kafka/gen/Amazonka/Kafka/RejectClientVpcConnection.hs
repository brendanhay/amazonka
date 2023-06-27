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
-- Module      : Amazonka.Kafka.RejectClientVpcConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns empty response.
module Amazonka.Kafka.RejectClientVpcConnection
  ( -- * Creating a Request
    RejectClientVpcConnection (..),
    newRejectClientVpcConnection,

    -- * Request Lenses
    rejectClientVpcConnection_vpcConnectionArn,
    rejectClientVpcConnection_clusterArn,

    -- * Destructuring the Response
    RejectClientVpcConnectionResponse (..),
    newRejectClientVpcConnectionResponse,

    -- * Response Lenses
    rejectClientVpcConnectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRejectClientVpcConnection' smart constructor.
data RejectClientVpcConnection = RejectClientVpcConnection'
  { -- | The VPC connection ARN.
    vpcConnectionArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the cluster.
    clusterArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectClientVpcConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConnectionArn', 'rejectClientVpcConnection_vpcConnectionArn' - The VPC connection ARN.
--
-- 'clusterArn', 'rejectClientVpcConnection_clusterArn' - The Amazon Resource Name (ARN) of the cluster.
newRejectClientVpcConnection ::
  -- | 'vpcConnectionArn'
  Prelude.Text ->
  -- | 'clusterArn'
  Prelude.Text ->
  RejectClientVpcConnection
newRejectClientVpcConnection
  pVpcConnectionArn_
  pClusterArn_ =
    RejectClientVpcConnection'
      { vpcConnectionArn =
          pVpcConnectionArn_,
        clusterArn = pClusterArn_
      }

-- | The VPC connection ARN.
rejectClientVpcConnection_vpcConnectionArn :: Lens.Lens' RejectClientVpcConnection Prelude.Text
rejectClientVpcConnection_vpcConnectionArn = Lens.lens (\RejectClientVpcConnection' {vpcConnectionArn} -> vpcConnectionArn) (\s@RejectClientVpcConnection' {} a -> s {vpcConnectionArn = a} :: RejectClientVpcConnection)

-- | The Amazon Resource Name (ARN) of the cluster.
rejectClientVpcConnection_clusterArn :: Lens.Lens' RejectClientVpcConnection Prelude.Text
rejectClientVpcConnection_clusterArn = Lens.lens (\RejectClientVpcConnection' {clusterArn} -> clusterArn) (\s@RejectClientVpcConnection' {} a -> s {clusterArn = a} :: RejectClientVpcConnection)

instance Core.AWSRequest RejectClientVpcConnection where
  type
    AWSResponse RejectClientVpcConnection =
      RejectClientVpcConnectionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RejectClientVpcConnectionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RejectClientVpcConnection where
  hashWithSalt _salt RejectClientVpcConnection' {..} =
    _salt
      `Prelude.hashWithSalt` vpcConnectionArn
      `Prelude.hashWithSalt` clusterArn

instance Prelude.NFData RejectClientVpcConnection where
  rnf RejectClientVpcConnection' {..} =
    Prelude.rnf vpcConnectionArn
      `Prelude.seq` Prelude.rnf clusterArn

instance Data.ToHeaders RejectClientVpcConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RejectClientVpcConnection where
  toJSON RejectClientVpcConnection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("vpcConnectionArn" Data..= vpcConnectionArn)
          ]
      )

instance Data.ToPath RejectClientVpcConnection where
  toPath RejectClientVpcConnection' {..} =
    Prelude.mconcat
      [ "/v1/clusters/",
        Data.toBS clusterArn,
        "/client-vpc-connection"
      ]

instance Data.ToQuery RejectClientVpcConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRejectClientVpcConnectionResponse' smart constructor.
data RejectClientVpcConnectionResponse = RejectClientVpcConnectionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectClientVpcConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'rejectClientVpcConnectionResponse_httpStatus' - The response's http status code.
newRejectClientVpcConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RejectClientVpcConnectionResponse
newRejectClientVpcConnectionResponse pHttpStatus_ =
  RejectClientVpcConnectionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
rejectClientVpcConnectionResponse_httpStatus :: Lens.Lens' RejectClientVpcConnectionResponse Prelude.Int
rejectClientVpcConnectionResponse_httpStatus = Lens.lens (\RejectClientVpcConnectionResponse' {httpStatus} -> httpStatus) (\s@RejectClientVpcConnectionResponse' {} a -> s {httpStatus = a} :: RejectClientVpcConnectionResponse)

instance
  Prelude.NFData
    RejectClientVpcConnectionResponse
  where
  rnf RejectClientVpcConnectionResponse' {..} =
    Prelude.rnf httpStatus
