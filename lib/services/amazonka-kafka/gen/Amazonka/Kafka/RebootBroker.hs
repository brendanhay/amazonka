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
-- Module      : Amazonka.Kafka.RebootBroker
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots brokers.
module Amazonka.Kafka.RebootBroker
  ( -- * Creating a Request
    RebootBroker (..),
    newRebootBroker,

    -- * Request Lenses
    rebootBroker_clusterArn,
    rebootBroker_brokerIds,

    -- * Destructuring the Response
    RebootBrokerResponse (..),
    newRebootBrokerResponse,

    -- * Response Lenses
    rebootBrokerResponse_clusterArn,
    rebootBrokerResponse_clusterOperationArn,
    rebootBrokerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Reboots a node.
--
-- /See:/ 'newRebootBroker' smart constructor.
data RebootBroker = RebootBroker'
  { -- | The Amazon Resource Name (ARN) of the cluster to be updated.
    clusterArn :: Prelude.Text,
    -- | The list of broker IDs to be rebooted. The reboot-broker operation
    -- supports rebooting one broker at a time.
    brokerIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RebootBroker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'rebootBroker_clusterArn' - The Amazon Resource Name (ARN) of the cluster to be updated.
--
-- 'brokerIds', 'rebootBroker_brokerIds' - The list of broker IDs to be rebooted. The reboot-broker operation
-- supports rebooting one broker at a time.
newRebootBroker ::
  -- | 'clusterArn'
  Prelude.Text ->
  RebootBroker
newRebootBroker pClusterArn_ =
  RebootBroker'
    { clusterArn = pClusterArn_,
      brokerIds = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) of the cluster to be updated.
rebootBroker_clusterArn :: Lens.Lens' RebootBroker Prelude.Text
rebootBroker_clusterArn = Lens.lens (\RebootBroker' {clusterArn} -> clusterArn) (\s@RebootBroker' {} a -> s {clusterArn = a} :: RebootBroker)

-- | The list of broker IDs to be rebooted. The reboot-broker operation
-- supports rebooting one broker at a time.
rebootBroker_brokerIds :: Lens.Lens' RebootBroker [Prelude.Text]
rebootBroker_brokerIds = Lens.lens (\RebootBroker' {brokerIds} -> brokerIds) (\s@RebootBroker' {} a -> s {brokerIds = a} :: RebootBroker) Prelude.. Lens.coerced

instance Core.AWSRequest RebootBroker where
  type AWSResponse RebootBroker = RebootBrokerResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RebootBrokerResponse'
            Prelude.<$> (x Data..?> "clusterArn")
            Prelude.<*> (x Data..?> "clusterOperationArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RebootBroker where
  hashWithSalt _salt RebootBroker' {..} =
    _salt `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` brokerIds

instance Prelude.NFData RebootBroker where
  rnf RebootBroker' {..} =
    Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf brokerIds

instance Data.ToHeaders RebootBroker where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RebootBroker where
  toJSON RebootBroker' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("brokerIds" Data..= brokerIds)]
      )

instance Data.ToPath RebootBroker where
  toPath RebootBroker' {..} =
    Prelude.mconcat
      [ "/v1/clusters/",
        Data.toBS clusterArn,
        "/reboot-broker"
      ]

instance Data.ToQuery RebootBroker where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRebootBrokerResponse' smart constructor.
data RebootBrokerResponse = RebootBrokerResponse'
  { -- | The Amazon Resource Name (ARN) of the cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the cluster operation.
    clusterOperationArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RebootBrokerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'rebootBrokerResponse_clusterArn' - The Amazon Resource Name (ARN) of the cluster.
--
-- 'clusterOperationArn', 'rebootBrokerResponse_clusterOperationArn' - The Amazon Resource Name (ARN) of the cluster operation.
--
-- 'httpStatus', 'rebootBrokerResponse_httpStatus' - The response's http status code.
newRebootBrokerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RebootBrokerResponse
newRebootBrokerResponse pHttpStatus_ =
  RebootBrokerResponse'
    { clusterArn = Prelude.Nothing,
      clusterOperationArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the cluster.
rebootBrokerResponse_clusterArn :: Lens.Lens' RebootBrokerResponse (Prelude.Maybe Prelude.Text)
rebootBrokerResponse_clusterArn = Lens.lens (\RebootBrokerResponse' {clusterArn} -> clusterArn) (\s@RebootBrokerResponse' {} a -> s {clusterArn = a} :: RebootBrokerResponse)

-- | The Amazon Resource Name (ARN) of the cluster operation.
rebootBrokerResponse_clusterOperationArn :: Lens.Lens' RebootBrokerResponse (Prelude.Maybe Prelude.Text)
rebootBrokerResponse_clusterOperationArn = Lens.lens (\RebootBrokerResponse' {clusterOperationArn} -> clusterOperationArn) (\s@RebootBrokerResponse' {} a -> s {clusterOperationArn = a} :: RebootBrokerResponse)

-- | The response's http status code.
rebootBrokerResponse_httpStatus :: Lens.Lens' RebootBrokerResponse Prelude.Int
rebootBrokerResponse_httpStatus = Lens.lens (\RebootBrokerResponse' {httpStatus} -> httpStatus) (\s@RebootBrokerResponse' {} a -> s {httpStatus = a} :: RebootBrokerResponse)

instance Prelude.NFData RebootBrokerResponse where
  rnf RebootBrokerResponse' {..} =
    Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf clusterOperationArn
      `Prelude.seq` Prelude.rnf httpStatus
