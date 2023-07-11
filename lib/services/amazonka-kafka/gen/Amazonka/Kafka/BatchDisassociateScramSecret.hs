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
-- Module      : Amazonka.Kafka.BatchDisassociateScramSecret
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates one or more Scram Secrets from an Amazon MSK cluster.
module Amazonka.Kafka.BatchDisassociateScramSecret
  ( -- * Creating a Request
    BatchDisassociateScramSecret (..),
    newBatchDisassociateScramSecret,

    -- * Request Lenses
    batchDisassociateScramSecret_clusterArn,
    batchDisassociateScramSecret_secretArnList,

    -- * Destructuring the Response
    BatchDisassociateScramSecretResponse (..),
    newBatchDisassociateScramSecretResponse,

    -- * Response Lenses
    batchDisassociateScramSecretResponse_clusterArn,
    batchDisassociateScramSecretResponse_unprocessedScramSecrets,
    batchDisassociateScramSecretResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Disassociates sasl scram secrets to cluster.
--
-- /See:/ 'newBatchDisassociateScramSecret' smart constructor.
data BatchDisassociateScramSecret = BatchDisassociateScramSecret'
  { -- | The Amazon Resource Name (ARN) of the cluster to be updated.
    clusterArn :: Prelude.Text,
    -- | List of AWS Secrets Manager secret ARNs.
    secretArnList :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDisassociateScramSecret' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'batchDisassociateScramSecret_clusterArn' - The Amazon Resource Name (ARN) of the cluster to be updated.
--
-- 'secretArnList', 'batchDisassociateScramSecret_secretArnList' - List of AWS Secrets Manager secret ARNs.
newBatchDisassociateScramSecret ::
  -- | 'clusterArn'
  Prelude.Text ->
  BatchDisassociateScramSecret
newBatchDisassociateScramSecret pClusterArn_ =
  BatchDisassociateScramSecret'
    { clusterArn =
        pClusterArn_,
      secretArnList = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) of the cluster to be updated.
batchDisassociateScramSecret_clusterArn :: Lens.Lens' BatchDisassociateScramSecret Prelude.Text
batchDisassociateScramSecret_clusterArn = Lens.lens (\BatchDisassociateScramSecret' {clusterArn} -> clusterArn) (\s@BatchDisassociateScramSecret' {} a -> s {clusterArn = a} :: BatchDisassociateScramSecret)

-- | List of AWS Secrets Manager secret ARNs.
batchDisassociateScramSecret_secretArnList :: Lens.Lens' BatchDisassociateScramSecret [Prelude.Text]
batchDisassociateScramSecret_secretArnList = Lens.lens (\BatchDisassociateScramSecret' {secretArnList} -> secretArnList) (\s@BatchDisassociateScramSecret' {} a -> s {secretArnList = a} :: BatchDisassociateScramSecret) Prelude.. Lens.coerced

instance Core.AWSRequest BatchDisassociateScramSecret where
  type
    AWSResponse BatchDisassociateScramSecret =
      BatchDisassociateScramSecretResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDisassociateScramSecretResponse'
            Prelude.<$> (x Data..?> "clusterArn")
            Prelude.<*> ( x
                            Data..?> "unprocessedScramSecrets"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    BatchDisassociateScramSecret
  where
  hashWithSalt _salt BatchDisassociateScramSecret' {..} =
    _salt
      `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` secretArnList

instance Prelude.NFData BatchDisassociateScramSecret where
  rnf BatchDisassociateScramSecret' {..} =
    Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf secretArnList

instance Data.ToHeaders BatchDisassociateScramSecret where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchDisassociateScramSecret where
  toJSON BatchDisassociateScramSecret' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("secretArnList" Data..= secretArnList)
          ]
      )

instance Data.ToPath BatchDisassociateScramSecret where
  toPath BatchDisassociateScramSecret' {..} =
    Prelude.mconcat
      [ "/v1/clusters/",
        Data.toBS clusterArn,
        "/scram-secrets"
      ]

instance Data.ToQuery BatchDisassociateScramSecret where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDisassociateScramSecretResponse' smart constructor.
data BatchDisassociateScramSecretResponse = BatchDisassociateScramSecretResponse'
  { -- | The Amazon Resource Name (ARN) of the cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | List of errors when disassociating secrets to cluster.
    unprocessedScramSecrets :: Prelude.Maybe [UnprocessedScramSecret],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDisassociateScramSecretResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'batchDisassociateScramSecretResponse_clusterArn' - The Amazon Resource Name (ARN) of the cluster.
--
-- 'unprocessedScramSecrets', 'batchDisassociateScramSecretResponse_unprocessedScramSecrets' - List of errors when disassociating secrets to cluster.
--
-- 'httpStatus', 'batchDisassociateScramSecretResponse_httpStatus' - The response's http status code.
newBatchDisassociateScramSecretResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDisassociateScramSecretResponse
newBatchDisassociateScramSecretResponse pHttpStatus_ =
  BatchDisassociateScramSecretResponse'
    { clusterArn =
        Prelude.Nothing,
      unprocessedScramSecrets =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the cluster.
batchDisassociateScramSecretResponse_clusterArn :: Lens.Lens' BatchDisassociateScramSecretResponse (Prelude.Maybe Prelude.Text)
batchDisassociateScramSecretResponse_clusterArn = Lens.lens (\BatchDisassociateScramSecretResponse' {clusterArn} -> clusterArn) (\s@BatchDisassociateScramSecretResponse' {} a -> s {clusterArn = a} :: BatchDisassociateScramSecretResponse)

-- | List of errors when disassociating secrets to cluster.
batchDisassociateScramSecretResponse_unprocessedScramSecrets :: Lens.Lens' BatchDisassociateScramSecretResponse (Prelude.Maybe [UnprocessedScramSecret])
batchDisassociateScramSecretResponse_unprocessedScramSecrets = Lens.lens (\BatchDisassociateScramSecretResponse' {unprocessedScramSecrets} -> unprocessedScramSecrets) (\s@BatchDisassociateScramSecretResponse' {} a -> s {unprocessedScramSecrets = a} :: BatchDisassociateScramSecretResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchDisassociateScramSecretResponse_httpStatus :: Lens.Lens' BatchDisassociateScramSecretResponse Prelude.Int
batchDisassociateScramSecretResponse_httpStatus = Lens.lens (\BatchDisassociateScramSecretResponse' {httpStatus} -> httpStatus) (\s@BatchDisassociateScramSecretResponse' {} a -> s {httpStatus = a} :: BatchDisassociateScramSecretResponse)

instance
  Prelude.NFData
    BatchDisassociateScramSecretResponse
  where
  rnf BatchDisassociateScramSecretResponse' {..} =
    Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf unprocessedScramSecrets
      `Prelude.seq` Prelude.rnf httpStatus
