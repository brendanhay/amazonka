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
-- Module      : Amazonka.Kafka.BatchAssociateScramSecret
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates one or more Scram Secrets with an Amazon MSK cluster.
module Amazonka.Kafka.BatchAssociateScramSecret
  ( -- * Creating a Request
    BatchAssociateScramSecret (..),
    newBatchAssociateScramSecret,

    -- * Request Lenses
    batchAssociateScramSecret_clusterArn,
    batchAssociateScramSecret_secretArnList,

    -- * Destructuring the Response
    BatchAssociateScramSecretResponse (..),
    newBatchAssociateScramSecretResponse,

    -- * Response Lenses
    batchAssociateScramSecretResponse_clusterArn,
    batchAssociateScramSecretResponse_unprocessedScramSecrets,
    batchAssociateScramSecretResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Associates sasl scram secrets to cluster.
--
-- /See:/ 'newBatchAssociateScramSecret' smart constructor.
data BatchAssociateScramSecret = BatchAssociateScramSecret'
  { -- | The Amazon Resource Name (ARN) of the cluster to be updated.
    clusterArn :: Prelude.Text,
    -- | List of AWS Secrets Manager secret ARNs.
    secretArnList :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchAssociateScramSecret' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'batchAssociateScramSecret_clusterArn' - The Amazon Resource Name (ARN) of the cluster to be updated.
--
-- 'secretArnList', 'batchAssociateScramSecret_secretArnList' - List of AWS Secrets Manager secret ARNs.
newBatchAssociateScramSecret ::
  -- | 'clusterArn'
  Prelude.Text ->
  BatchAssociateScramSecret
newBatchAssociateScramSecret pClusterArn_ =
  BatchAssociateScramSecret'
    { clusterArn =
        pClusterArn_,
      secretArnList = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) of the cluster to be updated.
batchAssociateScramSecret_clusterArn :: Lens.Lens' BatchAssociateScramSecret Prelude.Text
batchAssociateScramSecret_clusterArn = Lens.lens (\BatchAssociateScramSecret' {clusterArn} -> clusterArn) (\s@BatchAssociateScramSecret' {} a -> s {clusterArn = a} :: BatchAssociateScramSecret)

-- | List of AWS Secrets Manager secret ARNs.
batchAssociateScramSecret_secretArnList :: Lens.Lens' BatchAssociateScramSecret [Prelude.Text]
batchAssociateScramSecret_secretArnList = Lens.lens (\BatchAssociateScramSecret' {secretArnList} -> secretArnList) (\s@BatchAssociateScramSecret' {} a -> s {secretArnList = a} :: BatchAssociateScramSecret) Prelude.. Lens.coerced

instance Core.AWSRequest BatchAssociateScramSecret where
  type
    AWSResponse BatchAssociateScramSecret =
      BatchAssociateScramSecretResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchAssociateScramSecretResponse'
            Prelude.<$> (x Data..?> "clusterArn")
            Prelude.<*> ( x Data..?> "unprocessedScramSecrets"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchAssociateScramSecret where
  hashWithSalt _salt BatchAssociateScramSecret' {..} =
    _salt `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` secretArnList

instance Prelude.NFData BatchAssociateScramSecret where
  rnf BatchAssociateScramSecret' {..} =
    Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf secretArnList

instance Data.ToHeaders BatchAssociateScramSecret where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchAssociateScramSecret where
  toJSON BatchAssociateScramSecret' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("secretArnList" Data..= secretArnList)
          ]
      )

instance Data.ToPath BatchAssociateScramSecret where
  toPath BatchAssociateScramSecret' {..} =
    Prelude.mconcat
      [ "/v1/clusters/",
        Data.toBS clusterArn,
        "/scram-secrets"
      ]

instance Data.ToQuery BatchAssociateScramSecret where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchAssociateScramSecretResponse' smart constructor.
data BatchAssociateScramSecretResponse = BatchAssociateScramSecretResponse'
  { -- | The Amazon Resource Name (ARN) of the cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | List of errors when associating secrets to cluster.
    unprocessedScramSecrets :: Prelude.Maybe [UnprocessedScramSecret],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchAssociateScramSecretResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'batchAssociateScramSecretResponse_clusterArn' - The Amazon Resource Name (ARN) of the cluster.
--
-- 'unprocessedScramSecrets', 'batchAssociateScramSecretResponse_unprocessedScramSecrets' - List of errors when associating secrets to cluster.
--
-- 'httpStatus', 'batchAssociateScramSecretResponse_httpStatus' - The response's http status code.
newBatchAssociateScramSecretResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchAssociateScramSecretResponse
newBatchAssociateScramSecretResponse pHttpStatus_ =
  BatchAssociateScramSecretResponse'
    { clusterArn =
        Prelude.Nothing,
      unprocessedScramSecrets =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the cluster.
batchAssociateScramSecretResponse_clusterArn :: Lens.Lens' BatchAssociateScramSecretResponse (Prelude.Maybe Prelude.Text)
batchAssociateScramSecretResponse_clusterArn = Lens.lens (\BatchAssociateScramSecretResponse' {clusterArn} -> clusterArn) (\s@BatchAssociateScramSecretResponse' {} a -> s {clusterArn = a} :: BatchAssociateScramSecretResponse)

-- | List of errors when associating secrets to cluster.
batchAssociateScramSecretResponse_unprocessedScramSecrets :: Lens.Lens' BatchAssociateScramSecretResponse (Prelude.Maybe [UnprocessedScramSecret])
batchAssociateScramSecretResponse_unprocessedScramSecrets = Lens.lens (\BatchAssociateScramSecretResponse' {unprocessedScramSecrets} -> unprocessedScramSecrets) (\s@BatchAssociateScramSecretResponse' {} a -> s {unprocessedScramSecrets = a} :: BatchAssociateScramSecretResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchAssociateScramSecretResponse_httpStatus :: Lens.Lens' BatchAssociateScramSecretResponse Prelude.Int
batchAssociateScramSecretResponse_httpStatus = Lens.lens (\BatchAssociateScramSecretResponse' {httpStatus} -> httpStatus) (\s@BatchAssociateScramSecretResponse' {} a -> s {httpStatus = a} :: BatchAssociateScramSecretResponse)

instance
  Prelude.NFData
    BatchAssociateScramSecretResponse
  where
  rnf BatchAssociateScramSecretResponse' {..} =
    Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf unprocessedScramSecrets
      `Prelude.seq` Prelude.rnf httpStatus
