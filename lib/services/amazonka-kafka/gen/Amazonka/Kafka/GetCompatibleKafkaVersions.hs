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
-- Module      : Amazonka.Kafka.GetCompatibleKafkaVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the Apache Kafka versions to which you can update the MSK cluster.
module Amazonka.Kafka.GetCompatibleKafkaVersions
  ( -- * Creating a Request
    GetCompatibleKafkaVersions (..),
    newGetCompatibleKafkaVersions,

    -- * Request Lenses
    getCompatibleKafkaVersions_clusterArn,

    -- * Destructuring the Response
    GetCompatibleKafkaVersionsResponse (..),
    newGetCompatibleKafkaVersionsResponse,

    -- * Response Lenses
    getCompatibleKafkaVersionsResponse_compatibleKafkaVersions,
    getCompatibleKafkaVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCompatibleKafkaVersions' smart constructor.
data GetCompatibleKafkaVersions = GetCompatibleKafkaVersions'
  { -- | The Amazon Resource Name (ARN) of the cluster check.
    clusterArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCompatibleKafkaVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'getCompatibleKafkaVersions_clusterArn' - The Amazon Resource Name (ARN) of the cluster check.
newGetCompatibleKafkaVersions ::
  GetCompatibleKafkaVersions
newGetCompatibleKafkaVersions =
  GetCompatibleKafkaVersions'
    { clusterArn =
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the cluster check.
getCompatibleKafkaVersions_clusterArn :: Lens.Lens' GetCompatibleKafkaVersions (Prelude.Maybe Prelude.Text)
getCompatibleKafkaVersions_clusterArn = Lens.lens (\GetCompatibleKafkaVersions' {clusterArn} -> clusterArn) (\s@GetCompatibleKafkaVersions' {} a -> s {clusterArn = a} :: GetCompatibleKafkaVersions)

instance Core.AWSRequest GetCompatibleKafkaVersions where
  type
    AWSResponse GetCompatibleKafkaVersions =
      GetCompatibleKafkaVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCompatibleKafkaVersionsResponse'
            Prelude.<$> ( x
                            Data..?> "compatibleKafkaVersions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCompatibleKafkaVersions where
  hashWithSalt _salt GetCompatibleKafkaVersions' {..} =
    _salt `Prelude.hashWithSalt` clusterArn

instance Prelude.NFData GetCompatibleKafkaVersions where
  rnf GetCompatibleKafkaVersions' {..} =
    Prelude.rnf clusterArn

instance Data.ToHeaders GetCompatibleKafkaVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetCompatibleKafkaVersions where
  toPath =
    Prelude.const "/v1/compatible-kafka-versions"

instance Data.ToQuery GetCompatibleKafkaVersions where
  toQuery GetCompatibleKafkaVersions' {..} =
    Prelude.mconcat ["clusterArn" Data.=: clusterArn]

-- | /See:/ 'newGetCompatibleKafkaVersionsResponse' smart constructor.
data GetCompatibleKafkaVersionsResponse = GetCompatibleKafkaVersionsResponse'
  { -- | A list of CompatibleKafkaVersion objects.
    compatibleKafkaVersions :: Prelude.Maybe [CompatibleKafkaVersion],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCompatibleKafkaVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compatibleKafkaVersions', 'getCompatibleKafkaVersionsResponse_compatibleKafkaVersions' - A list of CompatibleKafkaVersion objects.
--
-- 'httpStatus', 'getCompatibleKafkaVersionsResponse_httpStatus' - The response's http status code.
newGetCompatibleKafkaVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCompatibleKafkaVersionsResponse
newGetCompatibleKafkaVersionsResponse pHttpStatus_ =
  GetCompatibleKafkaVersionsResponse'
    { compatibleKafkaVersions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of CompatibleKafkaVersion objects.
getCompatibleKafkaVersionsResponse_compatibleKafkaVersions :: Lens.Lens' GetCompatibleKafkaVersionsResponse (Prelude.Maybe [CompatibleKafkaVersion])
getCompatibleKafkaVersionsResponse_compatibleKafkaVersions = Lens.lens (\GetCompatibleKafkaVersionsResponse' {compatibleKafkaVersions} -> compatibleKafkaVersions) (\s@GetCompatibleKafkaVersionsResponse' {} a -> s {compatibleKafkaVersions = a} :: GetCompatibleKafkaVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getCompatibleKafkaVersionsResponse_httpStatus :: Lens.Lens' GetCompatibleKafkaVersionsResponse Prelude.Int
getCompatibleKafkaVersionsResponse_httpStatus = Lens.lens (\GetCompatibleKafkaVersionsResponse' {httpStatus} -> httpStatus) (\s@GetCompatibleKafkaVersionsResponse' {} a -> s {httpStatus = a} :: GetCompatibleKafkaVersionsResponse)

instance
  Prelude.NFData
    GetCompatibleKafkaVersionsResponse
  where
  rnf GetCompatibleKafkaVersionsResponse' {..} =
    Prelude.rnf compatibleKafkaVersions
      `Prelude.seq` Prelude.rnf httpStatus
