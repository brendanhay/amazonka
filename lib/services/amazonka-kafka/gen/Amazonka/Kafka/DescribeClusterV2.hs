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
-- Module      : Amazonka.Kafka.DescribeClusterV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of the MSK cluster whose Amazon Resource Name
-- (ARN) is specified in the request.
module Amazonka.Kafka.DescribeClusterV2
  ( -- * Creating a Request
    DescribeClusterV2 (..),
    newDescribeClusterV2,

    -- * Request Lenses
    describeClusterV2_clusterArn,

    -- * Destructuring the Response
    DescribeClusterV2Response (..),
    newDescribeClusterV2Response,

    -- * Response Lenses
    describeClusterV2Response_clusterInfo,
    describeClusterV2Response_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeClusterV2' smart constructor.
data DescribeClusterV2 = DescribeClusterV2'
  { -- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
    clusterArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeClusterV2' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'describeClusterV2_clusterArn' - The Amazon Resource Name (ARN) that uniquely identifies the cluster.
newDescribeClusterV2 ::
  -- | 'clusterArn'
  Prelude.Text ->
  DescribeClusterV2
newDescribeClusterV2 pClusterArn_ =
  DescribeClusterV2' {clusterArn = pClusterArn_}

-- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
describeClusterV2_clusterArn :: Lens.Lens' DescribeClusterV2 Prelude.Text
describeClusterV2_clusterArn = Lens.lens (\DescribeClusterV2' {clusterArn} -> clusterArn) (\s@DescribeClusterV2' {} a -> s {clusterArn = a} :: DescribeClusterV2)

instance Core.AWSRequest DescribeClusterV2 where
  type
    AWSResponse DescribeClusterV2 =
      DescribeClusterV2Response
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeClusterV2Response'
            Prelude.<$> (x Data..?> "clusterInfo")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeClusterV2 where
  hashWithSalt _salt DescribeClusterV2' {..} =
    _salt `Prelude.hashWithSalt` clusterArn

instance Prelude.NFData DescribeClusterV2 where
  rnf DescribeClusterV2' {..} = Prelude.rnf clusterArn

instance Data.ToHeaders DescribeClusterV2 where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeClusterV2 where
  toPath DescribeClusterV2' {..} =
    Prelude.mconcat
      ["/api/v2/clusters/", Data.toBS clusterArn]

instance Data.ToQuery DescribeClusterV2 where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeClusterV2Response' smart constructor.
data DescribeClusterV2Response = DescribeClusterV2Response'
  { -- | The cluster information.
    clusterInfo :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeClusterV2Response' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterInfo', 'describeClusterV2Response_clusterInfo' - The cluster information.
--
-- 'httpStatus', 'describeClusterV2Response_httpStatus' - The response's http status code.
newDescribeClusterV2Response ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeClusterV2Response
newDescribeClusterV2Response pHttpStatus_ =
  DescribeClusterV2Response'
    { clusterInfo =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The cluster information.
describeClusterV2Response_clusterInfo :: Lens.Lens' DescribeClusterV2Response (Prelude.Maybe Cluster)
describeClusterV2Response_clusterInfo = Lens.lens (\DescribeClusterV2Response' {clusterInfo} -> clusterInfo) (\s@DescribeClusterV2Response' {} a -> s {clusterInfo = a} :: DescribeClusterV2Response)

-- | The response's http status code.
describeClusterV2Response_httpStatus :: Lens.Lens' DescribeClusterV2Response Prelude.Int
describeClusterV2Response_httpStatus = Lens.lens (\DescribeClusterV2Response' {httpStatus} -> httpStatus) (\s@DescribeClusterV2Response' {} a -> s {httpStatus = a} :: DescribeClusterV2Response)

instance Prelude.NFData DescribeClusterV2Response where
  rnf DescribeClusterV2Response' {..} =
    Prelude.rnf clusterInfo
      `Prelude.seq` Prelude.rnf httpStatus
