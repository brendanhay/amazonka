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
-- Module      : Amazonka.DMS.DescribeRefreshSchemasStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the status of the RefreshSchemas operation.
module Amazonka.DMS.DescribeRefreshSchemasStatus
  ( -- * Creating a Request
    DescribeRefreshSchemasStatus (..),
    newDescribeRefreshSchemasStatus,

    -- * Request Lenses
    describeRefreshSchemasStatus_endpointArn,

    -- * Destructuring the Response
    DescribeRefreshSchemasStatusResponse (..),
    newDescribeRefreshSchemasStatusResponse,

    -- * Response Lenses
    describeRefreshSchemasStatusResponse_refreshSchemasStatus,
    describeRefreshSchemasStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribeRefreshSchemasStatus' smart constructor.
data DescribeRefreshSchemasStatus = DescribeRefreshSchemasStatus'
  { -- | The Amazon Resource Name (ARN) string that uniquely identifies the
    -- endpoint.
    endpointArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRefreshSchemasStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointArn', 'describeRefreshSchemasStatus_endpointArn' - The Amazon Resource Name (ARN) string that uniquely identifies the
-- endpoint.
newDescribeRefreshSchemasStatus ::
  -- | 'endpointArn'
  Prelude.Text ->
  DescribeRefreshSchemasStatus
newDescribeRefreshSchemasStatus pEndpointArn_ =
  DescribeRefreshSchemasStatus'
    { endpointArn =
        pEndpointArn_
    }

-- | The Amazon Resource Name (ARN) string that uniquely identifies the
-- endpoint.
describeRefreshSchemasStatus_endpointArn :: Lens.Lens' DescribeRefreshSchemasStatus Prelude.Text
describeRefreshSchemasStatus_endpointArn = Lens.lens (\DescribeRefreshSchemasStatus' {endpointArn} -> endpointArn) (\s@DescribeRefreshSchemasStatus' {} a -> s {endpointArn = a} :: DescribeRefreshSchemasStatus)

instance Core.AWSRequest DescribeRefreshSchemasStatus where
  type
    AWSResponse DescribeRefreshSchemasStatus =
      DescribeRefreshSchemasStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRefreshSchemasStatusResponse'
            Prelude.<$> (x Data..?> "RefreshSchemasStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeRefreshSchemasStatus
  where
  hashWithSalt _salt DescribeRefreshSchemasStatus' {..} =
    _salt `Prelude.hashWithSalt` endpointArn

instance Prelude.NFData DescribeRefreshSchemasStatus where
  rnf DescribeRefreshSchemasStatus' {..} =
    Prelude.rnf endpointArn

instance Data.ToHeaders DescribeRefreshSchemasStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.DescribeRefreshSchemasStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeRefreshSchemasStatus where
  toJSON DescribeRefreshSchemasStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("EndpointArn" Data..= endpointArn)]
      )

instance Data.ToPath DescribeRefreshSchemasStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeRefreshSchemasStatus where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeRefreshSchemasStatusResponse' smart constructor.
data DescribeRefreshSchemasStatusResponse = DescribeRefreshSchemasStatusResponse'
  { -- | The status of the schema.
    refreshSchemasStatus :: Prelude.Maybe RefreshSchemasStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRefreshSchemasStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'refreshSchemasStatus', 'describeRefreshSchemasStatusResponse_refreshSchemasStatus' - The status of the schema.
--
-- 'httpStatus', 'describeRefreshSchemasStatusResponse_httpStatus' - The response's http status code.
newDescribeRefreshSchemasStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRefreshSchemasStatusResponse
newDescribeRefreshSchemasStatusResponse pHttpStatus_ =
  DescribeRefreshSchemasStatusResponse'
    { refreshSchemasStatus =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the schema.
describeRefreshSchemasStatusResponse_refreshSchemasStatus :: Lens.Lens' DescribeRefreshSchemasStatusResponse (Prelude.Maybe RefreshSchemasStatus)
describeRefreshSchemasStatusResponse_refreshSchemasStatus = Lens.lens (\DescribeRefreshSchemasStatusResponse' {refreshSchemasStatus} -> refreshSchemasStatus) (\s@DescribeRefreshSchemasStatusResponse' {} a -> s {refreshSchemasStatus = a} :: DescribeRefreshSchemasStatusResponse)

-- | The response's http status code.
describeRefreshSchemasStatusResponse_httpStatus :: Lens.Lens' DescribeRefreshSchemasStatusResponse Prelude.Int
describeRefreshSchemasStatusResponse_httpStatus = Lens.lens (\DescribeRefreshSchemasStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeRefreshSchemasStatusResponse' {} a -> s {httpStatus = a} :: DescribeRefreshSchemasStatusResponse)

instance
  Prelude.NFData
    DescribeRefreshSchemasStatusResponse
  where
  rnf DescribeRefreshSchemasStatusResponse' {..} =
    Prelude.rnf refreshSchemasStatus `Prelude.seq`
      Prelude.rnf httpStatus
