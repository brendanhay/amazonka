{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DMS.DescribeRefreshSchemasStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the status of the RefreshSchemas operation.
module Network.AWS.DMS.DescribeRefreshSchemasStatus
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

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeRefreshSchemasStatus' smart constructor.
data DescribeRefreshSchemasStatus = DescribeRefreshSchemasStatus'
  { -- | The Amazon Resource Name (ARN) string that uniquely identifies the
    -- endpoint.
    endpointArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.AWSRequest
    DescribeRefreshSchemasStatus
  where
  type
    Rs DescribeRefreshSchemasStatus =
      DescribeRefreshSchemasStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRefreshSchemasStatusResponse'
            Prelude.<$> (x Prelude..?> "RefreshSchemasStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeRefreshSchemasStatus

instance Prelude.NFData DescribeRefreshSchemasStatus

instance
  Prelude.ToHeaders
    DescribeRefreshSchemasStatus
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonDMSv20160101.DescribeRefreshSchemasStatus" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeRefreshSchemasStatus where
  toJSON DescribeRefreshSchemasStatus' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("EndpointArn" Prelude..= endpointArn)
          ]
      )

instance Prelude.ToPath DescribeRefreshSchemasStatus where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeRefreshSchemasStatus where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
