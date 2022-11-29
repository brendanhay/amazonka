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
-- Module      : Amazonka.CognitoSync.DescribeIdentityPoolUsage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets usage details (for example, data storage) about a particular
-- identity pool.
--
-- This API can only be called with developer credentials. You cannot call
-- this API with the temporary user credentials provided by Cognito
-- Identity.
module Amazonka.CognitoSync.DescribeIdentityPoolUsage
  ( -- * Creating a Request
    DescribeIdentityPoolUsage (..),
    newDescribeIdentityPoolUsage,

    -- * Request Lenses
    describeIdentityPoolUsage_identityPoolId,

    -- * Destructuring the Response
    DescribeIdentityPoolUsageResponse (..),
    newDescribeIdentityPoolUsageResponse,

    -- * Response Lenses
    describeIdentityPoolUsageResponse_identityPoolUsage,
    describeIdentityPoolUsageResponse_httpStatus,
  )
where

import Amazonka.CognitoSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request for usage information about the identity pool.
--
-- /See:/ 'newDescribeIdentityPoolUsage' smart constructor.
data DescribeIdentityPoolUsage = DescribeIdentityPoolUsage'
  { -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIdentityPoolUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolId', 'describeIdentityPoolUsage_identityPoolId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
newDescribeIdentityPoolUsage ::
  -- | 'identityPoolId'
  Prelude.Text ->
  DescribeIdentityPoolUsage
newDescribeIdentityPoolUsage pIdentityPoolId_ =
  DescribeIdentityPoolUsage'
    { identityPoolId =
        pIdentityPoolId_
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
describeIdentityPoolUsage_identityPoolId :: Lens.Lens' DescribeIdentityPoolUsage Prelude.Text
describeIdentityPoolUsage_identityPoolId = Lens.lens (\DescribeIdentityPoolUsage' {identityPoolId} -> identityPoolId) (\s@DescribeIdentityPoolUsage' {} a -> s {identityPoolId = a} :: DescribeIdentityPoolUsage)

instance Core.AWSRequest DescribeIdentityPoolUsage where
  type
    AWSResponse DescribeIdentityPoolUsage =
      DescribeIdentityPoolUsageResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeIdentityPoolUsageResponse'
            Prelude.<$> (x Core..?> "IdentityPoolUsage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeIdentityPoolUsage where
  hashWithSalt _salt DescribeIdentityPoolUsage' {..} =
    _salt `Prelude.hashWithSalt` identityPoolId

instance Prelude.NFData DescribeIdentityPoolUsage where
  rnf DescribeIdentityPoolUsage' {..} =
    Prelude.rnf identityPoolId

instance Core.ToHeaders DescribeIdentityPoolUsage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeIdentityPoolUsage where
  toPath DescribeIdentityPoolUsage' {..} =
    Prelude.mconcat
      ["/identitypools/", Core.toBS identityPoolId]

instance Core.ToQuery DescribeIdentityPoolUsage where
  toQuery = Prelude.const Prelude.mempty

-- | Response to a successful DescribeIdentityPoolUsage request.
--
-- /See:/ 'newDescribeIdentityPoolUsageResponse' smart constructor.
data DescribeIdentityPoolUsageResponse = DescribeIdentityPoolUsageResponse'
  { -- | Information about the usage of the identity pool.
    identityPoolUsage :: Prelude.Maybe IdentityPoolUsage,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIdentityPoolUsageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolUsage', 'describeIdentityPoolUsageResponse_identityPoolUsage' - Information about the usage of the identity pool.
--
-- 'httpStatus', 'describeIdentityPoolUsageResponse_httpStatus' - The response's http status code.
newDescribeIdentityPoolUsageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeIdentityPoolUsageResponse
newDescribeIdentityPoolUsageResponse pHttpStatus_ =
  DescribeIdentityPoolUsageResponse'
    { identityPoolUsage =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the usage of the identity pool.
describeIdentityPoolUsageResponse_identityPoolUsage :: Lens.Lens' DescribeIdentityPoolUsageResponse (Prelude.Maybe IdentityPoolUsage)
describeIdentityPoolUsageResponse_identityPoolUsage = Lens.lens (\DescribeIdentityPoolUsageResponse' {identityPoolUsage} -> identityPoolUsage) (\s@DescribeIdentityPoolUsageResponse' {} a -> s {identityPoolUsage = a} :: DescribeIdentityPoolUsageResponse)

-- | The response's http status code.
describeIdentityPoolUsageResponse_httpStatus :: Lens.Lens' DescribeIdentityPoolUsageResponse Prelude.Int
describeIdentityPoolUsageResponse_httpStatus = Lens.lens (\DescribeIdentityPoolUsageResponse' {httpStatus} -> httpStatus) (\s@DescribeIdentityPoolUsageResponse' {} a -> s {httpStatus = a} :: DescribeIdentityPoolUsageResponse)

instance
  Prelude.NFData
    DescribeIdentityPoolUsageResponse
  where
  rnf DescribeIdentityPoolUsageResponse' {..} =
    Prelude.rnf identityPoolUsage
      `Prelude.seq` Prelude.rnf httpStatus
