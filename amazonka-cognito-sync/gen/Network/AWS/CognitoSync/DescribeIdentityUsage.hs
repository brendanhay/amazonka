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
-- Module      : Network.AWS.CognitoSync.DescribeIdentityUsage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets usage information for an identity, including number of datasets and
-- data usage.
--
-- This API can be called with temporary user credentials provided by
-- Cognito Identity or with developer credentials.
module Network.AWS.CognitoSync.DescribeIdentityUsage
  ( -- * Creating a Request
    DescribeIdentityUsage (..),
    newDescribeIdentityUsage,

    -- * Request Lenses
    describeIdentityUsage_identityPoolId,
    describeIdentityUsage_identityId,

    -- * Destructuring the Response
    DescribeIdentityUsageResponse (..),
    newDescribeIdentityUsageResponse,

    -- * Response Lenses
    describeIdentityUsageResponse_identityUsage,
    describeIdentityUsageResponse_httpStatus,
  )
where

import Network.AWS.CognitoSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request for information about the usage of an identity pool.
--
-- /See:/ 'newDescribeIdentityUsage' smart constructor.
data DescribeIdentityUsage = DescribeIdentityUsage'
  { -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityPoolId :: Prelude.Text,
    -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeIdentityUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolId', 'describeIdentityUsage_identityPoolId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
--
-- 'identityId', 'describeIdentityUsage_identityId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
newDescribeIdentityUsage ::
  -- | 'identityPoolId'
  Prelude.Text ->
  -- | 'identityId'
  Prelude.Text ->
  DescribeIdentityUsage
newDescribeIdentityUsage
  pIdentityPoolId_
  pIdentityId_ =
    DescribeIdentityUsage'
      { identityPoolId =
          pIdentityPoolId_,
        identityId = pIdentityId_
      }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
describeIdentityUsage_identityPoolId :: Lens.Lens' DescribeIdentityUsage Prelude.Text
describeIdentityUsage_identityPoolId = Lens.lens (\DescribeIdentityUsage' {identityPoolId} -> identityPoolId) (\s@DescribeIdentityUsage' {} a -> s {identityPoolId = a} :: DescribeIdentityUsage)

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
describeIdentityUsage_identityId :: Lens.Lens' DescribeIdentityUsage Prelude.Text
describeIdentityUsage_identityId = Lens.lens (\DescribeIdentityUsage' {identityId} -> identityId) (\s@DescribeIdentityUsage' {} a -> s {identityId = a} :: DescribeIdentityUsage)

instance Prelude.AWSRequest DescribeIdentityUsage where
  type
    Rs DescribeIdentityUsage =
      DescribeIdentityUsageResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeIdentityUsageResponse'
            Prelude.<$> (x Prelude..?> "IdentityUsage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeIdentityUsage

instance Prelude.NFData DescribeIdentityUsage

instance Prelude.ToHeaders DescribeIdentityUsage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DescribeIdentityUsage where
  toPath DescribeIdentityUsage' {..} =
    Prelude.mconcat
      [ "/identitypools/",
        Prelude.toBS identityPoolId,
        "/identities/",
        Prelude.toBS identityId
      ]

instance Prelude.ToQuery DescribeIdentityUsage where
  toQuery = Prelude.const Prelude.mempty

-- | The response to a successful DescribeIdentityUsage request.
--
-- /See:/ 'newDescribeIdentityUsageResponse' smart constructor.
data DescribeIdentityUsageResponse = DescribeIdentityUsageResponse'
  { -- | Usage information for the identity.
    identityUsage :: Prelude.Maybe IdentityUsage,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeIdentityUsageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityUsage', 'describeIdentityUsageResponse_identityUsage' - Usage information for the identity.
--
-- 'httpStatus', 'describeIdentityUsageResponse_httpStatus' - The response's http status code.
newDescribeIdentityUsageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeIdentityUsageResponse
newDescribeIdentityUsageResponse pHttpStatus_ =
  DescribeIdentityUsageResponse'
    { identityUsage =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Usage information for the identity.
describeIdentityUsageResponse_identityUsage :: Lens.Lens' DescribeIdentityUsageResponse (Prelude.Maybe IdentityUsage)
describeIdentityUsageResponse_identityUsage = Lens.lens (\DescribeIdentityUsageResponse' {identityUsage} -> identityUsage) (\s@DescribeIdentityUsageResponse' {} a -> s {identityUsage = a} :: DescribeIdentityUsageResponse)

-- | The response's http status code.
describeIdentityUsageResponse_httpStatus :: Lens.Lens' DescribeIdentityUsageResponse Prelude.Int
describeIdentityUsageResponse_httpStatus = Lens.lens (\DescribeIdentityUsageResponse' {httpStatus} -> httpStatus) (\s@DescribeIdentityUsageResponse' {} a -> s {httpStatus = a} :: DescribeIdentityUsageResponse)

instance Prelude.NFData DescribeIdentityUsageResponse
