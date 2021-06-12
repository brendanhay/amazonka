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
-- Module      : Network.AWS.Mobile.DescribeBundle
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the bundle details for the requested bundle id.
module Network.AWS.Mobile.DescribeBundle
  ( -- * Creating a Request
    DescribeBundle (..),
    newDescribeBundle,

    -- * Request Lenses
    describeBundle_bundleId,

    -- * Destructuring the Response
    DescribeBundleResponse (..),
    newDescribeBundleResponse,

    -- * Response Lenses
    describeBundleResponse_details,
    describeBundleResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Mobile.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request structure to request the details of a specific bundle.
--
-- /See:/ 'newDescribeBundle' smart constructor.
data DescribeBundle = DescribeBundle'
  { -- | Unique bundle identifier.
    bundleId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeBundle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bundleId', 'describeBundle_bundleId' - Unique bundle identifier.
newDescribeBundle ::
  -- | 'bundleId'
  Core.Text ->
  DescribeBundle
newDescribeBundle pBundleId_ =
  DescribeBundle' {bundleId = pBundleId_}

-- | Unique bundle identifier.
describeBundle_bundleId :: Lens.Lens' DescribeBundle Core.Text
describeBundle_bundleId = Lens.lens (\DescribeBundle' {bundleId} -> bundleId) (\s@DescribeBundle' {} a -> s {bundleId = a} :: DescribeBundle)

instance Core.AWSRequest DescribeBundle where
  type
    AWSResponse DescribeBundle =
      DescribeBundleResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBundleResponse'
            Core.<$> (x Core..?> "details")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeBundle

instance Core.NFData DescribeBundle

instance Core.ToHeaders DescribeBundle where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DescribeBundle where
  toPath DescribeBundle' {..} =
    Core.mconcat ["/bundles/", Core.toBS bundleId]

instance Core.ToQuery DescribeBundle where
  toQuery = Core.const Core.mempty

-- | Result structure contains the details of the bundle.
--
-- /See:/ 'newDescribeBundleResponse' smart constructor.
data DescribeBundleResponse = DescribeBundleResponse'
  { -- | The details of the bundle.
    details :: Core.Maybe BundleDetails,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeBundleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'details', 'describeBundleResponse_details' - The details of the bundle.
--
-- 'httpStatus', 'describeBundleResponse_httpStatus' - The response's http status code.
newDescribeBundleResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeBundleResponse
newDescribeBundleResponse pHttpStatus_ =
  DescribeBundleResponse'
    { details = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the bundle.
describeBundleResponse_details :: Lens.Lens' DescribeBundleResponse (Core.Maybe BundleDetails)
describeBundleResponse_details = Lens.lens (\DescribeBundleResponse' {details} -> details) (\s@DescribeBundleResponse' {} a -> s {details = a} :: DescribeBundleResponse)

-- | The response's http status code.
describeBundleResponse_httpStatus :: Lens.Lens' DescribeBundleResponse Core.Int
describeBundleResponse_httpStatus = Lens.lens (\DescribeBundleResponse' {httpStatus} -> httpStatus) (\s@DescribeBundleResponse' {} a -> s {httpStatus = a} :: DescribeBundleResponse)

instance Core.NFData DescribeBundleResponse
