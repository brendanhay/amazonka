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
-- Module      : Network.AWS.Lightsail.UpdateDistributionBundle
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the bundle of your Amazon Lightsail content delivery network
-- (CDN) distribution.
--
-- A distribution bundle specifies the monthly network transfer quota and
-- monthly cost of your dsitribution.
--
-- Update your distribution\'s bundle if your distribution is going over
-- its monthly network transfer quota and is incurring an overage fee.
--
-- You can update your distribution\'s bundle only one time within your
-- monthly AWS billing cycle. To determine if you can update your
-- distribution\'s bundle, use the @GetDistributions@ action. The
-- @ableToUpdateBundle@ parameter in the result will indicate whether you
-- can currently update your distribution\'s bundle.
module Network.AWS.Lightsail.UpdateDistributionBundle
  ( -- * Creating a Request
    UpdateDistributionBundle (..),
    newUpdateDistributionBundle,

    -- * Request Lenses
    updateDistributionBundle_bundleId,
    updateDistributionBundle_distributionName,

    -- * Destructuring the Response
    UpdateDistributionBundleResponse (..),
    newUpdateDistributionBundleResponse,

    -- * Response Lenses
    updateDistributionBundleResponse_operation,
    updateDistributionBundleResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateDistributionBundle' smart constructor.
data UpdateDistributionBundle = UpdateDistributionBundle'
  { -- | The bundle ID of the new bundle to apply to your distribution.
    --
    -- Use the @GetDistributionBundles@ action to get a list of distribution
    -- bundle IDs that you can specify.
    bundleId :: Core.Maybe Core.Text,
    -- | The name of the distribution for which to update the bundle.
    --
    -- Use the @GetDistributions@ action to get a list of distribution names
    -- that you can specify.
    distributionName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDistributionBundle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bundleId', 'updateDistributionBundle_bundleId' - The bundle ID of the new bundle to apply to your distribution.
--
-- Use the @GetDistributionBundles@ action to get a list of distribution
-- bundle IDs that you can specify.
--
-- 'distributionName', 'updateDistributionBundle_distributionName' - The name of the distribution for which to update the bundle.
--
-- Use the @GetDistributions@ action to get a list of distribution names
-- that you can specify.
newUpdateDistributionBundle ::
  UpdateDistributionBundle
newUpdateDistributionBundle =
  UpdateDistributionBundle'
    { bundleId = Core.Nothing,
      distributionName = Core.Nothing
    }

-- | The bundle ID of the new bundle to apply to your distribution.
--
-- Use the @GetDistributionBundles@ action to get a list of distribution
-- bundle IDs that you can specify.
updateDistributionBundle_bundleId :: Lens.Lens' UpdateDistributionBundle (Core.Maybe Core.Text)
updateDistributionBundle_bundleId = Lens.lens (\UpdateDistributionBundle' {bundleId} -> bundleId) (\s@UpdateDistributionBundle' {} a -> s {bundleId = a} :: UpdateDistributionBundle)

-- | The name of the distribution for which to update the bundle.
--
-- Use the @GetDistributions@ action to get a list of distribution names
-- that you can specify.
updateDistributionBundle_distributionName :: Lens.Lens' UpdateDistributionBundle (Core.Maybe Core.Text)
updateDistributionBundle_distributionName = Lens.lens (\UpdateDistributionBundle' {distributionName} -> distributionName) (\s@UpdateDistributionBundle' {} a -> s {distributionName = a} :: UpdateDistributionBundle)

instance Core.AWSRequest UpdateDistributionBundle where
  type
    AWSResponse UpdateDistributionBundle =
      UpdateDistributionBundleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDistributionBundleResponse'
            Core.<$> (x Core..?> "operation")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateDistributionBundle

instance Core.NFData UpdateDistributionBundle

instance Core.ToHeaders UpdateDistributionBundle where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.UpdateDistributionBundle" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateDistributionBundle where
  toJSON UpdateDistributionBundle' {..} =
    Core.object
      ( Core.catMaybes
          [ ("bundleId" Core..=) Core.<$> bundleId,
            ("distributionName" Core..=)
              Core.<$> distributionName
          ]
      )

instance Core.ToPath UpdateDistributionBundle where
  toPath = Core.const "/"

instance Core.ToQuery UpdateDistributionBundle where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateDistributionBundleResponse' smart constructor.
data UpdateDistributionBundleResponse = UpdateDistributionBundleResponse'
  { operation :: Core.Maybe Operation,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDistributionBundleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operation', 'updateDistributionBundleResponse_operation' - Undocumented member.
--
-- 'httpStatus', 'updateDistributionBundleResponse_httpStatus' - The response's http status code.
newUpdateDistributionBundleResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateDistributionBundleResponse
newUpdateDistributionBundleResponse pHttpStatus_ =
  UpdateDistributionBundleResponse'
    { operation =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateDistributionBundleResponse_operation :: Lens.Lens' UpdateDistributionBundleResponse (Core.Maybe Operation)
updateDistributionBundleResponse_operation = Lens.lens (\UpdateDistributionBundleResponse' {operation} -> operation) (\s@UpdateDistributionBundleResponse' {} a -> s {operation = a} :: UpdateDistributionBundleResponse)

-- | The response's http status code.
updateDistributionBundleResponse_httpStatus :: Lens.Lens' UpdateDistributionBundleResponse Core.Int
updateDistributionBundleResponse_httpStatus = Lens.lens (\UpdateDistributionBundleResponse' {httpStatus} -> httpStatus) (\s@UpdateDistributionBundleResponse' {} a -> s {httpStatus = a} :: UpdateDistributionBundleResponse)

instance Core.NFData UpdateDistributionBundleResponse
