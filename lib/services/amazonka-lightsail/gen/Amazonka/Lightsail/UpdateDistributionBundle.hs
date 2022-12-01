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
-- Module      : Amazonka.Lightsail.UpdateDistributionBundle
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the bundle of your Amazon Lightsail content delivery network
-- (CDN) distribution.
--
-- A distribution bundle specifies the monthly network transfer quota and
-- monthly cost of your distribution.
--
-- Update your distribution\'s bundle if your distribution is going over
-- its monthly network transfer quota and is incurring an overage fee.
--
-- You can update your distribution\'s bundle only one time within your
-- monthly Amazon Web Services billing cycle. To determine if you can
-- update your distribution\'s bundle, use the @GetDistributions@ action.
-- The @ableToUpdateBundle@ parameter in the result will indicate whether
-- you can currently update your distribution\'s bundle.
module Amazonka.Lightsail.UpdateDistributionBundle
  ( -- * Creating a Request
    UpdateDistributionBundle (..),
    newUpdateDistributionBundle,

    -- * Request Lenses
    updateDistributionBundle_distributionName,
    updateDistributionBundle_bundleId,

    -- * Destructuring the Response
    UpdateDistributionBundleResponse (..),
    newUpdateDistributionBundleResponse,

    -- * Response Lenses
    updateDistributionBundleResponse_operation,
    updateDistributionBundleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDistributionBundle' smart constructor.
data UpdateDistributionBundle = UpdateDistributionBundle'
  { -- | The name of the distribution for which to update the bundle.
    --
    -- Use the @GetDistributions@ action to get a list of distribution names
    -- that you can specify.
    distributionName :: Prelude.Maybe Prelude.Text,
    -- | The bundle ID of the new bundle to apply to your distribution.
    --
    -- Use the @GetDistributionBundles@ action to get a list of distribution
    -- bundle IDs that you can specify.
    bundleId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDistributionBundle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distributionName', 'updateDistributionBundle_distributionName' - The name of the distribution for which to update the bundle.
--
-- Use the @GetDistributions@ action to get a list of distribution names
-- that you can specify.
--
-- 'bundleId', 'updateDistributionBundle_bundleId' - The bundle ID of the new bundle to apply to your distribution.
--
-- Use the @GetDistributionBundles@ action to get a list of distribution
-- bundle IDs that you can specify.
newUpdateDistributionBundle ::
  UpdateDistributionBundle
newUpdateDistributionBundle =
  UpdateDistributionBundle'
    { distributionName =
        Prelude.Nothing,
      bundleId = Prelude.Nothing
    }

-- | The name of the distribution for which to update the bundle.
--
-- Use the @GetDistributions@ action to get a list of distribution names
-- that you can specify.
updateDistributionBundle_distributionName :: Lens.Lens' UpdateDistributionBundle (Prelude.Maybe Prelude.Text)
updateDistributionBundle_distributionName = Lens.lens (\UpdateDistributionBundle' {distributionName} -> distributionName) (\s@UpdateDistributionBundle' {} a -> s {distributionName = a} :: UpdateDistributionBundle)

-- | The bundle ID of the new bundle to apply to your distribution.
--
-- Use the @GetDistributionBundles@ action to get a list of distribution
-- bundle IDs that you can specify.
updateDistributionBundle_bundleId :: Lens.Lens' UpdateDistributionBundle (Prelude.Maybe Prelude.Text)
updateDistributionBundle_bundleId = Lens.lens (\UpdateDistributionBundle' {bundleId} -> bundleId) (\s@UpdateDistributionBundle' {} a -> s {bundleId = a} :: UpdateDistributionBundle)

instance Core.AWSRequest UpdateDistributionBundle where
  type
    AWSResponse UpdateDistributionBundle =
      UpdateDistributionBundleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDistributionBundleResponse'
            Prelude.<$> (x Core..?> "operation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDistributionBundle where
  hashWithSalt _salt UpdateDistributionBundle' {..} =
    _salt `Prelude.hashWithSalt` distributionName
      `Prelude.hashWithSalt` bundleId

instance Prelude.NFData UpdateDistributionBundle where
  rnf UpdateDistributionBundle' {..} =
    Prelude.rnf distributionName
      `Prelude.seq` Prelude.rnf bundleId

instance Core.ToHeaders UpdateDistributionBundle where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.UpdateDistributionBundle" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateDistributionBundle where
  toJSON UpdateDistributionBundle' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("distributionName" Core..=)
              Prelude.<$> distributionName,
            ("bundleId" Core..=) Prelude.<$> bundleId
          ]
      )

instance Core.ToPath UpdateDistributionBundle where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateDistributionBundle where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDistributionBundleResponse' smart constructor.
data UpdateDistributionBundleResponse = UpdateDistributionBundleResponse'
  { -- | An object that describes the result of the action, such as the status of
    -- the request, the timestamp of the request, and the resources affected by
    -- the request.
    operation :: Prelude.Maybe Operation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDistributionBundleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operation', 'updateDistributionBundleResponse_operation' - An object that describes the result of the action, such as the status of
-- the request, the timestamp of the request, and the resources affected by
-- the request.
--
-- 'httpStatus', 'updateDistributionBundleResponse_httpStatus' - The response's http status code.
newUpdateDistributionBundleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDistributionBundleResponse
newUpdateDistributionBundleResponse pHttpStatus_ =
  UpdateDistributionBundleResponse'
    { operation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that describes the result of the action, such as the status of
-- the request, the timestamp of the request, and the resources affected by
-- the request.
updateDistributionBundleResponse_operation :: Lens.Lens' UpdateDistributionBundleResponse (Prelude.Maybe Operation)
updateDistributionBundleResponse_operation = Lens.lens (\UpdateDistributionBundleResponse' {operation} -> operation) (\s@UpdateDistributionBundleResponse' {} a -> s {operation = a} :: UpdateDistributionBundleResponse)

-- | The response's http status code.
updateDistributionBundleResponse_httpStatus :: Lens.Lens' UpdateDistributionBundleResponse Prelude.Int
updateDistributionBundleResponse_httpStatus = Lens.lens (\UpdateDistributionBundleResponse' {httpStatus} -> httpStatus) (\s@UpdateDistributionBundleResponse' {} a -> s {httpStatus = a} :: UpdateDistributionBundleResponse)

instance
  Prelude.NFData
    UpdateDistributionBundleResponse
  where
  rnf UpdateDistributionBundleResponse' {..} =
    Prelude.rnf operation
      `Prelude.seq` Prelude.rnf httpStatus
