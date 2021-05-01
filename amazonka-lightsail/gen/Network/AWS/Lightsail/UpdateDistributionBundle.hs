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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateDistributionBundle' smart constructor.
data UpdateDistributionBundle = UpdateDistributionBundle'
  { -- | The bundle ID of the new bundle to apply to your distribution.
    --
    -- Use the @GetDistributionBundles@ action to get a list of distribution
    -- bundle IDs that you can specify.
    bundleId :: Prelude.Maybe Prelude.Text,
    -- | The name of the distribution for which to update the bundle.
    --
    -- Use the @GetDistributions@ action to get a list of distribution names
    -- that you can specify.
    distributionName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { bundleId =
        Prelude.Nothing,
      distributionName = Prelude.Nothing
    }

-- | The bundle ID of the new bundle to apply to your distribution.
--
-- Use the @GetDistributionBundles@ action to get a list of distribution
-- bundle IDs that you can specify.
updateDistributionBundle_bundleId :: Lens.Lens' UpdateDistributionBundle (Prelude.Maybe Prelude.Text)
updateDistributionBundle_bundleId = Lens.lens (\UpdateDistributionBundle' {bundleId} -> bundleId) (\s@UpdateDistributionBundle' {} a -> s {bundleId = a} :: UpdateDistributionBundle)

-- | The name of the distribution for which to update the bundle.
--
-- Use the @GetDistributions@ action to get a list of distribution names
-- that you can specify.
updateDistributionBundle_distributionName :: Lens.Lens' UpdateDistributionBundle (Prelude.Maybe Prelude.Text)
updateDistributionBundle_distributionName = Lens.lens (\UpdateDistributionBundle' {distributionName} -> distributionName) (\s@UpdateDistributionBundle' {} a -> s {distributionName = a} :: UpdateDistributionBundle)

instance Prelude.AWSRequest UpdateDistributionBundle where
  type
    Rs UpdateDistributionBundle =
      UpdateDistributionBundleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDistributionBundleResponse'
            Prelude.<$> (x Prelude..?> "operation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDistributionBundle

instance Prelude.NFData UpdateDistributionBundle

instance Prelude.ToHeaders UpdateDistributionBundle where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Lightsail_20161128.UpdateDistributionBundle" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateDistributionBundle where
  toJSON UpdateDistributionBundle' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("bundleId" Prelude..=) Prelude.<$> bundleId,
            ("distributionName" Prelude..=)
              Prelude.<$> distributionName
          ]
      )

instance Prelude.ToPath UpdateDistributionBundle where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateDistributionBundle where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDistributionBundleResponse' smart constructor.
data UpdateDistributionBundleResponse = UpdateDistributionBundleResponse'
  { operation :: Prelude.Maybe Operation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdateDistributionBundleResponse
newUpdateDistributionBundleResponse pHttpStatus_ =
  UpdateDistributionBundleResponse'
    { operation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateDistributionBundleResponse_operation :: Lens.Lens' UpdateDistributionBundleResponse (Prelude.Maybe Operation)
updateDistributionBundleResponse_operation = Lens.lens (\UpdateDistributionBundleResponse' {operation} -> operation) (\s@UpdateDistributionBundleResponse' {} a -> s {operation = a} :: UpdateDistributionBundleResponse)

-- | The response's http status code.
updateDistributionBundleResponse_httpStatus :: Lens.Lens' UpdateDistributionBundleResponse Prelude.Int
updateDistributionBundleResponse_httpStatus = Lens.lens (\UpdateDistributionBundleResponse' {httpStatus} -> httpStatus) (\s@UpdateDistributionBundleResponse' {} a -> s {httpStatus = a} :: UpdateDistributionBundleResponse)

instance
  Prelude.NFData
    UpdateDistributionBundleResponse
