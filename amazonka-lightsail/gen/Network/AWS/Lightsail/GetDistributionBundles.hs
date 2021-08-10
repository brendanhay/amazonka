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
-- Module      : Network.AWS.Lightsail.GetDistributionBundles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list bundles that can be applied to you Amazon Lightsail
-- content delivery network (CDN) distributions.
--
-- A distribution bundle specifies the monthly network transfer quota and
-- monthly cost of your dsitribution.
module Network.AWS.Lightsail.GetDistributionBundles
  ( -- * Creating a Request
    GetDistributionBundles (..),
    newGetDistributionBundles,

    -- * Destructuring the Response
    GetDistributionBundlesResponse (..),
    newGetDistributionBundlesResponse,

    -- * Response Lenses
    getDistributionBundlesResponse_bundles,
    getDistributionBundlesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDistributionBundles' smart constructor.
data GetDistributionBundles = GetDistributionBundles'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDistributionBundles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetDistributionBundles ::
  GetDistributionBundles
newGetDistributionBundles = GetDistributionBundles'

instance Core.AWSRequest GetDistributionBundles where
  type
    AWSResponse GetDistributionBundles =
      GetDistributionBundlesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDistributionBundlesResponse'
            Prelude.<$> (x Core..?> "bundles" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDistributionBundles

instance Prelude.NFData GetDistributionBundles

instance Core.ToHeaders GetDistributionBundles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetDistributionBundles" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetDistributionBundles where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath GetDistributionBundles where
  toPath = Prelude.const "/"

instance Core.ToQuery GetDistributionBundles where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDistributionBundlesResponse' smart constructor.
data GetDistributionBundlesResponse = GetDistributionBundlesResponse'
  { -- | An object that describes a distribution bundle.
    bundles :: Prelude.Maybe [DistributionBundle],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDistributionBundlesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bundles', 'getDistributionBundlesResponse_bundles' - An object that describes a distribution bundle.
--
-- 'httpStatus', 'getDistributionBundlesResponse_httpStatus' - The response's http status code.
newGetDistributionBundlesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDistributionBundlesResponse
newGetDistributionBundlesResponse pHttpStatus_ =
  GetDistributionBundlesResponse'
    { bundles =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that describes a distribution bundle.
getDistributionBundlesResponse_bundles :: Lens.Lens' GetDistributionBundlesResponse (Prelude.Maybe [DistributionBundle])
getDistributionBundlesResponse_bundles = Lens.lens (\GetDistributionBundlesResponse' {bundles} -> bundles) (\s@GetDistributionBundlesResponse' {} a -> s {bundles = a} :: GetDistributionBundlesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getDistributionBundlesResponse_httpStatus :: Lens.Lens' GetDistributionBundlesResponse Prelude.Int
getDistributionBundlesResponse_httpStatus = Lens.lens (\GetDistributionBundlesResponse' {httpStatus} -> httpStatus) (\s@GetDistributionBundlesResponse' {} a -> s {httpStatus = a} :: GetDistributionBundlesResponse)

instance
  Prelude.NFData
    GetDistributionBundlesResponse
