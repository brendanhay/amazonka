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
-- Module      : Network.AWS.CloudFront.GetDistribution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the information about a distribution.
module Network.AWS.CloudFront.GetDistribution
  ( -- * Creating a Request
    GetDistribution (..),
    newGetDistribution,

    -- * Request Lenses
    getDistribution_id,

    -- * Destructuring the Response
    GetDistributionResponse (..),
    newGetDistributionResponse,

    -- * Response Lenses
    getDistributionResponse_eTag,
    getDistributionResponse_distribution,
    getDistributionResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to get a distribution\'s information.
--
-- /See:/ 'newGetDistribution' smart constructor.
data GetDistribution = GetDistribution'
  { -- | The distribution\'s ID. If the ID is empty, an empty distribution
    -- configuration is returned.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDistribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getDistribution_id' - The distribution\'s ID. If the ID is empty, an empty distribution
-- configuration is returned.
newGetDistribution ::
  -- | 'id'
  Core.Text ->
  GetDistribution
newGetDistribution pId_ = GetDistribution' {id = pId_}

-- | The distribution\'s ID. If the ID is empty, an empty distribution
-- configuration is returned.
getDistribution_id :: Lens.Lens' GetDistribution Core.Text
getDistribution_id = Lens.lens (\GetDistribution' {id} -> id) (\s@GetDistribution' {} a -> s {id = a} :: GetDistribution)

instance Core.AWSRequest GetDistribution where
  type
    AWSResponse GetDistribution =
      GetDistributionResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetDistributionResponse'
            Core.<$> (h Core..#? "ETag")
            Core.<*> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDistribution

instance Core.NFData GetDistribution

instance Core.ToHeaders GetDistribution where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetDistribution where
  toPath GetDistribution' {..} =
    Core.mconcat
      ["/2020-05-31/distribution/", Core.toBS id]

instance Core.ToQuery GetDistribution where
  toQuery = Core.const Core.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'newGetDistributionResponse' smart constructor.
data GetDistributionResponse = GetDistributionResponse'
  { -- | The current version of the distribution\'s information. For example:
    -- @E2QWRUHAPOMQZL@.
    eTag :: Core.Maybe Core.Text,
    -- | The distribution\'s information.
    distribution :: Core.Maybe Distribution,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDistributionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'getDistributionResponse_eTag' - The current version of the distribution\'s information. For example:
-- @E2QWRUHAPOMQZL@.
--
-- 'distribution', 'getDistributionResponse_distribution' - The distribution\'s information.
--
-- 'httpStatus', 'getDistributionResponse_httpStatus' - The response's http status code.
newGetDistributionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetDistributionResponse
newGetDistributionResponse pHttpStatus_ =
  GetDistributionResponse'
    { eTag = Core.Nothing,
      distribution = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current version of the distribution\'s information. For example:
-- @E2QWRUHAPOMQZL@.
getDistributionResponse_eTag :: Lens.Lens' GetDistributionResponse (Core.Maybe Core.Text)
getDistributionResponse_eTag = Lens.lens (\GetDistributionResponse' {eTag} -> eTag) (\s@GetDistributionResponse' {} a -> s {eTag = a} :: GetDistributionResponse)

-- | The distribution\'s information.
getDistributionResponse_distribution :: Lens.Lens' GetDistributionResponse (Core.Maybe Distribution)
getDistributionResponse_distribution = Lens.lens (\GetDistributionResponse' {distribution} -> distribution) (\s@GetDistributionResponse' {} a -> s {distribution = a} :: GetDistributionResponse)

-- | The response's http status code.
getDistributionResponse_httpStatus :: Lens.Lens' GetDistributionResponse Core.Int
getDistributionResponse_httpStatus = Lens.lens (\GetDistributionResponse' {httpStatus} -> httpStatus) (\s@GetDistributionResponse' {} a -> s {httpStatus = a} :: GetDistributionResponse)

instance Core.NFData GetDistributionResponse
