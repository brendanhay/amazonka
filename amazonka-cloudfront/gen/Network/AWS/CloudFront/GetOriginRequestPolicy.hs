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
-- Module      : Network.AWS.CloudFront.GetOriginRequestPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an origin request policy, including the following metadata:
--
-- -   The policy’s identifier.
--
-- -   The date and time when the policy was last modified.
--
-- To get an origin request policy, you must provide the policy’s
-- identifier. If the origin request policy is attached to a distribution’s
-- cache behavior, you can get the policy’s identifier using
-- @ListDistributions@ or @GetDistribution@. If the origin request policy
-- is not attached to a cache behavior, you can get the identifier using
-- @ListOriginRequestPolicies@.
module Network.AWS.CloudFront.GetOriginRequestPolicy
  ( -- * Creating a Request
    GetOriginRequestPolicy (..),
    newGetOriginRequestPolicy,

    -- * Request Lenses
    getOriginRequestPolicy_id,

    -- * Destructuring the Response
    GetOriginRequestPolicyResponse (..),
    newGetOriginRequestPolicyResponse,

    -- * Response Lenses
    getOriginRequestPolicyResponse_eTag,
    getOriginRequestPolicyResponse_originRequestPolicy,
    getOriginRequestPolicyResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetOriginRequestPolicy' smart constructor.
data GetOriginRequestPolicy = GetOriginRequestPolicy'
  { -- | The unique identifier for the origin request policy. If the origin
    -- request policy is attached to a distribution’s cache behavior, you can
    -- get the policy’s identifier using @ListDistributions@ or
    -- @GetDistribution@. If the origin request policy is not attached to a
    -- cache behavior, you can get the identifier using
    -- @ListOriginRequestPolicies@.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetOriginRequestPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getOriginRequestPolicy_id' - The unique identifier for the origin request policy. If the origin
-- request policy is attached to a distribution’s cache behavior, you can
-- get the policy’s identifier using @ListDistributions@ or
-- @GetDistribution@. If the origin request policy is not attached to a
-- cache behavior, you can get the identifier using
-- @ListOriginRequestPolicies@.
newGetOriginRequestPolicy ::
  -- | 'id'
  Core.Text ->
  GetOriginRequestPolicy
newGetOriginRequestPolicy pId_ =
  GetOriginRequestPolicy' {id = pId_}

-- | The unique identifier for the origin request policy. If the origin
-- request policy is attached to a distribution’s cache behavior, you can
-- get the policy’s identifier using @ListDistributions@ or
-- @GetDistribution@. If the origin request policy is not attached to a
-- cache behavior, you can get the identifier using
-- @ListOriginRequestPolicies@.
getOriginRequestPolicy_id :: Lens.Lens' GetOriginRequestPolicy Core.Text
getOriginRequestPolicy_id = Lens.lens (\GetOriginRequestPolicy' {id} -> id) (\s@GetOriginRequestPolicy' {} a -> s {id = a} :: GetOriginRequestPolicy)

instance Core.AWSRequest GetOriginRequestPolicy where
  type
    AWSResponse GetOriginRequestPolicy =
      GetOriginRequestPolicyResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetOriginRequestPolicyResponse'
            Core.<$> (h Core..#? "ETag")
            Core.<*> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetOriginRequestPolicy

instance Core.NFData GetOriginRequestPolicy

instance Core.ToHeaders GetOriginRequestPolicy where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetOriginRequestPolicy where
  toPath GetOriginRequestPolicy' {..} =
    Core.mconcat
      ["/2020-05-31/origin-request-policy/", Core.toBS id]

instance Core.ToQuery GetOriginRequestPolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetOriginRequestPolicyResponse' smart constructor.
data GetOriginRequestPolicyResponse = GetOriginRequestPolicyResponse'
  { -- | The current version of the origin request policy.
    eTag :: Core.Maybe Core.Text,
    -- | The origin request policy.
    originRequestPolicy :: Core.Maybe OriginRequestPolicy,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetOriginRequestPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'getOriginRequestPolicyResponse_eTag' - The current version of the origin request policy.
--
-- 'originRequestPolicy', 'getOriginRequestPolicyResponse_originRequestPolicy' - The origin request policy.
--
-- 'httpStatus', 'getOriginRequestPolicyResponse_httpStatus' - The response's http status code.
newGetOriginRequestPolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetOriginRequestPolicyResponse
newGetOriginRequestPolicyResponse pHttpStatus_ =
  GetOriginRequestPolicyResponse'
    { eTag =
        Core.Nothing,
      originRequestPolicy = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current version of the origin request policy.
getOriginRequestPolicyResponse_eTag :: Lens.Lens' GetOriginRequestPolicyResponse (Core.Maybe Core.Text)
getOriginRequestPolicyResponse_eTag = Lens.lens (\GetOriginRequestPolicyResponse' {eTag} -> eTag) (\s@GetOriginRequestPolicyResponse' {} a -> s {eTag = a} :: GetOriginRequestPolicyResponse)

-- | The origin request policy.
getOriginRequestPolicyResponse_originRequestPolicy :: Lens.Lens' GetOriginRequestPolicyResponse (Core.Maybe OriginRequestPolicy)
getOriginRequestPolicyResponse_originRequestPolicy = Lens.lens (\GetOriginRequestPolicyResponse' {originRequestPolicy} -> originRequestPolicy) (\s@GetOriginRequestPolicyResponse' {} a -> s {originRequestPolicy = a} :: GetOriginRequestPolicyResponse)

-- | The response's http status code.
getOriginRequestPolicyResponse_httpStatus :: Lens.Lens' GetOriginRequestPolicyResponse Core.Int
getOriginRequestPolicyResponse_httpStatus = Lens.lens (\GetOriginRequestPolicyResponse' {httpStatus} -> httpStatus) (\s@GetOriginRequestPolicyResponse' {} a -> s {httpStatus = a} :: GetOriginRequestPolicyResponse)

instance Core.NFData GetOriginRequestPolicyResponse
