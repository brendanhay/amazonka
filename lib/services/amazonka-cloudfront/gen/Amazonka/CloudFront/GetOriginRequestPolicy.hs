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
-- Module      : Amazonka.CloudFront.GetOriginRequestPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.CloudFront.GetOriginRequestPolicy
  ( -- * Creating a Request
    GetOriginRequestPolicy (..),
    newGetOriginRequestPolicy,

    -- * Request Lenses
    getOriginRequestPolicy_id,

    -- * Destructuring the Response
    GetOriginRequestPolicyResponse (..),
    newGetOriginRequestPolicyResponse,

    -- * Response Lenses
    getOriginRequestPolicyResponse_originRequestPolicy,
    getOriginRequestPolicyResponse_eTag,
    getOriginRequestPolicyResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetOriginRequestPolicy' smart constructor.
data GetOriginRequestPolicy = GetOriginRequestPolicy'
  { -- | The unique identifier for the origin request policy. If the origin
    -- request policy is attached to a distribution’s cache behavior, you can
    -- get the policy’s identifier using @ListDistributions@ or
    -- @GetDistribution@. If the origin request policy is not attached to a
    -- cache behavior, you can get the identifier using
    -- @ListOriginRequestPolicies@.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetOriginRequestPolicy
newGetOriginRequestPolicy pId_ =
  GetOriginRequestPolicy' {id = pId_}

-- | The unique identifier for the origin request policy. If the origin
-- request policy is attached to a distribution’s cache behavior, you can
-- get the policy’s identifier using @ListDistributions@ or
-- @GetDistribution@. If the origin request policy is not attached to a
-- cache behavior, you can get the identifier using
-- @ListOriginRequestPolicies@.
getOriginRequestPolicy_id :: Lens.Lens' GetOriginRequestPolicy Prelude.Text
getOriginRequestPolicy_id = Lens.lens (\GetOriginRequestPolicy' {id} -> id) (\s@GetOriginRequestPolicy' {} a -> s {id = a} :: GetOriginRequestPolicy)

instance Core.AWSRequest GetOriginRequestPolicy where
  type
    AWSResponse GetOriginRequestPolicy =
      GetOriginRequestPolicyResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetOriginRequestPolicyResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (h Data..#? "ETag")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetOriginRequestPolicy where
  hashWithSalt _salt GetOriginRequestPolicy' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetOriginRequestPolicy where
  rnf GetOriginRequestPolicy' {..} = Prelude.rnf id

instance Data.ToHeaders GetOriginRequestPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetOriginRequestPolicy where
  toPath GetOriginRequestPolicy' {..} =
    Prelude.mconcat
      ["/2020-05-31/origin-request-policy/", Data.toBS id]

instance Data.ToQuery GetOriginRequestPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetOriginRequestPolicyResponse' smart constructor.
data GetOriginRequestPolicyResponse = GetOriginRequestPolicyResponse'
  { -- | The origin request policy.
    originRequestPolicy :: Prelude.Maybe OriginRequestPolicy,
    -- | The current version of the origin request policy.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOriginRequestPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'originRequestPolicy', 'getOriginRequestPolicyResponse_originRequestPolicy' - The origin request policy.
--
-- 'eTag', 'getOriginRequestPolicyResponse_eTag' - The current version of the origin request policy.
--
-- 'httpStatus', 'getOriginRequestPolicyResponse_httpStatus' - The response's http status code.
newGetOriginRequestPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetOriginRequestPolicyResponse
newGetOriginRequestPolicyResponse pHttpStatus_ =
  GetOriginRequestPolicyResponse'
    { originRequestPolicy =
        Prelude.Nothing,
      eTag = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The origin request policy.
getOriginRequestPolicyResponse_originRequestPolicy :: Lens.Lens' GetOriginRequestPolicyResponse (Prelude.Maybe OriginRequestPolicy)
getOriginRequestPolicyResponse_originRequestPolicy = Lens.lens (\GetOriginRequestPolicyResponse' {originRequestPolicy} -> originRequestPolicy) (\s@GetOriginRequestPolicyResponse' {} a -> s {originRequestPolicy = a} :: GetOriginRequestPolicyResponse)

-- | The current version of the origin request policy.
getOriginRequestPolicyResponse_eTag :: Lens.Lens' GetOriginRequestPolicyResponse (Prelude.Maybe Prelude.Text)
getOriginRequestPolicyResponse_eTag = Lens.lens (\GetOriginRequestPolicyResponse' {eTag} -> eTag) (\s@GetOriginRequestPolicyResponse' {} a -> s {eTag = a} :: GetOriginRequestPolicyResponse)

-- | The response's http status code.
getOriginRequestPolicyResponse_httpStatus :: Lens.Lens' GetOriginRequestPolicyResponse Prelude.Int
getOriginRequestPolicyResponse_httpStatus = Lens.lens (\GetOriginRequestPolicyResponse' {httpStatus} -> httpStatus) (\s@GetOriginRequestPolicyResponse' {} a -> s {httpStatus = a} :: GetOriginRequestPolicyResponse)

instance
  Prelude.NFData
    GetOriginRequestPolicyResponse
  where
  rnf GetOriginRequestPolicyResponse' {..} =
    Prelude.rnf originRequestPolicy
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf httpStatus
