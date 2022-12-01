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
-- Module      : Amazonka.CloudFront.GetResponseHeadersPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a response headers policy, including metadata (the policy’s
-- identifier and the date and time when the policy was last modified).
--
-- To get a response headers policy, you must provide the policy’s
-- identifier. If the response headers policy is attached to a
-- distribution’s cache behavior, you can get the policy’s identifier using
-- @ListDistributions@ or @GetDistribution@. If the response headers policy
-- is not attached to a cache behavior, you can get the identifier using
-- @ListResponseHeadersPolicies@.
module Amazonka.CloudFront.GetResponseHeadersPolicy
  ( -- * Creating a Request
    GetResponseHeadersPolicy (..),
    newGetResponseHeadersPolicy,

    -- * Request Lenses
    getResponseHeadersPolicy_id,

    -- * Destructuring the Response
    GetResponseHeadersPolicyResponse (..),
    newGetResponseHeadersPolicyResponse,

    -- * Response Lenses
    getResponseHeadersPolicyResponse_responseHeadersPolicy,
    getResponseHeadersPolicyResponse_eTag,
    getResponseHeadersPolicyResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetResponseHeadersPolicy' smart constructor.
data GetResponseHeadersPolicy = GetResponseHeadersPolicy'
  { -- | The identifier for the response headers policy.
    --
    -- If the response headers policy is attached to a distribution’s cache
    -- behavior, you can get the policy’s identifier using @ListDistributions@
    -- or @GetDistribution@. If the response headers policy is not attached to
    -- a cache behavior, you can get the identifier using
    -- @ListResponseHeadersPolicies@.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResponseHeadersPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getResponseHeadersPolicy_id' - The identifier for the response headers policy.
--
-- If the response headers policy is attached to a distribution’s cache
-- behavior, you can get the policy’s identifier using @ListDistributions@
-- or @GetDistribution@. If the response headers policy is not attached to
-- a cache behavior, you can get the identifier using
-- @ListResponseHeadersPolicies@.
newGetResponseHeadersPolicy ::
  -- | 'id'
  Prelude.Text ->
  GetResponseHeadersPolicy
newGetResponseHeadersPolicy pId_ =
  GetResponseHeadersPolicy' {id = pId_}

-- | The identifier for the response headers policy.
--
-- If the response headers policy is attached to a distribution’s cache
-- behavior, you can get the policy’s identifier using @ListDistributions@
-- or @GetDistribution@. If the response headers policy is not attached to
-- a cache behavior, you can get the identifier using
-- @ListResponseHeadersPolicies@.
getResponseHeadersPolicy_id :: Lens.Lens' GetResponseHeadersPolicy Prelude.Text
getResponseHeadersPolicy_id = Lens.lens (\GetResponseHeadersPolicy' {id} -> id) (\s@GetResponseHeadersPolicy' {} a -> s {id = a} :: GetResponseHeadersPolicy)

instance Core.AWSRequest GetResponseHeadersPolicy where
  type
    AWSResponse GetResponseHeadersPolicy =
      GetResponseHeadersPolicyResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetResponseHeadersPolicyResponse'
            Prelude.<$> (Core.parseXML x)
            Prelude.<*> (h Core..#? "ETag")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResponseHeadersPolicy where
  hashWithSalt _salt GetResponseHeadersPolicy' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetResponseHeadersPolicy where
  rnf GetResponseHeadersPolicy' {..} = Prelude.rnf id

instance Core.ToHeaders GetResponseHeadersPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetResponseHeadersPolicy where
  toPath GetResponseHeadersPolicy' {..} =
    Prelude.mconcat
      [ "/2020-05-31/response-headers-policy/",
        Core.toBS id
      ]

instance Core.ToQuery GetResponseHeadersPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResponseHeadersPolicyResponse' smart constructor.
data GetResponseHeadersPolicyResponse = GetResponseHeadersPolicyResponse'
  { -- | Contains a response headers policy.
    responseHeadersPolicy :: Prelude.Maybe ResponseHeadersPolicy,
    -- | The version identifier for the current version of the response headers
    -- policy.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResponseHeadersPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'responseHeadersPolicy', 'getResponseHeadersPolicyResponse_responseHeadersPolicy' - Contains a response headers policy.
--
-- 'eTag', 'getResponseHeadersPolicyResponse_eTag' - The version identifier for the current version of the response headers
-- policy.
--
-- 'httpStatus', 'getResponseHeadersPolicyResponse_httpStatus' - The response's http status code.
newGetResponseHeadersPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResponseHeadersPolicyResponse
newGetResponseHeadersPolicyResponse pHttpStatus_ =
  GetResponseHeadersPolicyResponse'
    { responseHeadersPolicy =
        Prelude.Nothing,
      eTag = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains a response headers policy.
getResponseHeadersPolicyResponse_responseHeadersPolicy :: Lens.Lens' GetResponseHeadersPolicyResponse (Prelude.Maybe ResponseHeadersPolicy)
getResponseHeadersPolicyResponse_responseHeadersPolicy = Lens.lens (\GetResponseHeadersPolicyResponse' {responseHeadersPolicy} -> responseHeadersPolicy) (\s@GetResponseHeadersPolicyResponse' {} a -> s {responseHeadersPolicy = a} :: GetResponseHeadersPolicyResponse)

-- | The version identifier for the current version of the response headers
-- policy.
getResponseHeadersPolicyResponse_eTag :: Lens.Lens' GetResponseHeadersPolicyResponse (Prelude.Maybe Prelude.Text)
getResponseHeadersPolicyResponse_eTag = Lens.lens (\GetResponseHeadersPolicyResponse' {eTag} -> eTag) (\s@GetResponseHeadersPolicyResponse' {} a -> s {eTag = a} :: GetResponseHeadersPolicyResponse)

-- | The response's http status code.
getResponseHeadersPolicyResponse_httpStatus :: Lens.Lens' GetResponseHeadersPolicyResponse Prelude.Int
getResponseHeadersPolicyResponse_httpStatus = Lens.lens (\GetResponseHeadersPolicyResponse' {httpStatus} -> httpStatus) (\s@GetResponseHeadersPolicyResponse' {} a -> s {httpStatus = a} :: GetResponseHeadersPolicyResponse)

instance
  Prelude.NFData
    GetResponseHeadersPolicyResponse
  where
  rnf GetResponseHeadersPolicyResponse' {..} =
    Prelude.rnf responseHeadersPolicy
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf httpStatus
