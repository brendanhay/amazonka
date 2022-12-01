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
-- Module      : Amazonka.Route53.GetTrafficPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specific traffic policy version.
--
-- For information about how of deleting a traffic policy affects the
-- response from @GetTrafficPolicy@, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_DeleteTrafficPolicy.html DeleteTrafficPolicy>.
module Amazonka.Route53.GetTrafficPolicy
  ( -- * Creating a Request
    GetTrafficPolicy (..),
    newGetTrafficPolicy,

    -- * Request Lenses
    getTrafficPolicy_id,
    getTrafficPolicy_version,

    -- * Destructuring the Response
    GetTrafficPolicyResponse (..),
    newGetTrafficPolicyResponse,

    -- * Response Lenses
    getTrafficPolicyResponse_httpStatus,
    getTrafficPolicyResponse_trafficPolicy,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | Gets information about a specific traffic policy version.
--
-- /See:/ 'newGetTrafficPolicy' smart constructor.
data GetTrafficPolicy = GetTrafficPolicy'
  { -- | The ID of the traffic policy that you want to get information about.
    id :: Prelude.Text,
    -- | The version number of the traffic policy that you want to get
    -- information about.
    version :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTrafficPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getTrafficPolicy_id' - The ID of the traffic policy that you want to get information about.
--
-- 'version', 'getTrafficPolicy_version' - The version number of the traffic policy that you want to get
-- information about.
newGetTrafficPolicy ::
  -- | 'id'
  Prelude.Text ->
  -- | 'version'
  Prelude.Natural ->
  GetTrafficPolicy
newGetTrafficPolicy pId_ pVersion_ =
  GetTrafficPolicy' {id = pId_, version = pVersion_}

-- | The ID of the traffic policy that you want to get information about.
getTrafficPolicy_id :: Lens.Lens' GetTrafficPolicy Prelude.Text
getTrafficPolicy_id = Lens.lens (\GetTrafficPolicy' {id} -> id) (\s@GetTrafficPolicy' {} a -> s {id = a} :: GetTrafficPolicy)

-- | The version number of the traffic policy that you want to get
-- information about.
getTrafficPolicy_version :: Lens.Lens' GetTrafficPolicy Prelude.Natural
getTrafficPolicy_version = Lens.lens (\GetTrafficPolicy' {version} -> version) (\s@GetTrafficPolicy' {} a -> s {version = a} :: GetTrafficPolicy)

instance Core.AWSRequest GetTrafficPolicy where
  type
    AWSResponse GetTrafficPolicy =
      GetTrafficPolicyResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetTrafficPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..@ "TrafficPolicy")
      )

instance Prelude.Hashable GetTrafficPolicy where
  hashWithSalt _salt GetTrafficPolicy' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` version

instance Prelude.NFData GetTrafficPolicy where
  rnf GetTrafficPolicy' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf version

instance Core.ToHeaders GetTrafficPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetTrafficPolicy where
  toPath GetTrafficPolicy' {..} =
    Prelude.mconcat
      [ "/2013-04-01/trafficpolicy/",
        Core.toBS id,
        "/",
        Core.toBS version
      ]

instance Core.ToQuery GetTrafficPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | A complex type that contains the response information for the request.
--
-- /See:/ 'newGetTrafficPolicyResponse' smart constructor.
data GetTrafficPolicyResponse = GetTrafficPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A complex type that contains settings for the specified traffic policy.
    trafficPolicy :: TrafficPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTrafficPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getTrafficPolicyResponse_httpStatus' - The response's http status code.
--
-- 'trafficPolicy', 'getTrafficPolicyResponse_trafficPolicy' - A complex type that contains settings for the specified traffic policy.
newGetTrafficPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'trafficPolicy'
  TrafficPolicy ->
  GetTrafficPolicyResponse
newGetTrafficPolicyResponse
  pHttpStatus_
  pTrafficPolicy_ =
    GetTrafficPolicyResponse'
      { httpStatus =
          pHttpStatus_,
        trafficPolicy = pTrafficPolicy_
      }

-- | The response's http status code.
getTrafficPolicyResponse_httpStatus :: Lens.Lens' GetTrafficPolicyResponse Prelude.Int
getTrafficPolicyResponse_httpStatus = Lens.lens (\GetTrafficPolicyResponse' {httpStatus} -> httpStatus) (\s@GetTrafficPolicyResponse' {} a -> s {httpStatus = a} :: GetTrafficPolicyResponse)

-- | A complex type that contains settings for the specified traffic policy.
getTrafficPolicyResponse_trafficPolicy :: Lens.Lens' GetTrafficPolicyResponse TrafficPolicy
getTrafficPolicyResponse_trafficPolicy = Lens.lens (\GetTrafficPolicyResponse' {trafficPolicy} -> trafficPolicy) (\s@GetTrafficPolicyResponse' {} a -> s {trafficPolicy = a} :: GetTrafficPolicyResponse)

instance Prelude.NFData GetTrafficPolicyResponse where
  rnf GetTrafficPolicyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf trafficPolicy
