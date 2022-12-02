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
-- Module      : Amazonka.Route53.GetTrafficPolicyInstance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specified traffic policy instance.
--
-- After you submit a @CreateTrafficPolicyInstance@ or an
-- @UpdateTrafficPolicyInstance@ request, there\'s a brief delay while
-- Amazon Route 53 creates the resource record sets that are specified in
-- the traffic policy definition. For more information, see the @State@
-- response element.
--
-- In the Route 53 console, traffic policy instances are known as policy
-- records.
module Amazonka.Route53.GetTrafficPolicyInstance
  ( -- * Creating a Request
    GetTrafficPolicyInstance (..),
    newGetTrafficPolicyInstance,

    -- * Request Lenses
    getTrafficPolicyInstance_id,

    -- * Destructuring the Response
    GetTrafficPolicyInstanceResponse (..),
    newGetTrafficPolicyInstanceResponse,

    -- * Response Lenses
    getTrafficPolicyInstanceResponse_httpStatus,
    getTrafficPolicyInstanceResponse_trafficPolicyInstance,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | Gets information about a specified traffic policy instance.
--
-- /See:/ 'newGetTrafficPolicyInstance' smart constructor.
data GetTrafficPolicyInstance = GetTrafficPolicyInstance'
  { -- | The ID of the traffic policy instance that you want to get information
    -- about.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTrafficPolicyInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getTrafficPolicyInstance_id' - The ID of the traffic policy instance that you want to get information
-- about.
newGetTrafficPolicyInstance ::
  -- | 'id'
  Prelude.Text ->
  GetTrafficPolicyInstance
newGetTrafficPolicyInstance pId_ =
  GetTrafficPolicyInstance' {id = pId_}

-- | The ID of the traffic policy instance that you want to get information
-- about.
getTrafficPolicyInstance_id :: Lens.Lens' GetTrafficPolicyInstance Prelude.Text
getTrafficPolicyInstance_id = Lens.lens (\GetTrafficPolicyInstance' {id} -> id) (\s@GetTrafficPolicyInstance' {} a -> s {id = a} :: GetTrafficPolicyInstance)

instance Core.AWSRequest GetTrafficPolicyInstance where
  type
    AWSResponse GetTrafficPolicyInstance =
      GetTrafficPolicyInstanceResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetTrafficPolicyInstanceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "TrafficPolicyInstance")
      )

instance Prelude.Hashable GetTrafficPolicyInstance where
  hashWithSalt _salt GetTrafficPolicyInstance' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetTrafficPolicyInstance where
  rnf GetTrafficPolicyInstance' {..} = Prelude.rnf id

instance Data.ToHeaders GetTrafficPolicyInstance where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetTrafficPolicyInstance where
  toPath GetTrafficPolicyInstance' {..} =
    Prelude.mconcat
      ["/2013-04-01/trafficpolicyinstance/", Data.toBS id]

instance Data.ToQuery GetTrafficPolicyInstance where
  toQuery = Prelude.const Prelude.mempty

-- | A complex type that contains information about the resource record sets
-- that Amazon Route 53 created based on a specified traffic policy.
--
-- /See:/ 'newGetTrafficPolicyInstanceResponse' smart constructor.
data GetTrafficPolicyInstanceResponse = GetTrafficPolicyInstanceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A complex type that contains settings for the traffic policy instance.
    trafficPolicyInstance :: TrafficPolicyInstance
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTrafficPolicyInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getTrafficPolicyInstanceResponse_httpStatus' - The response's http status code.
--
-- 'trafficPolicyInstance', 'getTrafficPolicyInstanceResponse_trafficPolicyInstance' - A complex type that contains settings for the traffic policy instance.
newGetTrafficPolicyInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'trafficPolicyInstance'
  TrafficPolicyInstance ->
  GetTrafficPolicyInstanceResponse
newGetTrafficPolicyInstanceResponse
  pHttpStatus_
  pTrafficPolicyInstance_ =
    GetTrafficPolicyInstanceResponse'
      { httpStatus =
          pHttpStatus_,
        trafficPolicyInstance =
          pTrafficPolicyInstance_
      }

-- | The response's http status code.
getTrafficPolicyInstanceResponse_httpStatus :: Lens.Lens' GetTrafficPolicyInstanceResponse Prelude.Int
getTrafficPolicyInstanceResponse_httpStatus = Lens.lens (\GetTrafficPolicyInstanceResponse' {httpStatus} -> httpStatus) (\s@GetTrafficPolicyInstanceResponse' {} a -> s {httpStatus = a} :: GetTrafficPolicyInstanceResponse)

-- | A complex type that contains settings for the traffic policy instance.
getTrafficPolicyInstanceResponse_trafficPolicyInstance :: Lens.Lens' GetTrafficPolicyInstanceResponse TrafficPolicyInstance
getTrafficPolicyInstanceResponse_trafficPolicyInstance = Lens.lens (\GetTrafficPolicyInstanceResponse' {trafficPolicyInstance} -> trafficPolicyInstance) (\s@GetTrafficPolicyInstanceResponse' {} a -> s {trafficPolicyInstance = a} :: GetTrafficPolicyInstanceResponse)

instance
  Prelude.NFData
    GetTrafficPolicyInstanceResponse
  where
  rnf GetTrafficPolicyInstanceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf trafficPolicyInstance
