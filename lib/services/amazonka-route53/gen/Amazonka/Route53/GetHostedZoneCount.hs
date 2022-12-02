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
-- Module      : Amazonka.Route53.GetHostedZoneCount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the number of hosted zones that are associated with the
-- current Amazon Web Services account.
module Amazonka.Route53.GetHostedZoneCount
  ( -- * Creating a Request
    GetHostedZoneCount (..),
    newGetHostedZoneCount,

    -- * Destructuring the Response
    GetHostedZoneCountResponse (..),
    newGetHostedZoneCountResponse,

    -- * Response Lenses
    getHostedZoneCountResponse_httpStatus,
    getHostedZoneCountResponse_hostedZoneCount,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | A request to retrieve a count of all the hosted zones that are
-- associated with the current Amazon Web Services account.
--
-- /See:/ 'newGetHostedZoneCount' smart constructor.
data GetHostedZoneCount = GetHostedZoneCount'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetHostedZoneCount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetHostedZoneCount ::
  GetHostedZoneCount
newGetHostedZoneCount = GetHostedZoneCount'

instance Core.AWSRequest GetHostedZoneCount where
  type
    AWSResponse GetHostedZoneCount =
      GetHostedZoneCountResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetHostedZoneCountResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "HostedZoneCount")
      )

instance Prelude.Hashable GetHostedZoneCount where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetHostedZoneCount where
  rnf _ = ()

instance Data.ToHeaders GetHostedZoneCount where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetHostedZoneCount where
  toPath = Prelude.const "/2013-04-01/hostedzonecount"

instance Data.ToQuery GetHostedZoneCount where
  toQuery = Prelude.const Prelude.mempty

-- | A complex type that contains the response to a @GetHostedZoneCount@
-- request.
--
-- /See:/ 'newGetHostedZoneCountResponse' smart constructor.
data GetHostedZoneCountResponse = GetHostedZoneCountResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The total number of public and private hosted zones that are associated
    -- with the current Amazon Web Services account.
    hostedZoneCount :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetHostedZoneCountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getHostedZoneCountResponse_httpStatus' - The response's http status code.
--
-- 'hostedZoneCount', 'getHostedZoneCountResponse_hostedZoneCount' - The total number of public and private hosted zones that are associated
-- with the current Amazon Web Services account.
newGetHostedZoneCountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'hostedZoneCount'
  Prelude.Integer ->
  GetHostedZoneCountResponse
newGetHostedZoneCountResponse
  pHttpStatus_
  pHostedZoneCount_ =
    GetHostedZoneCountResponse'
      { httpStatus =
          pHttpStatus_,
        hostedZoneCount = pHostedZoneCount_
      }

-- | The response's http status code.
getHostedZoneCountResponse_httpStatus :: Lens.Lens' GetHostedZoneCountResponse Prelude.Int
getHostedZoneCountResponse_httpStatus = Lens.lens (\GetHostedZoneCountResponse' {httpStatus} -> httpStatus) (\s@GetHostedZoneCountResponse' {} a -> s {httpStatus = a} :: GetHostedZoneCountResponse)

-- | The total number of public and private hosted zones that are associated
-- with the current Amazon Web Services account.
getHostedZoneCountResponse_hostedZoneCount :: Lens.Lens' GetHostedZoneCountResponse Prelude.Integer
getHostedZoneCountResponse_hostedZoneCount = Lens.lens (\GetHostedZoneCountResponse' {hostedZoneCount} -> hostedZoneCount) (\s@GetHostedZoneCountResponse' {} a -> s {hostedZoneCount = a} :: GetHostedZoneCountResponse)

instance Prelude.NFData GetHostedZoneCountResponse where
  rnf GetHostedZoneCountResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf hostedZoneCount
