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
-- Module      : Amazonka.Route53.GetHostedZone
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specified hosted zone including the four name
-- servers assigned to the hosted zone.
module Amazonka.Route53.GetHostedZone
  ( -- * Creating a Request
    GetHostedZone (..),
    newGetHostedZone,

    -- * Request Lenses
    getHostedZone_id,

    -- * Destructuring the Response
    GetHostedZoneResponse (..),
    newGetHostedZoneResponse,

    -- * Response Lenses
    getHostedZoneResponse_vPCs,
    getHostedZoneResponse_delegationSet,
    getHostedZoneResponse_httpStatus,
    getHostedZoneResponse_hostedZone,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | A request to get information about a specified hosted zone.
--
-- /See:/ 'newGetHostedZone' smart constructor.
data GetHostedZone = GetHostedZone'
  { -- | The ID of the hosted zone that you want to get information about.
    id :: ResourceId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetHostedZone' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getHostedZone_id' - The ID of the hosted zone that you want to get information about.
newGetHostedZone ::
  -- | 'id'
  ResourceId ->
  GetHostedZone
newGetHostedZone pId_ = GetHostedZone' {id = pId_}

-- | The ID of the hosted zone that you want to get information about.
getHostedZone_id :: Lens.Lens' GetHostedZone ResourceId
getHostedZone_id = Lens.lens (\GetHostedZone' {id} -> id) (\s@GetHostedZone' {} a -> s {id = a} :: GetHostedZone)

instance Core.AWSRequest GetHostedZone where
  type
    AWSResponse GetHostedZone =
      GetHostedZoneResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetHostedZoneResponse'
            Prelude.<$> ( x Core..@? "VPCs" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList1 "VPC")
                        )
            Prelude.<*> (x Core..@? "DelegationSet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..@ "HostedZone")
      )

instance Prelude.Hashable GetHostedZone where
  hashWithSalt _salt GetHostedZone' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetHostedZone where
  rnf GetHostedZone' {..} = Prelude.rnf id

instance Core.ToHeaders GetHostedZone where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetHostedZone where
  toPath GetHostedZone' {..} =
    Prelude.mconcat
      ["/2013-04-01/hostedzone/", Core.toBS id]

instance Core.ToQuery GetHostedZone where
  toQuery = Prelude.const Prelude.mempty

-- | A complex type that contain the response to a @GetHostedZone@ request.
--
-- /See:/ 'newGetHostedZoneResponse' smart constructor.
data GetHostedZoneResponse = GetHostedZoneResponse'
  { -- | A complex type that contains information about the VPCs that are
    -- associated with the specified hosted zone.
    vPCs :: Prelude.Maybe (Prelude.NonEmpty VPC),
    -- | A complex type that lists the Amazon Route 53 name servers for the
    -- specified hosted zone.
    delegationSet :: Prelude.Maybe DelegationSet,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A complex type that contains general information about the specified
    -- hosted zone.
    hostedZone :: HostedZone
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetHostedZoneResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vPCs', 'getHostedZoneResponse_vPCs' - A complex type that contains information about the VPCs that are
-- associated with the specified hosted zone.
--
-- 'delegationSet', 'getHostedZoneResponse_delegationSet' - A complex type that lists the Amazon Route 53 name servers for the
-- specified hosted zone.
--
-- 'httpStatus', 'getHostedZoneResponse_httpStatus' - The response's http status code.
--
-- 'hostedZone', 'getHostedZoneResponse_hostedZone' - A complex type that contains general information about the specified
-- hosted zone.
newGetHostedZoneResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'hostedZone'
  HostedZone ->
  GetHostedZoneResponse
newGetHostedZoneResponse pHttpStatus_ pHostedZone_ =
  GetHostedZoneResponse'
    { vPCs = Prelude.Nothing,
      delegationSet = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      hostedZone = pHostedZone_
    }

-- | A complex type that contains information about the VPCs that are
-- associated with the specified hosted zone.
getHostedZoneResponse_vPCs :: Lens.Lens' GetHostedZoneResponse (Prelude.Maybe (Prelude.NonEmpty VPC))
getHostedZoneResponse_vPCs = Lens.lens (\GetHostedZoneResponse' {vPCs} -> vPCs) (\s@GetHostedZoneResponse' {} a -> s {vPCs = a} :: GetHostedZoneResponse) Prelude.. Lens.mapping Lens.coerced

-- | A complex type that lists the Amazon Route 53 name servers for the
-- specified hosted zone.
getHostedZoneResponse_delegationSet :: Lens.Lens' GetHostedZoneResponse (Prelude.Maybe DelegationSet)
getHostedZoneResponse_delegationSet = Lens.lens (\GetHostedZoneResponse' {delegationSet} -> delegationSet) (\s@GetHostedZoneResponse' {} a -> s {delegationSet = a} :: GetHostedZoneResponse)

-- | The response's http status code.
getHostedZoneResponse_httpStatus :: Lens.Lens' GetHostedZoneResponse Prelude.Int
getHostedZoneResponse_httpStatus = Lens.lens (\GetHostedZoneResponse' {httpStatus} -> httpStatus) (\s@GetHostedZoneResponse' {} a -> s {httpStatus = a} :: GetHostedZoneResponse)

-- | A complex type that contains general information about the specified
-- hosted zone.
getHostedZoneResponse_hostedZone :: Lens.Lens' GetHostedZoneResponse HostedZone
getHostedZoneResponse_hostedZone = Lens.lens (\GetHostedZoneResponse' {hostedZone} -> hostedZone) (\s@GetHostedZoneResponse' {} a -> s {hostedZone = a} :: GetHostedZoneResponse)

instance Prelude.NFData GetHostedZoneResponse where
  rnf GetHostedZoneResponse' {..} =
    Prelude.rnf vPCs
      `Prelude.seq` Prelude.rnf delegationSet
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf hostedZone
