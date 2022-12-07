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
-- Module      : Amazonka.Route53.EnableHostedZoneDNSSEC
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables DNSSEC signing in a specific hosted zone.
module Amazonka.Route53.EnableHostedZoneDNSSEC
  ( -- * Creating a Request
    EnableHostedZoneDNSSEC (..),
    newEnableHostedZoneDNSSEC,

    -- * Request Lenses
    enableHostedZoneDNSSEC_hostedZoneId,

    -- * Destructuring the Response
    EnableHostedZoneDNSSECResponse (..),
    newEnableHostedZoneDNSSECResponse,

    -- * Response Lenses
    enableHostedZoneDNSSECResponse_httpStatus,
    enableHostedZoneDNSSECResponse_changeInfo,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | /See:/ 'newEnableHostedZoneDNSSEC' smart constructor.
data EnableHostedZoneDNSSEC = EnableHostedZoneDNSSEC'
  { -- | A unique string used to identify a hosted zone.
    hostedZoneId :: ResourceId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableHostedZoneDNSSEC' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostedZoneId', 'enableHostedZoneDNSSEC_hostedZoneId' - A unique string used to identify a hosted zone.
newEnableHostedZoneDNSSEC ::
  -- | 'hostedZoneId'
  ResourceId ->
  EnableHostedZoneDNSSEC
newEnableHostedZoneDNSSEC pHostedZoneId_ =
  EnableHostedZoneDNSSEC'
    { hostedZoneId =
        pHostedZoneId_
    }

-- | A unique string used to identify a hosted zone.
enableHostedZoneDNSSEC_hostedZoneId :: Lens.Lens' EnableHostedZoneDNSSEC ResourceId
enableHostedZoneDNSSEC_hostedZoneId = Lens.lens (\EnableHostedZoneDNSSEC' {hostedZoneId} -> hostedZoneId) (\s@EnableHostedZoneDNSSEC' {} a -> s {hostedZoneId = a} :: EnableHostedZoneDNSSEC)

instance Core.AWSRequest EnableHostedZoneDNSSEC where
  type
    AWSResponse EnableHostedZoneDNSSEC =
      EnableHostedZoneDNSSECResponse
  request overrides =
    Request.post (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          EnableHostedZoneDNSSECResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "ChangeInfo")
      )

instance Prelude.Hashable EnableHostedZoneDNSSEC where
  hashWithSalt _salt EnableHostedZoneDNSSEC' {..} =
    _salt `Prelude.hashWithSalt` hostedZoneId

instance Prelude.NFData EnableHostedZoneDNSSEC where
  rnf EnableHostedZoneDNSSEC' {..} =
    Prelude.rnf hostedZoneId

instance Data.ToHeaders EnableHostedZoneDNSSEC where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath EnableHostedZoneDNSSEC where
  toPath EnableHostedZoneDNSSEC' {..} =
    Prelude.mconcat
      [ "/2013-04-01/hostedzone/",
        Data.toBS hostedZoneId,
        "/enable-dnssec"
      ]

instance Data.ToQuery EnableHostedZoneDNSSEC where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableHostedZoneDNSSECResponse' smart constructor.
data EnableHostedZoneDNSSECResponse = EnableHostedZoneDNSSECResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    changeInfo :: ChangeInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableHostedZoneDNSSECResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'enableHostedZoneDNSSECResponse_httpStatus' - The response's http status code.
--
-- 'changeInfo', 'enableHostedZoneDNSSECResponse_changeInfo' - Undocumented member.
newEnableHostedZoneDNSSECResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'changeInfo'
  ChangeInfo ->
  EnableHostedZoneDNSSECResponse
newEnableHostedZoneDNSSECResponse
  pHttpStatus_
  pChangeInfo_ =
    EnableHostedZoneDNSSECResponse'
      { httpStatus =
          pHttpStatus_,
        changeInfo = pChangeInfo_
      }

-- | The response's http status code.
enableHostedZoneDNSSECResponse_httpStatus :: Lens.Lens' EnableHostedZoneDNSSECResponse Prelude.Int
enableHostedZoneDNSSECResponse_httpStatus = Lens.lens (\EnableHostedZoneDNSSECResponse' {httpStatus} -> httpStatus) (\s@EnableHostedZoneDNSSECResponse' {} a -> s {httpStatus = a} :: EnableHostedZoneDNSSECResponse)

-- | Undocumented member.
enableHostedZoneDNSSECResponse_changeInfo :: Lens.Lens' EnableHostedZoneDNSSECResponse ChangeInfo
enableHostedZoneDNSSECResponse_changeInfo = Lens.lens (\EnableHostedZoneDNSSECResponse' {changeInfo} -> changeInfo) (\s@EnableHostedZoneDNSSECResponse' {} a -> s {changeInfo = a} :: EnableHostedZoneDNSSECResponse)

instance
  Prelude.NFData
    EnableHostedZoneDNSSECResponse
  where
  rnf EnableHostedZoneDNSSECResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf changeInfo
