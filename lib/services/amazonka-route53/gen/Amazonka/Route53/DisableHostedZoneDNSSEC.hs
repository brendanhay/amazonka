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
-- Module      : Amazonka.Route53.DisableHostedZoneDNSSEC
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables DNSSEC signing in a specific hosted zone. This action does not
-- deactivate any key-signing keys (KSKs) that are active in the hosted
-- zone.
module Amazonka.Route53.DisableHostedZoneDNSSEC
  ( -- * Creating a Request
    DisableHostedZoneDNSSEC (..),
    newDisableHostedZoneDNSSEC,

    -- * Request Lenses
    disableHostedZoneDNSSEC_hostedZoneId,

    -- * Destructuring the Response
    DisableHostedZoneDNSSECResponse (..),
    newDisableHostedZoneDNSSECResponse,

    -- * Response Lenses
    disableHostedZoneDNSSECResponse_httpStatus,
    disableHostedZoneDNSSECResponse_changeInfo,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | /See:/ 'newDisableHostedZoneDNSSEC' smart constructor.
data DisableHostedZoneDNSSEC = DisableHostedZoneDNSSEC'
  { -- | A unique string used to identify a hosted zone.
    hostedZoneId :: ResourceId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableHostedZoneDNSSEC' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostedZoneId', 'disableHostedZoneDNSSEC_hostedZoneId' - A unique string used to identify a hosted zone.
newDisableHostedZoneDNSSEC ::
  -- | 'hostedZoneId'
  ResourceId ->
  DisableHostedZoneDNSSEC
newDisableHostedZoneDNSSEC pHostedZoneId_ =
  DisableHostedZoneDNSSEC'
    { hostedZoneId =
        pHostedZoneId_
    }

-- | A unique string used to identify a hosted zone.
disableHostedZoneDNSSEC_hostedZoneId :: Lens.Lens' DisableHostedZoneDNSSEC ResourceId
disableHostedZoneDNSSEC_hostedZoneId = Lens.lens (\DisableHostedZoneDNSSEC' {hostedZoneId} -> hostedZoneId) (\s@DisableHostedZoneDNSSEC' {} a -> s {hostedZoneId = a} :: DisableHostedZoneDNSSEC)

instance Core.AWSRequest DisableHostedZoneDNSSEC where
  type
    AWSResponse DisableHostedZoneDNSSEC =
      DisableHostedZoneDNSSECResponse
  request overrides =
    Request.post (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DisableHostedZoneDNSSECResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "ChangeInfo")
      )

instance Prelude.Hashable DisableHostedZoneDNSSEC where
  hashWithSalt _salt DisableHostedZoneDNSSEC' {..} =
    _salt `Prelude.hashWithSalt` hostedZoneId

instance Prelude.NFData DisableHostedZoneDNSSEC where
  rnf DisableHostedZoneDNSSEC' {..} =
    Prelude.rnf hostedZoneId

instance Data.ToHeaders DisableHostedZoneDNSSEC where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DisableHostedZoneDNSSEC where
  toPath DisableHostedZoneDNSSEC' {..} =
    Prelude.mconcat
      [ "/2013-04-01/hostedzone/",
        Data.toBS hostedZoneId,
        "/disable-dnssec"
      ]

instance Data.ToQuery DisableHostedZoneDNSSEC where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableHostedZoneDNSSECResponse' smart constructor.
data DisableHostedZoneDNSSECResponse = DisableHostedZoneDNSSECResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    changeInfo :: ChangeInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableHostedZoneDNSSECResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disableHostedZoneDNSSECResponse_httpStatus' - The response's http status code.
--
-- 'changeInfo', 'disableHostedZoneDNSSECResponse_changeInfo' - Undocumented member.
newDisableHostedZoneDNSSECResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'changeInfo'
  ChangeInfo ->
  DisableHostedZoneDNSSECResponse
newDisableHostedZoneDNSSECResponse
  pHttpStatus_
  pChangeInfo_ =
    DisableHostedZoneDNSSECResponse'
      { httpStatus =
          pHttpStatus_,
        changeInfo = pChangeInfo_
      }

-- | The response's http status code.
disableHostedZoneDNSSECResponse_httpStatus :: Lens.Lens' DisableHostedZoneDNSSECResponse Prelude.Int
disableHostedZoneDNSSECResponse_httpStatus = Lens.lens (\DisableHostedZoneDNSSECResponse' {httpStatus} -> httpStatus) (\s@DisableHostedZoneDNSSECResponse' {} a -> s {httpStatus = a} :: DisableHostedZoneDNSSECResponse)

-- | Undocumented member.
disableHostedZoneDNSSECResponse_changeInfo :: Lens.Lens' DisableHostedZoneDNSSECResponse ChangeInfo
disableHostedZoneDNSSECResponse_changeInfo = Lens.lens (\DisableHostedZoneDNSSECResponse' {changeInfo} -> changeInfo) (\s@DisableHostedZoneDNSSECResponse' {} a -> s {changeInfo = a} :: DisableHostedZoneDNSSECResponse)

instance
  Prelude.NFData
    DisableHostedZoneDNSSECResponse
  where
  rnf DisableHostedZoneDNSSECResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf changeInfo
