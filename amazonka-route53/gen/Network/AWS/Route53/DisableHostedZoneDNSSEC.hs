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
-- Module      : Network.AWS.Route53.DisableHostedZoneDNSSEC
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables DNSSEC signing in a specific hosted zone. This action does not
-- deactivate any key-signing keys (KSKs) that are active in the hosted
-- zone.
module Network.AWS.Route53.DisableHostedZoneDNSSEC
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

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
  request = Request.post defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DisableHostedZoneDNSSECResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..@ "ChangeInfo")
      )

instance Prelude.Hashable DisableHostedZoneDNSSEC

instance Prelude.NFData DisableHostedZoneDNSSEC

instance Core.ToHeaders DisableHostedZoneDNSSEC where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DisableHostedZoneDNSSEC where
  toPath DisableHostedZoneDNSSEC' {..} =
    Prelude.mconcat
      [ "/2013-04-01/hostedzone/",
        Core.toBS hostedZoneId,
        "/disable-dnssec"
      ]

instance Core.ToQuery DisableHostedZoneDNSSEC where
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
