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
-- Module      : Amazonka.FMS.DisassociateThirdPartyFirewall
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a Firewall Manager policy administrator from a third-party
-- firewall tenant. When you call @DisassociateThirdPartyFirewall@, the
-- third-party firewall vendor deletes all of the firewalls that are
-- associated with the account.
module Amazonka.FMS.DisassociateThirdPartyFirewall
  ( -- * Creating a Request
    DisassociateThirdPartyFirewall (..),
    newDisassociateThirdPartyFirewall,

    -- * Request Lenses
    disassociateThirdPartyFirewall_thirdPartyFirewall,

    -- * Destructuring the Response
    DisassociateThirdPartyFirewallResponse (..),
    newDisassociateThirdPartyFirewallResponse,

    -- * Response Lenses
    disassociateThirdPartyFirewallResponse_thirdPartyFirewallStatus,
    disassociateThirdPartyFirewallResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateThirdPartyFirewall' smart constructor.
data DisassociateThirdPartyFirewall = DisassociateThirdPartyFirewall'
  { -- | The name of the third-party firewall vendor.
    thirdPartyFirewall :: ThirdPartyFirewall
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateThirdPartyFirewall' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thirdPartyFirewall', 'disassociateThirdPartyFirewall_thirdPartyFirewall' - The name of the third-party firewall vendor.
newDisassociateThirdPartyFirewall ::
  -- | 'thirdPartyFirewall'
  ThirdPartyFirewall ->
  DisassociateThirdPartyFirewall
newDisassociateThirdPartyFirewall
  pThirdPartyFirewall_ =
    DisassociateThirdPartyFirewall'
      { thirdPartyFirewall =
          pThirdPartyFirewall_
      }

-- | The name of the third-party firewall vendor.
disassociateThirdPartyFirewall_thirdPartyFirewall :: Lens.Lens' DisassociateThirdPartyFirewall ThirdPartyFirewall
disassociateThirdPartyFirewall_thirdPartyFirewall = Lens.lens (\DisassociateThirdPartyFirewall' {thirdPartyFirewall} -> thirdPartyFirewall) (\s@DisassociateThirdPartyFirewall' {} a -> s {thirdPartyFirewall = a} :: DisassociateThirdPartyFirewall)

instance
  Core.AWSRequest
    DisassociateThirdPartyFirewall
  where
  type
    AWSResponse DisassociateThirdPartyFirewall =
      DisassociateThirdPartyFirewallResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateThirdPartyFirewallResponse'
            Prelude.<$> (x Data..?> "ThirdPartyFirewallStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateThirdPartyFirewall
  where
  hashWithSalt
    _salt
    DisassociateThirdPartyFirewall' {..} =
      _salt `Prelude.hashWithSalt` thirdPartyFirewall

instance
  Prelude.NFData
    DisassociateThirdPartyFirewall
  where
  rnf DisassociateThirdPartyFirewall' {..} =
    Prelude.rnf thirdPartyFirewall

instance
  Data.ToHeaders
    DisassociateThirdPartyFirewall
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSFMS_20180101.DisassociateThirdPartyFirewall" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateThirdPartyFirewall where
  toJSON DisassociateThirdPartyFirewall' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ThirdPartyFirewall" Data..= thirdPartyFirewall)
          ]
      )

instance Data.ToPath DisassociateThirdPartyFirewall where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateThirdPartyFirewall where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateThirdPartyFirewallResponse' smart constructor.
data DisassociateThirdPartyFirewallResponse = DisassociateThirdPartyFirewallResponse'
  { -- | The current status for the disassociation of a Firewall Manager
    -- administrators account with a third-party firewall.
    thirdPartyFirewallStatus :: Prelude.Maybe ThirdPartyFirewallAssociationStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateThirdPartyFirewallResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thirdPartyFirewallStatus', 'disassociateThirdPartyFirewallResponse_thirdPartyFirewallStatus' - The current status for the disassociation of a Firewall Manager
-- administrators account with a third-party firewall.
--
-- 'httpStatus', 'disassociateThirdPartyFirewallResponse_httpStatus' - The response's http status code.
newDisassociateThirdPartyFirewallResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateThirdPartyFirewallResponse
newDisassociateThirdPartyFirewallResponse
  pHttpStatus_ =
    DisassociateThirdPartyFirewallResponse'
      { thirdPartyFirewallStatus =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The current status for the disassociation of a Firewall Manager
-- administrators account with a third-party firewall.
disassociateThirdPartyFirewallResponse_thirdPartyFirewallStatus :: Lens.Lens' DisassociateThirdPartyFirewallResponse (Prelude.Maybe ThirdPartyFirewallAssociationStatus)
disassociateThirdPartyFirewallResponse_thirdPartyFirewallStatus = Lens.lens (\DisassociateThirdPartyFirewallResponse' {thirdPartyFirewallStatus} -> thirdPartyFirewallStatus) (\s@DisassociateThirdPartyFirewallResponse' {} a -> s {thirdPartyFirewallStatus = a} :: DisassociateThirdPartyFirewallResponse)

-- | The response's http status code.
disassociateThirdPartyFirewallResponse_httpStatus :: Lens.Lens' DisassociateThirdPartyFirewallResponse Prelude.Int
disassociateThirdPartyFirewallResponse_httpStatus = Lens.lens (\DisassociateThirdPartyFirewallResponse' {httpStatus} -> httpStatus) (\s@DisassociateThirdPartyFirewallResponse' {} a -> s {httpStatus = a} :: DisassociateThirdPartyFirewallResponse)

instance
  Prelude.NFData
    DisassociateThirdPartyFirewallResponse
  where
  rnf DisassociateThirdPartyFirewallResponse' {..} =
    Prelude.rnf thirdPartyFirewallStatus
      `Prelude.seq` Prelude.rnf httpStatus
