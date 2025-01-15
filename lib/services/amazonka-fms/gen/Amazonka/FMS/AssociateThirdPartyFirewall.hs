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
-- Module      : Amazonka.FMS.AssociateThirdPartyFirewall
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the Firewall Manager policy administrator as a tenant administrator
-- of a third-party firewall service. A tenant is an instance of the
-- third-party firewall service that\'s associated with your Amazon Web
-- Services customer account.
module Amazonka.FMS.AssociateThirdPartyFirewall
  ( -- * Creating a Request
    AssociateThirdPartyFirewall (..),
    newAssociateThirdPartyFirewall,

    -- * Request Lenses
    associateThirdPartyFirewall_thirdPartyFirewall,

    -- * Destructuring the Response
    AssociateThirdPartyFirewallResponse (..),
    newAssociateThirdPartyFirewallResponse,

    -- * Response Lenses
    associateThirdPartyFirewallResponse_thirdPartyFirewallStatus,
    associateThirdPartyFirewallResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateThirdPartyFirewall' smart constructor.
data AssociateThirdPartyFirewall = AssociateThirdPartyFirewall'
  { -- | The name of the third-party firewall vendor.
    thirdPartyFirewall :: ThirdPartyFirewall
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateThirdPartyFirewall' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thirdPartyFirewall', 'associateThirdPartyFirewall_thirdPartyFirewall' - The name of the third-party firewall vendor.
newAssociateThirdPartyFirewall ::
  -- | 'thirdPartyFirewall'
  ThirdPartyFirewall ->
  AssociateThirdPartyFirewall
newAssociateThirdPartyFirewall pThirdPartyFirewall_ =
  AssociateThirdPartyFirewall'
    { thirdPartyFirewall =
        pThirdPartyFirewall_
    }

-- | The name of the third-party firewall vendor.
associateThirdPartyFirewall_thirdPartyFirewall :: Lens.Lens' AssociateThirdPartyFirewall ThirdPartyFirewall
associateThirdPartyFirewall_thirdPartyFirewall = Lens.lens (\AssociateThirdPartyFirewall' {thirdPartyFirewall} -> thirdPartyFirewall) (\s@AssociateThirdPartyFirewall' {} a -> s {thirdPartyFirewall = a} :: AssociateThirdPartyFirewall)

instance Core.AWSRequest AssociateThirdPartyFirewall where
  type
    AWSResponse AssociateThirdPartyFirewall =
      AssociateThirdPartyFirewallResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateThirdPartyFirewallResponse'
            Prelude.<$> (x Data..?> "ThirdPartyFirewallStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateThirdPartyFirewall where
  hashWithSalt _salt AssociateThirdPartyFirewall' {..} =
    _salt `Prelude.hashWithSalt` thirdPartyFirewall

instance Prelude.NFData AssociateThirdPartyFirewall where
  rnf AssociateThirdPartyFirewall' {..} =
    Prelude.rnf thirdPartyFirewall

instance Data.ToHeaders AssociateThirdPartyFirewall where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSFMS_20180101.AssociateThirdPartyFirewall" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateThirdPartyFirewall where
  toJSON AssociateThirdPartyFirewall' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ThirdPartyFirewall" Data..= thirdPartyFirewall)
          ]
      )

instance Data.ToPath AssociateThirdPartyFirewall where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateThirdPartyFirewall where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateThirdPartyFirewallResponse' smart constructor.
data AssociateThirdPartyFirewallResponse = AssociateThirdPartyFirewallResponse'
  { -- | The current status for setting a Firewall Manager policy
    -- administrator\'s account as an administrator of the third-party firewall
    -- tenant.
    --
    -- -   @ONBOARDING@ - The Firewall Manager policy administrator is being
    --     designated as a tenant administrator.
    --
    -- -   @ONBOARD_COMPLETE@ - The Firewall Manager policy administrator is
    --     designated as a tenant administrator.
    --
    -- -   @OFFBOARDING@ - The Firewall Manager policy administrator is being
    --     removed as a tenant administrator.
    --
    -- -   @OFFBOARD_COMPLETE@ - The Firewall Manager policy administrator has
    --     been removed as a tenant administrator.
    --
    -- -   @NOT_EXIST@ - The Firewall Manager policy administrator doesn\'t
    --     exist as a tenant administrator.
    thirdPartyFirewallStatus :: Prelude.Maybe ThirdPartyFirewallAssociationStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateThirdPartyFirewallResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thirdPartyFirewallStatus', 'associateThirdPartyFirewallResponse_thirdPartyFirewallStatus' - The current status for setting a Firewall Manager policy
-- administrator\'s account as an administrator of the third-party firewall
-- tenant.
--
-- -   @ONBOARDING@ - The Firewall Manager policy administrator is being
--     designated as a tenant administrator.
--
-- -   @ONBOARD_COMPLETE@ - The Firewall Manager policy administrator is
--     designated as a tenant administrator.
--
-- -   @OFFBOARDING@ - The Firewall Manager policy administrator is being
--     removed as a tenant administrator.
--
-- -   @OFFBOARD_COMPLETE@ - The Firewall Manager policy administrator has
--     been removed as a tenant administrator.
--
-- -   @NOT_EXIST@ - The Firewall Manager policy administrator doesn\'t
--     exist as a tenant administrator.
--
-- 'httpStatus', 'associateThirdPartyFirewallResponse_httpStatus' - The response's http status code.
newAssociateThirdPartyFirewallResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateThirdPartyFirewallResponse
newAssociateThirdPartyFirewallResponse pHttpStatus_ =
  AssociateThirdPartyFirewallResponse'
    { thirdPartyFirewallStatus =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current status for setting a Firewall Manager policy
-- administrator\'s account as an administrator of the third-party firewall
-- tenant.
--
-- -   @ONBOARDING@ - The Firewall Manager policy administrator is being
--     designated as a tenant administrator.
--
-- -   @ONBOARD_COMPLETE@ - The Firewall Manager policy administrator is
--     designated as a tenant administrator.
--
-- -   @OFFBOARDING@ - The Firewall Manager policy administrator is being
--     removed as a tenant administrator.
--
-- -   @OFFBOARD_COMPLETE@ - The Firewall Manager policy administrator has
--     been removed as a tenant administrator.
--
-- -   @NOT_EXIST@ - The Firewall Manager policy administrator doesn\'t
--     exist as a tenant administrator.
associateThirdPartyFirewallResponse_thirdPartyFirewallStatus :: Lens.Lens' AssociateThirdPartyFirewallResponse (Prelude.Maybe ThirdPartyFirewallAssociationStatus)
associateThirdPartyFirewallResponse_thirdPartyFirewallStatus = Lens.lens (\AssociateThirdPartyFirewallResponse' {thirdPartyFirewallStatus} -> thirdPartyFirewallStatus) (\s@AssociateThirdPartyFirewallResponse' {} a -> s {thirdPartyFirewallStatus = a} :: AssociateThirdPartyFirewallResponse)

-- | The response's http status code.
associateThirdPartyFirewallResponse_httpStatus :: Lens.Lens' AssociateThirdPartyFirewallResponse Prelude.Int
associateThirdPartyFirewallResponse_httpStatus = Lens.lens (\AssociateThirdPartyFirewallResponse' {httpStatus} -> httpStatus) (\s@AssociateThirdPartyFirewallResponse' {} a -> s {httpStatus = a} :: AssociateThirdPartyFirewallResponse)

instance
  Prelude.NFData
    AssociateThirdPartyFirewallResponse
  where
  rnf AssociateThirdPartyFirewallResponse' {..} =
    Prelude.rnf thirdPartyFirewallStatus `Prelude.seq`
      Prelude.rnf httpStatus
