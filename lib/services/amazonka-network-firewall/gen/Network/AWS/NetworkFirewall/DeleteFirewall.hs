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
-- Module      : Network.AWS.NetworkFirewall.DeleteFirewall
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Firewall and its FirewallStatus. This operation
-- requires the firewall\'s @DeleteProtection@ flag to be @FALSE@. You
-- can\'t revert this operation.
--
-- You can check whether a firewall is in use by reviewing the route tables
-- for the Availability Zones where you have firewall subnet mappings.
-- Retrieve the subnet mappings by calling DescribeFirewall. You define and
-- update the route tables through Amazon VPC. As needed, update the route
-- tables for the zones to remove the firewall endpoints. When the route
-- tables no longer use the firewall endpoints, you can remove the firewall
-- safely.
--
-- To delete a firewall, remove the delete protection if you need to using
-- UpdateFirewallDeleteProtection, then delete the firewall by calling
-- DeleteFirewall.
module Network.AWS.NetworkFirewall.DeleteFirewall
  ( -- * Creating a Request
    DeleteFirewall (..),
    newDeleteFirewall,

    -- * Request Lenses
    deleteFirewall_firewallArn,
    deleteFirewall_firewallName,

    -- * Destructuring the Response
    DeleteFirewallResponse (..),
    newDeleteFirewallResponse,

    -- * Response Lenses
    deleteFirewallResponse_firewallStatus,
    deleteFirewallResponse_firewall,
    deleteFirewallResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.NetworkFirewall.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteFirewall' smart constructor.
data DeleteFirewall = DeleteFirewall'
  { -- | The Amazon Resource Name (ARN) of the firewall.
    --
    -- You must specify the ARN or the name, and you can specify both.
    firewallArn :: Prelude.Maybe Prelude.Text,
    -- | The descriptive name of the firewall. You can\'t change the name of a
    -- firewall after you create it.
    --
    -- You must specify the ARN or the name, and you can specify both.
    firewallName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFirewall' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallArn', 'deleteFirewall_firewallArn' - The Amazon Resource Name (ARN) of the firewall.
--
-- You must specify the ARN or the name, and you can specify both.
--
-- 'firewallName', 'deleteFirewall_firewallName' - The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
newDeleteFirewall ::
  DeleteFirewall
newDeleteFirewall =
  DeleteFirewall'
    { firewallArn = Prelude.Nothing,
      firewallName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the firewall.
--
-- You must specify the ARN or the name, and you can specify both.
deleteFirewall_firewallArn :: Lens.Lens' DeleteFirewall (Prelude.Maybe Prelude.Text)
deleteFirewall_firewallArn = Lens.lens (\DeleteFirewall' {firewallArn} -> firewallArn) (\s@DeleteFirewall' {} a -> s {firewallArn = a} :: DeleteFirewall)

-- | The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
deleteFirewall_firewallName :: Lens.Lens' DeleteFirewall (Prelude.Maybe Prelude.Text)
deleteFirewall_firewallName = Lens.lens (\DeleteFirewall' {firewallName} -> firewallName) (\s@DeleteFirewall' {} a -> s {firewallName = a} :: DeleteFirewall)

instance Core.AWSRequest DeleteFirewall where
  type
    AWSResponse DeleteFirewall =
      DeleteFirewallResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteFirewallResponse'
            Prelude.<$> (x Core..?> "FirewallStatus")
            Prelude.<*> (x Core..?> "Firewall")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteFirewall

instance Prelude.NFData DeleteFirewall

instance Core.ToHeaders DeleteFirewall where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "NetworkFirewall_20201112.DeleteFirewall" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteFirewall where
  toJSON DeleteFirewall' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FirewallArn" Core..=) Prelude.<$> firewallArn,
            ("FirewallName" Core..=) Prelude.<$> firewallName
          ]
      )

instance Core.ToPath DeleteFirewall where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteFirewall where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFirewallResponse' smart constructor.
data DeleteFirewallResponse = DeleteFirewallResponse'
  { firewallStatus :: Prelude.Maybe FirewallStatus,
    firewall :: Prelude.Maybe Firewall,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFirewallResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallStatus', 'deleteFirewallResponse_firewallStatus' - Undocumented member.
--
-- 'firewall', 'deleteFirewallResponse_firewall' - Undocumented member.
--
-- 'httpStatus', 'deleteFirewallResponse_httpStatus' - The response's http status code.
newDeleteFirewallResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteFirewallResponse
newDeleteFirewallResponse pHttpStatus_ =
  DeleteFirewallResponse'
    { firewallStatus =
        Prelude.Nothing,
      firewall = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deleteFirewallResponse_firewallStatus :: Lens.Lens' DeleteFirewallResponse (Prelude.Maybe FirewallStatus)
deleteFirewallResponse_firewallStatus = Lens.lens (\DeleteFirewallResponse' {firewallStatus} -> firewallStatus) (\s@DeleteFirewallResponse' {} a -> s {firewallStatus = a} :: DeleteFirewallResponse)

-- | Undocumented member.
deleteFirewallResponse_firewall :: Lens.Lens' DeleteFirewallResponse (Prelude.Maybe Firewall)
deleteFirewallResponse_firewall = Lens.lens (\DeleteFirewallResponse' {firewall} -> firewall) (\s@DeleteFirewallResponse' {} a -> s {firewall = a} :: DeleteFirewallResponse)

-- | The response's http status code.
deleteFirewallResponse_httpStatus :: Lens.Lens' DeleteFirewallResponse Prelude.Int
deleteFirewallResponse_httpStatus = Lens.lens (\DeleteFirewallResponse' {httpStatus} -> httpStatus) (\s@DeleteFirewallResponse' {} a -> s {httpStatus = a} :: DeleteFirewallResponse)

instance Prelude.NFData DeleteFirewallResponse
