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
-- Module      : Amazonka.NetworkFirewall.AssociateSubnets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified subnets in the Amazon VPC to the firewall. You
-- can specify one subnet for each of the Availability Zones that the VPC
-- spans.
--
-- This request creates an Network Firewall firewall endpoint in each of
-- the subnets. To enable the firewall\'s protections, you must also modify
-- the VPC\'s route tables for each subnet\'s Availability Zone, to
-- redirect the traffic that\'s coming into and going out of the zone
-- through the firewall endpoint.
module Amazonka.NetworkFirewall.AssociateSubnets
  ( -- * Creating a Request
    AssociateSubnets (..),
    newAssociateSubnets,

    -- * Request Lenses
    associateSubnets_firewallArn,
    associateSubnets_firewallName,
    associateSubnets_updateToken,
    associateSubnets_subnetMappings,

    -- * Destructuring the Response
    AssociateSubnetsResponse (..),
    newAssociateSubnetsResponse,

    -- * Response Lenses
    associateSubnetsResponse_firewallArn,
    associateSubnetsResponse_firewallName,
    associateSubnetsResponse_subnetMappings,
    associateSubnetsResponse_updateToken,
    associateSubnetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateSubnets' smart constructor.
data AssociateSubnets = AssociateSubnets'
  { -- | The Amazon Resource Name (ARN) of the firewall.
    --
    -- You must specify the ARN or the name, and you can specify both.
    firewallArn :: Prelude.Maybe Prelude.Text,
    -- | The descriptive name of the firewall. You can\'t change the name of a
    -- firewall after you create it.
    --
    -- You must specify the ARN or the name, and you can specify both.
    firewallName :: Prelude.Maybe Prelude.Text,
    -- | An optional token that you can use for optimistic locking. Network
    -- Firewall returns a token to your requests that access the firewall. The
    -- token marks the state of the firewall resource at the time of the
    -- request.
    --
    -- To make an unconditional change to the firewall, omit the token in your
    -- update request. Without the token, Network Firewall performs your
    -- updates regardless of whether the firewall has changed since you last
    -- retrieved it.
    --
    -- To make a conditional change to the firewall, provide the token in your
    -- update request. Network Firewall uses the token to ensure that the
    -- firewall hasn\'t changed since you last retrieved it. If it has changed,
    -- the operation fails with an @InvalidTokenException@. If this happens,
    -- retrieve the firewall again to get a current copy of it with a new
    -- token. Reapply your changes as needed, then try the operation again
    -- using the new token.
    updateToken :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the subnets that you want to associate with the firewall.
    subnetMappings :: [SubnetMapping]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateSubnets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallArn', 'associateSubnets_firewallArn' - The Amazon Resource Name (ARN) of the firewall.
--
-- You must specify the ARN or the name, and you can specify both.
--
-- 'firewallName', 'associateSubnets_firewallName' - The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
--
-- 'updateToken', 'associateSubnets_updateToken' - An optional token that you can use for optimistic locking. Network
-- Firewall returns a token to your requests that access the firewall. The
-- token marks the state of the firewall resource at the time of the
-- request.
--
-- To make an unconditional change to the firewall, omit the token in your
-- update request. Without the token, Network Firewall performs your
-- updates regardless of whether the firewall has changed since you last
-- retrieved it.
--
-- To make a conditional change to the firewall, provide the token in your
-- update request. Network Firewall uses the token to ensure that the
-- firewall hasn\'t changed since you last retrieved it. If it has changed,
-- the operation fails with an @InvalidTokenException@. If this happens,
-- retrieve the firewall again to get a current copy of it with a new
-- token. Reapply your changes as needed, then try the operation again
-- using the new token.
--
-- 'subnetMappings', 'associateSubnets_subnetMappings' - The IDs of the subnets that you want to associate with the firewall.
newAssociateSubnets ::
  AssociateSubnets
newAssociateSubnets =
  AssociateSubnets'
    { firewallArn = Prelude.Nothing,
      firewallName = Prelude.Nothing,
      updateToken = Prelude.Nothing,
      subnetMappings = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) of the firewall.
--
-- You must specify the ARN or the name, and you can specify both.
associateSubnets_firewallArn :: Lens.Lens' AssociateSubnets (Prelude.Maybe Prelude.Text)
associateSubnets_firewallArn = Lens.lens (\AssociateSubnets' {firewallArn} -> firewallArn) (\s@AssociateSubnets' {} a -> s {firewallArn = a} :: AssociateSubnets)

-- | The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
associateSubnets_firewallName :: Lens.Lens' AssociateSubnets (Prelude.Maybe Prelude.Text)
associateSubnets_firewallName = Lens.lens (\AssociateSubnets' {firewallName} -> firewallName) (\s@AssociateSubnets' {} a -> s {firewallName = a} :: AssociateSubnets)

-- | An optional token that you can use for optimistic locking. Network
-- Firewall returns a token to your requests that access the firewall. The
-- token marks the state of the firewall resource at the time of the
-- request.
--
-- To make an unconditional change to the firewall, omit the token in your
-- update request. Without the token, Network Firewall performs your
-- updates regardless of whether the firewall has changed since you last
-- retrieved it.
--
-- To make a conditional change to the firewall, provide the token in your
-- update request. Network Firewall uses the token to ensure that the
-- firewall hasn\'t changed since you last retrieved it. If it has changed,
-- the operation fails with an @InvalidTokenException@. If this happens,
-- retrieve the firewall again to get a current copy of it with a new
-- token. Reapply your changes as needed, then try the operation again
-- using the new token.
associateSubnets_updateToken :: Lens.Lens' AssociateSubnets (Prelude.Maybe Prelude.Text)
associateSubnets_updateToken = Lens.lens (\AssociateSubnets' {updateToken} -> updateToken) (\s@AssociateSubnets' {} a -> s {updateToken = a} :: AssociateSubnets)

-- | The IDs of the subnets that you want to associate with the firewall.
associateSubnets_subnetMappings :: Lens.Lens' AssociateSubnets [SubnetMapping]
associateSubnets_subnetMappings = Lens.lens (\AssociateSubnets' {subnetMappings} -> subnetMappings) (\s@AssociateSubnets' {} a -> s {subnetMappings = a} :: AssociateSubnets) Prelude.. Lens.coerced

instance Core.AWSRequest AssociateSubnets where
  type
    AWSResponse AssociateSubnets =
      AssociateSubnetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateSubnetsResponse'
            Prelude.<$> (x Data..?> "FirewallArn")
            Prelude.<*> (x Data..?> "FirewallName")
            Prelude.<*> (x Data..?> "SubnetMappings" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "UpdateToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateSubnets where
  hashWithSalt _salt AssociateSubnets' {..} =
    _salt
      `Prelude.hashWithSalt` firewallArn
      `Prelude.hashWithSalt` firewallName
      `Prelude.hashWithSalt` updateToken
      `Prelude.hashWithSalt` subnetMappings

instance Prelude.NFData AssociateSubnets where
  rnf AssociateSubnets' {..} =
    Prelude.rnf firewallArn
      `Prelude.seq` Prelude.rnf firewallName
      `Prelude.seq` Prelude.rnf updateToken
      `Prelude.seq` Prelude.rnf subnetMappings

instance Data.ToHeaders AssociateSubnets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "NetworkFirewall_20201112.AssociateSubnets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateSubnets where
  toJSON AssociateSubnets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FirewallArn" Data..=) Prelude.<$> firewallArn,
            ("FirewallName" Data..=) Prelude.<$> firewallName,
            ("UpdateToken" Data..=) Prelude.<$> updateToken,
            Prelude.Just
              ("SubnetMappings" Data..= subnetMappings)
          ]
      )

instance Data.ToPath AssociateSubnets where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateSubnets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateSubnetsResponse' smart constructor.
data AssociateSubnetsResponse = AssociateSubnetsResponse'
  { -- | The Amazon Resource Name (ARN) of the firewall.
    firewallArn :: Prelude.Maybe Prelude.Text,
    -- | The descriptive name of the firewall. You can\'t change the name of a
    -- firewall after you create it.
    firewallName :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the subnets that are associated with the firewall.
    subnetMappings :: Prelude.Maybe [SubnetMapping],
    -- | An optional token that you can use for optimistic locking. Network
    -- Firewall returns a token to your requests that access the firewall. The
    -- token marks the state of the firewall resource at the time of the
    -- request.
    --
    -- To make an unconditional change to the firewall, omit the token in your
    -- update request. Without the token, Network Firewall performs your
    -- updates regardless of whether the firewall has changed since you last
    -- retrieved it.
    --
    -- To make a conditional change to the firewall, provide the token in your
    -- update request. Network Firewall uses the token to ensure that the
    -- firewall hasn\'t changed since you last retrieved it. If it has changed,
    -- the operation fails with an @InvalidTokenException@. If this happens,
    -- retrieve the firewall again to get a current copy of it with a new
    -- token. Reapply your changes as needed, then try the operation again
    -- using the new token.
    updateToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateSubnetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallArn', 'associateSubnetsResponse_firewallArn' - The Amazon Resource Name (ARN) of the firewall.
--
-- 'firewallName', 'associateSubnetsResponse_firewallName' - The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
--
-- 'subnetMappings', 'associateSubnetsResponse_subnetMappings' - The IDs of the subnets that are associated with the firewall.
--
-- 'updateToken', 'associateSubnetsResponse_updateToken' - An optional token that you can use for optimistic locking. Network
-- Firewall returns a token to your requests that access the firewall. The
-- token marks the state of the firewall resource at the time of the
-- request.
--
-- To make an unconditional change to the firewall, omit the token in your
-- update request. Without the token, Network Firewall performs your
-- updates regardless of whether the firewall has changed since you last
-- retrieved it.
--
-- To make a conditional change to the firewall, provide the token in your
-- update request. Network Firewall uses the token to ensure that the
-- firewall hasn\'t changed since you last retrieved it. If it has changed,
-- the operation fails with an @InvalidTokenException@. If this happens,
-- retrieve the firewall again to get a current copy of it with a new
-- token. Reapply your changes as needed, then try the operation again
-- using the new token.
--
-- 'httpStatus', 'associateSubnetsResponse_httpStatus' - The response's http status code.
newAssociateSubnetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateSubnetsResponse
newAssociateSubnetsResponse pHttpStatus_ =
  AssociateSubnetsResponse'
    { firewallArn =
        Prelude.Nothing,
      firewallName = Prelude.Nothing,
      subnetMappings = Prelude.Nothing,
      updateToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the firewall.
associateSubnetsResponse_firewallArn :: Lens.Lens' AssociateSubnetsResponse (Prelude.Maybe Prelude.Text)
associateSubnetsResponse_firewallArn = Lens.lens (\AssociateSubnetsResponse' {firewallArn} -> firewallArn) (\s@AssociateSubnetsResponse' {} a -> s {firewallArn = a} :: AssociateSubnetsResponse)

-- | The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
associateSubnetsResponse_firewallName :: Lens.Lens' AssociateSubnetsResponse (Prelude.Maybe Prelude.Text)
associateSubnetsResponse_firewallName = Lens.lens (\AssociateSubnetsResponse' {firewallName} -> firewallName) (\s@AssociateSubnetsResponse' {} a -> s {firewallName = a} :: AssociateSubnetsResponse)

-- | The IDs of the subnets that are associated with the firewall.
associateSubnetsResponse_subnetMappings :: Lens.Lens' AssociateSubnetsResponse (Prelude.Maybe [SubnetMapping])
associateSubnetsResponse_subnetMappings = Lens.lens (\AssociateSubnetsResponse' {subnetMappings} -> subnetMappings) (\s@AssociateSubnetsResponse' {} a -> s {subnetMappings = a} :: AssociateSubnetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An optional token that you can use for optimistic locking. Network
-- Firewall returns a token to your requests that access the firewall. The
-- token marks the state of the firewall resource at the time of the
-- request.
--
-- To make an unconditional change to the firewall, omit the token in your
-- update request. Without the token, Network Firewall performs your
-- updates regardless of whether the firewall has changed since you last
-- retrieved it.
--
-- To make a conditional change to the firewall, provide the token in your
-- update request. Network Firewall uses the token to ensure that the
-- firewall hasn\'t changed since you last retrieved it. If it has changed,
-- the operation fails with an @InvalidTokenException@. If this happens,
-- retrieve the firewall again to get a current copy of it with a new
-- token. Reapply your changes as needed, then try the operation again
-- using the new token.
associateSubnetsResponse_updateToken :: Lens.Lens' AssociateSubnetsResponse (Prelude.Maybe Prelude.Text)
associateSubnetsResponse_updateToken = Lens.lens (\AssociateSubnetsResponse' {updateToken} -> updateToken) (\s@AssociateSubnetsResponse' {} a -> s {updateToken = a} :: AssociateSubnetsResponse)

-- | The response's http status code.
associateSubnetsResponse_httpStatus :: Lens.Lens' AssociateSubnetsResponse Prelude.Int
associateSubnetsResponse_httpStatus = Lens.lens (\AssociateSubnetsResponse' {httpStatus} -> httpStatus) (\s@AssociateSubnetsResponse' {} a -> s {httpStatus = a} :: AssociateSubnetsResponse)

instance Prelude.NFData AssociateSubnetsResponse where
  rnf AssociateSubnetsResponse' {..} =
    Prelude.rnf firewallArn
      `Prelude.seq` Prelude.rnf firewallName
      `Prelude.seq` Prelude.rnf subnetMappings
      `Prelude.seq` Prelude.rnf updateToken
      `Prelude.seq` Prelude.rnf httpStatus
