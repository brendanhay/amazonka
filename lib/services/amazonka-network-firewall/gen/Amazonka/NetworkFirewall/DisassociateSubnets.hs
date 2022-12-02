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
-- Module      : Amazonka.NetworkFirewall.DisassociateSubnets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified subnet associations from the firewall. This
-- removes the firewall endpoints from the subnets and removes any network
-- filtering protections that the endpoints were providing.
module Amazonka.NetworkFirewall.DisassociateSubnets
  ( -- * Creating a Request
    DisassociateSubnets (..),
    newDisassociateSubnets,

    -- * Request Lenses
    disassociateSubnets_updateToken,
    disassociateSubnets_firewallArn,
    disassociateSubnets_firewallName,
    disassociateSubnets_subnetIds,

    -- * Destructuring the Response
    DisassociateSubnetsResponse (..),
    newDisassociateSubnetsResponse,

    -- * Response Lenses
    disassociateSubnetsResponse_updateToken,
    disassociateSubnetsResponse_firewallArn,
    disassociateSubnetsResponse_subnetMappings,
    disassociateSubnetsResponse_firewallName,
    disassociateSubnetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateSubnets' smart constructor.
data DisassociateSubnets = DisassociateSubnets'
  { -- | An optional token that you can use for optimistic locking. Network
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
    -- | The Amazon Resource Name (ARN) of the firewall.
    --
    -- You must specify the ARN or the name, and you can specify both.
    firewallArn :: Prelude.Maybe Prelude.Text,
    -- | The descriptive name of the firewall. You can\'t change the name of a
    -- firewall after you create it.
    --
    -- You must specify the ARN or the name, and you can specify both.
    firewallName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifiers for the subnets that you want to disassociate.
    subnetIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateSubnets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updateToken', 'disassociateSubnets_updateToken' - An optional token that you can use for optimistic locking. Network
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
-- 'firewallArn', 'disassociateSubnets_firewallArn' - The Amazon Resource Name (ARN) of the firewall.
--
-- You must specify the ARN or the name, and you can specify both.
--
-- 'firewallName', 'disassociateSubnets_firewallName' - The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
--
-- 'subnetIds', 'disassociateSubnets_subnetIds' - The unique identifiers for the subnets that you want to disassociate.
newDisassociateSubnets ::
  DisassociateSubnets
newDisassociateSubnets =
  DisassociateSubnets'
    { updateToken = Prelude.Nothing,
      firewallArn = Prelude.Nothing,
      firewallName = Prelude.Nothing,
      subnetIds = Prelude.mempty
    }

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
disassociateSubnets_updateToken :: Lens.Lens' DisassociateSubnets (Prelude.Maybe Prelude.Text)
disassociateSubnets_updateToken = Lens.lens (\DisassociateSubnets' {updateToken} -> updateToken) (\s@DisassociateSubnets' {} a -> s {updateToken = a} :: DisassociateSubnets)

-- | The Amazon Resource Name (ARN) of the firewall.
--
-- You must specify the ARN or the name, and you can specify both.
disassociateSubnets_firewallArn :: Lens.Lens' DisassociateSubnets (Prelude.Maybe Prelude.Text)
disassociateSubnets_firewallArn = Lens.lens (\DisassociateSubnets' {firewallArn} -> firewallArn) (\s@DisassociateSubnets' {} a -> s {firewallArn = a} :: DisassociateSubnets)

-- | The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
disassociateSubnets_firewallName :: Lens.Lens' DisassociateSubnets (Prelude.Maybe Prelude.Text)
disassociateSubnets_firewallName = Lens.lens (\DisassociateSubnets' {firewallName} -> firewallName) (\s@DisassociateSubnets' {} a -> s {firewallName = a} :: DisassociateSubnets)

-- | The unique identifiers for the subnets that you want to disassociate.
disassociateSubnets_subnetIds :: Lens.Lens' DisassociateSubnets [Prelude.Text]
disassociateSubnets_subnetIds = Lens.lens (\DisassociateSubnets' {subnetIds} -> subnetIds) (\s@DisassociateSubnets' {} a -> s {subnetIds = a} :: DisassociateSubnets) Prelude.. Lens.coerced

instance Core.AWSRequest DisassociateSubnets where
  type
    AWSResponse DisassociateSubnets =
      DisassociateSubnetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateSubnetsResponse'
            Prelude.<$> (x Data..?> "UpdateToken")
            Prelude.<*> (x Data..?> "FirewallArn")
            Prelude.<*> (x Data..?> "SubnetMappings" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "FirewallName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateSubnets where
  hashWithSalt _salt DisassociateSubnets' {..} =
    _salt `Prelude.hashWithSalt` updateToken
      `Prelude.hashWithSalt` firewallArn
      `Prelude.hashWithSalt` firewallName
      `Prelude.hashWithSalt` subnetIds

instance Prelude.NFData DisassociateSubnets where
  rnf DisassociateSubnets' {..} =
    Prelude.rnf updateToken
      `Prelude.seq` Prelude.rnf firewallArn
      `Prelude.seq` Prelude.rnf firewallName
      `Prelude.seq` Prelude.rnf subnetIds

instance Data.ToHeaders DisassociateSubnets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "NetworkFirewall_20201112.DisassociateSubnets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateSubnets where
  toJSON DisassociateSubnets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("UpdateToken" Data..=) Prelude.<$> updateToken,
            ("FirewallArn" Data..=) Prelude.<$> firewallArn,
            ("FirewallName" Data..=) Prelude.<$> firewallName,
            Prelude.Just ("SubnetIds" Data..= subnetIds)
          ]
      )

instance Data.ToPath DisassociateSubnets where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateSubnets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateSubnetsResponse' smart constructor.
data DisassociateSubnetsResponse = DisassociateSubnetsResponse'
  { -- | An optional token that you can use for optimistic locking. Network
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
    -- | The Amazon Resource Name (ARN) of the firewall.
    firewallArn :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the subnets that are associated with the firewall.
    subnetMappings :: Prelude.Maybe [SubnetMapping],
    -- | The descriptive name of the firewall. You can\'t change the name of a
    -- firewall after you create it.
    firewallName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateSubnetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updateToken', 'disassociateSubnetsResponse_updateToken' - An optional token that you can use for optimistic locking. Network
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
-- 'firewallArn', 'disassociateSubnetsResponse_firewallArn' - The Amazon Resource Name (ARN) of the firewall.
--
-- 'subnetMappings', 'disassociateSubnetsResponse_subnetMappings' - The IDs of the subnets that are associated with the firewall.
--
-- 'firewallName', 'disassociateSubnetsResponse_firewallName' - The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
--
-- 'httpStatus', 'disassociateSubnetsResponse_httpStatus' - The response's http status code.
newDisassociateSubnetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateSubnetsResponse
newDisassociateSubnetsResponse pHttpStatus_ =
  DisassociateSubnetsResponse'
    { updateToken =
        Prelude.Nothing,
      firewallArn = Prelude.Nothing,
      subnetMappings = Prelude.Nothing,
      firewallName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

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
disassociateSubnetsResponse_updateToken :: Lens.Lens' DisassociateSubnetsResponse (Prelude.Maybe Prelude.Text)
disassociateSubnetsResponse_updateToken = Lens.lens (\DisassociateSubnetsResponse' {updateToken} -> updateToken) (\s@DisassociateSubnetsResponse' {} a -> s {updateToken = a} :: DisassociateSubnetsResponse)

-- | The Amazon Resource Name (ARN) of the firewall.
disassociateSubnetsResponse_firewallArn :: Lens.Lens' DisassociateSubnetsResponse (Prelude.Maybe Prelude.Text)
disassociateSubnetsResponse_firewallArn = Lens.lens (\DisassociateSubnetsResponse' {firewallArn} -> firewallArn) (\s@DisassociateSubnetsResponse' {} a -> s {firewallArn = a} :: DisassociateSubnetsResponse)

-- | The IDs of the subnets that are associated with the firewall.
disassociateSubnetsResponse_subnetMappings :: Lens.Lens' DisassociateSubnetsResponse (Prelude.Maybe [SubnetMapping])
disassociateSubnetsResponse_subnetMappings = Lens.lens (\DisassociateSubnetsResponse' {subnetMappings} -> subnetMappings) (\s@DisassociateSubnetsResponse' {} a -> s {subnetMappings = a} :: DisassociateSubnetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
disassociateSubnetsResponse_firewallName :: Lens.Lens' DisassociateSubnetsResponse (Prelude.Maybe Prelude.Text)
disassociateSubnetsResponse_firewallName = Lens.lens (\DisassociateSubnetsResponse' {firewallName} -> firewallName) (\s@DisassociateSubnetsResponse' {} a -> s {firewallName = a} :: DisassociateSubnetsResponse)

-- | The response's http status code.
disassociateSubnetsResponse_httpStatus :: Lens.Lens' DisassociateSubnetsResponse Prelude.Int
disassociateSubnetsResponse_httpStatus = Lens.lens (\DisassociateSubnetsResponse' {httpStatus} -> httpStatus) (\s@DisassociateSubnetsResponse' {} a -> s {httpStatus = a} :: DisassociateSubnetsResponse)

instance Prelude.NFData DisassociateSubnetsResponse where
  rnf DisassociateSubnetsResponse' {..} =
    Prelude.rnf updateToken
      `Prelude.seq` Prelude.rnf firewallArn
      `Prelude.seq` Prelude.rnf subnetMappings
      `Prelude.seq` Prelude.rnf firewallName
      `Prelude.seq` Prelude.rnf httpStatus
