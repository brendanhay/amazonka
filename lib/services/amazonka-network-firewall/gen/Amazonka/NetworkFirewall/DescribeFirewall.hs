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
-- Module      : Amazonka.NetworkFirewall.DescribeFirewall
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the data objects for the specified firewall.
module Amazonka.NetworkFirewall.DescribeFirewall
  ( -- * Creating a Request
    DescribeFirewall (..),
    newDescribeFirewall,

    -- * Request Lenses
    describeFirewall_firewallArn,
    describeFirewall_firewallName,

    -- * Destructuring the Response
    DescribeFirewallResponse (..),
    newDescribeFirewallResponse,

    -- * Response Lenses
    describeFirewallResponse_firewall,
    describeFirewallResponse_firewallStatus,
    describeFirewallResponse_updateToken,
    describeFirewallResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFirewall' smart constructor.
data DescribeFirewall = DescribeFirewall'
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
-- Create a value of 'DescribeFirewall' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallArn', 'describeFirewall_firewallArn' - The Amazon Resource Name (ARN) of the firewall.
--
-- You must specify the ARN or the name, and you can specify both.
--
-- 'firewallName', 'describeFirewall_firewallName' - The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
newDescribeFirewall ::
  DescribeFirewall
newDescribeFirewall =
  DescribeFirewall'
    { firewallArn = Prelude.Nothing,
      firewallName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the firewall.
--
-- You must specify the ARN or the name, and you can specify both.
describeFirewall_firewallArn :: Lens.Lens' DescribeFirewall (Prelude.Maybe Prelude.Text)
describeFirewall_firewallArn = Lens.lens (\DescribeFirewall' {firewallArn} -> firewallArn) (\s@DescribeFirewall' {} a -> s {firewallArn = a} :: DescribeFirewall)

-- | The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
describeFirewall_firewallName :: Lens.Lens' DescribeFirewall (Prelude.Maybe Prelude.Text)
describeFirewall_firewallName = Lens.lens (\DescribeFirewall' {firewallName} -> firewallName) (\s@DescribeFirewall' {} a -> s {firewallName = a} :: DescribeFirewall)

instance Core.AWSRequest DescribeFirewall where
  type
    AWSResponse DescribeFirewall =
      DescribeFirewallResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFirewallResponse'
            Prelude.<$> (x Data..?> "Firewall")
            Prelude.<*> (x Data..?> "FirewallStatus")
            Prelude.<*> (x Data..?> "UpdateToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFirewall where
  hashWithSalt _salt DescribeFirewall' {..} =
    _salt `Prelude.hashWithSalt` firewallArn
      `Prelude.hashWithSalt` firewallName

instance Prelude.NFData DescribeFirewall where
  rnf DescribeFirewall' {..} =
    Prelude.rnf firewallArn
      `Prelude.seq` Prelude.rnf firewallName

instance Data.ToHeaders DescribeFirewall where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "NetworkFirewall_20201112.DescribeFirewall" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeFirewall where
  toJSON DescribeFirewall' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FirewallArn" Data..=) Prelude.<$> firewallArn,
            ("FirewallName" Data..=) Prelude.<$> firewallName
          ]
      )

instance Data.ToPath DescribeFirewall where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeFirewall where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFirewallResponse' smart constructor.
data DescribeFirewallResponse = DescribeFirewallResponse'
  { -- | The configuration settings for the firewall. These settings include the
    -- firewall policy and the subnets in your VPC to use for the firewall
    -- endpoints.
    firewall :: Prelude.Maybe Firewall,
    -- | Detailed information about the current status of a Firewall. You can
    -- retrieve this for a firewall by calling DescribeFirewall and providing
    -- the firewall name and ARN.
    firewallStatus :: Prelude.Maybe FirewallStatus,
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
-- Create a value of 'DescribeFirewallResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewall', 'describeFirewallResponse_firewall' - The configuration settings for the firewall. These settings include the
-- firewall policy and the subnets in your VPC to use for the firewall
-- endpoints.
--
-- 'firewallStatus', 'describeFirewallResponse_firewallStatus' - Detailed information about the current status of a Firewall. You can
-- retrieve this for a firewall by calling DescribeFirewall and providing
-- the firewall name and ARN.
--
-- 'updateToken', 'describeFirewallResponse_updateToken' - An optional token that you can use for optimistic locking. Network
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
-- 'httpStatus', 'describeFirewallResponse_httpStatus' - The response's http status code.
newDescribeFirewallResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFirewallResponse
newDescribeFirewallResponse pHttpStatus_ =
  DescribeFirewallResponse'
    { firewall =
        Prelude.Nothing,
      firewallStatus = Prelude.Nothing,
      updateToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The configuration settings for the firewall. These settings include the
-- firewall policy and the subnets in your VPC to use for the firewall
-- endpoints.
describeFirewallResponse_firewall :: Lens.Lens' DescribeFirewallResponse (Prelude.Maybe Firewall)
describeFirewallResponse_firewall = Lens.lens (\DescribeFirewallResponse' {firewall} -> firewall) (\s@DescribeFirewallResponse' {} a -> s {firewall = a} :: DescribeFirewallResponse)

-- | Detailed information about the current status of a Firewall. You can
-- retrieve this for a firewall by calling DescribeFirewall and providing
-- the firewall name and ARN.
describeFirewallResponse_firewallStatus :: Lens.Lens' DescribeFirewallResponse (Prelude.Maybe FirewallStatus)
describeFirewallResponse_firewallStatus = Lens.lens (\DescribeFirewallResponse' {firewallStatus} -> firewallStatus) (\s@DescribeFirewallResponse' {} a -> s {firewallStatus = a} :: DescribeFirewallResponse)

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
describeFirewallResponse_updateToken :: Lens.Lens' DescribeFirewallResponse (Prelude.Maybe Prelude.Text)
describeFirewallResponse_updateToken = Lens.lens (\DescribeFirewallResponse' {updateToken} -> updateToken) (\s@DescribeFirewallResponse' {} a -> s {updateToken = a} :: DescribeFirewallResponse)

-- | The response's http status code.
describeFirewallResponse_httpStatus :: Lens.Lens' DescribeFirewallResponse Prelude.Int
describeFirewallResponse_httpStatus = Lens.lens (\DescribeFirewallResponse' {httpStatus} -> httpStatus) (\s@DescribeFirewallResponse' {} a -> s {httpStatus = a} :: DescribeFirewallResponse)

instance Prelude.NFData DescribeFirewallResponse where
  rnf DescribeFirewallResponse' {..} =
    Prelude.rnf firewall
      `Prelude.seq` Prelude.rnf firewallStatus
      `Prelude.seq` Prelude.rnf updateToken
      `Prelude.seq` Prelude.rnf httpStatus
