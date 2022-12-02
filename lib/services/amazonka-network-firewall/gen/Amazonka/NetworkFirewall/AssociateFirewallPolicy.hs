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
-- Module      : Amazonka.NetworkFirewall.AssociateFirewallPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a FirewallPolicy to a Firewall.
--
-- A firewall policy defines how to monitor and manage your VPC network
-- traffic, using a collection of inspection rule groups and other
-- settings. Each firewall requires one firewall policy association, and
-- you can use the same firewall policy for multiple firewalls.
module Amazonka.NetworkFirewall.AssociateFirewallPolicy
  ( -- * Creating a Request
    AssociateFirewallPolicy (..),
    newAssociateFirewallPolicy,

    -- * Request Lenses
    associateFirewallPolicy_updateToken,
    associateFirewallPolicy_firewallArn,
    associateFirewallPolicy_firewallName,
    associateFirewallPolicy_firewallPolicyArn,

    -- * Destructuring the Response
    AssociateFirewallPolicyResponse (..),
    newAssociateFirewallPolicyResponse,

    -- * Response Lenses
    associateFirewallPolicyResponse_updateToken,
    associateFirewallPolicyResponse_firewallArn,
    associateFirewallPolicyResponse_firewallName,
    associateFirewallPolicyResponse_firewallPolicyArn,
    associateFirewallPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateFirewallPolicy' smart constructor.
data AssociateFirewallPolicy = AssociateFirewallPolicy'
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
    -- | The Amazon Resource Name (ARN) of the firewall policy.
    firewallPolicyArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateFirewallPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updateToken', 'associateFirewallPolicy_updateToken' - An optional token that you can use for optimistic locking. Network
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
-- 'firewallArn', 'associateFirewallPolicy_firewallArn' - The Amazon Resource Name (ARN) of the firewall.
--
-- You must specify the ARN or the name, and you can specify both.
--
-- 'firewallName', 'associateFirewallPolicy_firewallName' - The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
--
-- 'firewallPolicyArn', 'associateFirewallPolicy_firewallPolicyArn' - The Amazon Resource Name (ARN) of the firewall policy.
newAssociateFirewallPolicy ::
  -- | 'firewallPolicyArn'
  Prelude.Text ->
  AssociateFirewallPolicy
newAssociateFirewallPolicy pFirewallPolicyArn_ =
  AssociateFirewallPolicy'
    { updateToken =
        Prelude.Nothing,
      firewallArn = Prelude.Nothing,
      firewallName = Prelude.Nothing,
      firewallPolicyArn = pFirewallPolicyArn_
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
associateFirewallPolicy_updateToken :: Lens.Lens' AssociateFirewallPolicy (Prelude.Maybe Prelude.Text)
associateFirewallPolicy_updateToken = Lens.lens (\AssociateFirewallPolicy' {updateToken} -> updateToken) (\s@AssociateFirewallPolicy' {} a -> s {updateToken = a} :: AssociateFirewallPolicy)

-- | The Amazon Resource Name (ARN) of the firewall.
--
-- You must specify the ARN or the name, and you can specify both.
associateFirewallPolicy_firewallArn :: Lens.Lens' AssociateFirewallPolicy (Prelude.Maybe Prelude.Text)
associateFirewallPolicy_firewallArn = Lens.lens (\AssociateFirewallPolicy' {firewallArn} -> firewallArn) (\s@AssociateFirewallPolicy' {} a -> s {firewallArn = a} :: AssociateFirewallPolicy)

-- | The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
associateFirewallPolicy_firewallName :: Lens.Lens' AssociateFirewallPolicy (Prelude.Maybe Prelude.Text)
associateFirewallPolicy_firewallName = Lens.lens (\AssociateFirewallPolicy' {firewallName} -> firewallName) (\s@AssociateFirewallPolicy' {} a -> s {firewallName = a} :: AssociateFirewallPolicy)

-- | The Amazon Resource Name (ARN) of the firewall policy.
associateFirewallPolicy_firewallPolicyArn :: Lens.Lens' AssociateFirewallPolicy Prelude.Text
associateFirewallPolicy_firewallPolicyArn = Lens.lens (\AssociateFirewallPolicy' {firewallPolicyArn} -> firewallPolicyArn) (\s@AssociateFirewallPolicy' {} a -> s {firewallPolicyArn = a} :: AssociateFirewallPolicy)

instance Core.AWSRequest AssociateFirewallPolicy where
  type
    AWSResponse AssociateFirewallPolicy =
      AssociateFirewallPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateFirewallPolicyResponse'
            Prelude.<$> (x Data..?> "UpdateToken")
            Prelude.<*> (x Data..?> "FirewallArn")
            Prelude.<*> (x Data..?> "FirewallName")
            Prelude.<*> (x Data..?> "FirewallPolicyArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateFirewallPolicy where
  hashWithSalt _salt AssociateFirewallPolicy' {..} =
    _salt `Prelude.hashWithSalt` updateToken
      `Prelude.hashWithSalt` firewallArn
      `Prelude.hashWithSalt` firewallName
      `Prelude.hashWithSalt` firewallPolicyArn

instance Prelude.NFData AssociateFirewallPolicy where
  rnf AssociateFirewallPolicy' {..} =
    Prelude.rnf updateToken
      `Prelude.seq` Prelude.rnf firewallArn
      `Prelude.seq` Prelude.rnf firewallName
      `Prelude.seq` Prelude.rnf firewallPolicyArn

instance Data.ToHeaders AssociateFirewallPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "NetworkFirewall_20201112.AssociateFirewallPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateFirewallPolicy where
  toJSON AssociateFirewallPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("UpdateToken" Data..=) Prelude.<$> updateToken,
            ("FirewallArn" Data..=) Prelude.<$> firewallArn,
            ("FirewallName" Data..=) Prelude.<$> firewallName,
            Prelude.Just
              ("FirewallPolicyArn" Data..= firewallPolicyArn)
          ]
      )

instance Data.ToPath AssociateFirewallPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateFirewallPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateFirewallPolicyResponse' smart constructor.
data AssociateFirewallPolicyResponse = AssociateFirewallPolicyResponse'
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
    -- | The descriptive name of the firewall. You can\'t change the name of a
    -- firewall after you create it.
    firewallName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the firewall policy.
    firewallPolicyArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateFirewallPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updateToken', 'associateFirewallPolicyResponse_updateToken' - An optional token that you can use for optimistic locking. Network
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
-- 'firewallArn', 'associateFirewallPolicyResponse_firewallArn' - The Amazon Resource Name (ARN) of the firewall.
--
-- 'firewallName', 'associateFirewallPolicyResponse_firewallName' - The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
--
-- 'firewallPolicyArn', 'associateFirewallPolicyResponse_firewallPolicyArn' - The Amazon Resource Name (ARN) of the firewall policy.
--
-- 'httpStatus', 'associateFirewallPolicyResponse_httpStatus' - The response's http status code.
newAssociateFirewallPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateFirewallPolicyResponse
newAssociateFirewallPolicyResponse pHttpStatus_ =
  AssociateFirewallPolicyResponse'
    { updateToken =
        Prelude.Nothing,
      firewallArn = Prelude.Nothing,
      firewallName = Prelude.Nothing,
      firewallPolicyArn = Prelude.Nothing,
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
associateFirewallPolicyResponse_updateToken :: Lens.Lens' AssociateFirewallPolicyResponse (Prelude.Maybe Prelude.Text)
associateFirewallPolicyResponse_updateToken = Lens.lens (\AssociateFirewallPolicyResponse' {updateToken} -> updateToken) (\s@AssociateFirewallPolicyResponse' {} a -> s {updateToken = a} :: AssociateFirewallPolicyResponse)

-- | The Amazon Resource Name (ARN) of the firewall.
associateFirewallPolicyResponse_firewallArn :: Lens.Lens' AssociateFirewallPolicyResponse (Prelude.Maybe Prelude.Text)
associateFirewallPolicyResponse_firewallArn = Lens.lens (\AssociateFirewallPolicyResponse' {firewallArn} -> firewallArn) (\s@AssociateFirewallPolicyResponse' {} a -> s {firewallArn = a} :: AssociateFirewallPolicyResponse)

-- | The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
associateFirewallPolicyResponse_firewallName :: Lens.Lens' AssociateFirewallPolicyResponse (Prelude.Maybe Prelude.Text)
associateFirewallPolicyResponse_firewallName = Lens.lens (\AssociateFirewallPolicyResponse' {firewallName} -> firewallName) (\s@AssociateFirewallPolicyResponse' {} a -> s {firewallName = a} :: AssociateFirewallPolicyResponse)

-- | The Amazon Resource Name (ARN) of the firewall policy.
associateFirewallPolicyResponse_firewallPolicyArn :: Lens.Lens' AssociateFirewallPolicyResponse (Prelude.Maybe Prelude.Text)
associateFirewallPolicyResponse_firewallPolicyArn = Lens.lens (\AssociateFirewallPolicyResponse' {firewallPolicyArn} -> firewallPolicyArn) (\s@AssociateFirewallPolicyResponse' {} a -> s {firewallPolicyArn = a} :: AssociateFirewallPolicyResponse)

-- | The response's http status code.
associateFirewallPolicyResponse_httpStatus :: Lens.Lens' AssociateFirewallPolicyResponse Prelude.Int
associateFirewallPolicyResponse_httpStatus = Lens.lens (\AssociateFirewallPolicyResponse' {httpStatus} -> httpStatus) (\s@AssociateFirewallPolicyResponse' {} a -> s {httpStatus = a} :: AssociateFirewallPolicyResponse)

instance
  Prelude.NFData
    AssociateFirewallPolicyResponse
  where
  rnf AssociateFirewallPolicyResponse' {..} =
    Prelude.rnf updateToken
      `Prelude.seq` Prelude.rnf firewallArn
      `Prelude.seq` Prelude.rnf firewallName
      `Prelude.seq` Prelude.rnf firewallPolicyArn
      `Prelude.seq` Prelude.rnf httpStatus
