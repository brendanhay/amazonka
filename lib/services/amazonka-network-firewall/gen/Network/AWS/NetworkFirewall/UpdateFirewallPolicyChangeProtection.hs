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
-- Module      : Network.AWS.NetworkFirewall.UpdateFirewallPolicyChangeProtection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.NetworkFirewall.UpdateFirewallPolicyChangeProtection
  ( -- * Creating a Request
    UpdateFirewallPolicyChangeProtection (..),
    newUpdateFirewallPolicyChangeProtection,

    -- * Request Lenses
    updateFirewallPolicyChangeProtection_updateToken,
    updateFirewallPolicyChangeProtection_firewallArn,
    updateFirewallPolicyChangeProtection_firewallName,
    updateFirewallPolicyChangeProtection_firewallPolicyChangeProtection,

    -- * Destructuring the Response
    UpdateFirewallPolicyChangeProtectionResponse (..),
    newUpdateFirewallPolicyChangeProtectionResponse,

    -- * Response Lenses
    updateFirewallPolicyChangeProtectionResponse_updateToken,
    updateFirewallPolicyChangeProtectionResponse_firewallArn,
    updateFirewallPolicyChangeProtectionResponse_firewallPolicyChangeProtection,
    updateFirewallPolicyChangeProtectionResponse_firewallName,
    updateFirewallPolicyChangeProtectionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.NetworkFirewall.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateFirewallPolicyChangeProtection' smart constructor.
data UpdateFirewallPolicyChangeProtection = UpdateFirewallPolicyChangeProtection'
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
    -- | A setting indicating whether the firewall is protected against a change
    -- to the firewall policy association. Use this setting to protect against
    -- accidentally modifying the firewall policy for a firewall that is in
    -- use. When you create a firewall, the operation initializes this setting
    -- to @TRUE@.
    firewallPolicyChangeProtection :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFirewallPolicyChangeProtection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updateToken', 'updateFirewallPolicyChangeProtection_updateToken' - An optional token that you can use for optimistic locking. Network
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
-- 'firewallArn', 'updateFirewallPolicyChangeProtection_firewallArn' - The Amazon Resource Name (ARN) of the firewall.
--
-- You must specify the ARN or the name, and you can specify both.
--
-- 'firewallName', 'updateFirewallPolicyChangeProtection_firewallName' - The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
--
-- 'firewallPolicyChangeProtection', 'updateFirewallPolicyChangeProtection_firewallPolicyChangeProtection' - A setting indicating whether the firewall is protected against a change
-- to the firewall policy association. Use this setting to protect against
-- accidentally modifying the firewall policy for a firewall that is in
-- use. When you create a firewall, the operation initializes this setting
-- to @TRUE@.
newUpdateFirewallPolicyChangeProtection ::
  -- | 'firewallPolicyChangeProtection'
  Prelude.Bool ->
  UpdateFirewallPolicyChangeProtection
newUpdateFirewallPolicyChangeProtection
  pFirewallPolicyChangeProtection_ =
    UpdateFirewallPolicyChangeProtection'
      { updateToken =
          Prelude.Nothing,
        firewallArn = Prelude.Nothing,
        firewallName = Prelude.Nothing,
        firewallPolicyChangeProtection =
          pFirewallPolicyChangeProtection_
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
updateFirewallPolicyChangeProtection_updateToken :: Lens.Lens' UpdateFirewallPolicyChangeProtection (Prelude.Maybe Prelude.Text)
updateFirewallPolicyChangeProtection_updateToken = Lens.lens (\UpdateFirewallPolicyChangeProtection' {updateToken} -> updateToken) (\s@UpdateFirewallPolicyChangeProtection' {} a -> s {updateToken = a} :: UpdateFirewallPolicyChangeProtection)

-- | The Amazon Resource Name (ARN) of the firewall.
--
-- You must specify the ARN or the name, and you can specify both.
updateFirewallPolicyChangeProtection_firewallArn :: Lens.Lens' UpdateFirewallPolicyChangeProtection (Prelude.Maybe Prelude.Text)
updateFirewallPolicyChangeProtection_firewallArn = Lens.lens (\UpdateFirewallPolicyChangeProtection' {firewallArn} -> firewallArn) (\s@UpdateFirewallPolicyChangeProtection' {} a -> s {firewallArn = a} :: UpdateFirewallPolicyChangeProtection)

-- | The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
updateFirewallPolicyChangeProtection_firewallName :: Lens.Lens' UpdateFirewallPolicyChangeProtection (Prelude.Maybe Prelude.Text)
updateFirewallPolicyChangeProtection_firewallName = Lens.lens (\UpdateFirewallPolicyChangeProtection' {firewallName} -> firewallName) (\s@UpdateFirewallPolicyChangeProtection' {} a -> s {firewallName = a} :: UpdateFirewallPolicyChangeProtection)

-- | A setting indicating whether the firewall is protected against a change
-- to the firewall policy association. Use this setting to protect against
-- accidentally modifying the firewall policy for a firewall that is in
-- use. When you create a firewall, the operation initializes this setting
-- to @TRUE@.
updateFirewallPolicyChangeProtection_firewallPolicyChangeProtection :: Lens.Lens' UpdateFirewallPolicyChangeProtection Prelude.Bool
updateFirewallPolicyChangeProtection_firewallPolicyChangeProtection = Lens.lens (\UpdateFirewallPolicyChangeProtection' {firewallPolicyChangeProtection} -> firewallPolicyChangeProtection) (\s@UpdateFirewallPolicyChangeProtection' {} a -> s {firewallPolicyChangeProtection = a} :: UpdateFirewallPolicyChangeProtection)

instance
  Core.AWSRequest
    UpdateFirewallPolicyChangeProtection
  where
  type
    AWSResponse UpdateFirewallPolicyChangeProtection =
      UpdateFirewallPolicyChangeProtectionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFirewallPolicyChangeProtectionResponse'
            Prelude.<$> (x Core..?> "UpdateToken")
              Prelude.<*> (x Core..?> "FirewallArn")
              Prelude.<*> (x Core..?> "FirewallPolicyChangeProtection")
              Prelude.<*> (x Core..?> "FirewallName")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateFirewallPolicyChangeProtection

instance
  Prelude.NFData
    UpdateFirewallPolicyChangeProtection

instance
  Core.ToHeaders
    UpdateFirewallPolicyChangeProtection
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "NetworkFirewall_20201112.UpdateFirewallPolicyChangeProtection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    UpdateFirewallPolicyChangeProtection
  where
  toJSON UpdateFirewallPolicyChangeProtection' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("UpdateToken" Core..=) Prelude.<$> updateToken,
            ("FirewallArn" Core..=) Prelude.<$> firewallArn,
            ("FirewallName" Core..=) Prelude.<$> firewallName,
            Prelude.Just
              ( "FirewallPolicyChangeProtection"
                  Core..= firewallPolicyChangeProtection
              )
          ]
      )

instance
  Core.ToPath
    UpdateFirewallPolicyChangeProtection
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    UpdateFirewallPolicyChangeProtection
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFirewallPolicyChangeProtectionResponse' smart constructor.
data UpdateFirewallPolicyChangeProtectionResponse = UpdateFirewallPolicyChangeProtectionResponse'
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
    -- | A setting indicating whether the firewall is protected against a change
    -- to the firewall policy association. Use this setting to protect against
    -- accidentally modifying the firewall policy for a firewall that is in
    -- use. When you create a firewall, the operation initializes this setting
    -- to @TRUE@.
    firewallPolicyChangeProtection :: Prelude.Maybe Prelude.Bool,
    -- | The descriptive name of the firewall. You can\'t change the name of a
    -- firewall after you create it.
    firewallName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFirewallPolicyChangeProtectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updateToken', 'updateFirewallPolicyChangeProtectionResponse_updateToken' - An optional token that you can use for optimistic locking. Network
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
-- 'firewallArn', 'updateFirewallPolicyChangeProtectionResponse_firewallArn' - The Amazon Resource Name (ARN) of the firewall.
--
-- 'firewallPolicyChangeProtection', 'updateFirewallPolicyChangeProtectionResponse_firewallPolicyChangeProtection' - A setting indicating whether the firewall is protected against a change
-- to the firewall policy association. Use this setting to protect against
-- accidentally modifying the firewall policy for a firewall that is in
-- use. When you create a firewall, the operation initializes this setting
-- to @TRUE@.
--
-- 'firewallName', 'updateFirewallPolicyChangeProtectionResponse_firewallName' - The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
--
-- 'httpStatus', 'updateFirewallPolicyChangeProtectionResponse_httpStatus' - The response's http status code.
newUpdateFirewallPolicyChangeProtectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFirewallPolicyChangeProtectionResponse
newUpdateFirewallPolicyChangeProtectionResponse
  pHttpStatus_ =
    UpdateFirewallPolicyChangeProtectionResponse'
      { updateToken =
          Prelude.Nothing,
        firewallArn = Prelude.Nothing,
        firewallPolicyChangeProtection =
          Prelude.Nothing,
        firewallName =
          Prelude.Nothing,
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
updateFirewallPolicyChangeProtectionResponse_updateToken :: Lens.Lens' UpdateFirewallPolicyChangeProtectionResponse (Prelude.Maybe Prelude.Text)
updateFirewallPolicyChangeProtectionResponse_updateToken = Lens.lens (\UpdateFirewallPolicyChangeProtectionResponse' {updateToken} -> updateToken) (\s@UpdateFirewallPolicyChangeProtectionResponse' {} a -> s {updateToken = a} :: UpdateFirewallPolicyChangeProtectionResponse)

-- | The Amazon Resource Name (ARN) of the firewall.
updateFirewallPolicyChangeProtectionResponse_firewallArn :: Lens.Lens' UpdateFirewallPolicyChangeProtectionResponse (Prelude.Maybe Prelude.Text)
updateFirewallPolicyChangeProtectionResponse_firewallArn = Lens.lens (\UpdateFirewallPolicyChangeProtectionResponse' {firewallArn} -> firewallArn) (\s@UpdateFirewallPolicyChangeProtectionResponse' {} a -> s {firewallArn = a} :: UpdateFirewallPolicyChangeProtectionResponse)

-- | A setting indicating whether the firewall is protected against a change
-- to the firewall policy association. Use this setting to protect against
-- accidentally modifying the firewall policy for a firewall that is in
-- use. When you create a firewall, the operation initializes this setting
-- to @TRUE@.
updateFirewallPolicyChangeProtectionResponse_firewallPolicyChangeProtection :: Lens.Lens' UpdateFirewallPolicyChangeProtectionResponse (Prelude.Maybe Prelude.Bool)
updateFirewallPolicyChangeProtectionResponse_firewallPolicyChangeProtection = Lens.lens (\UpdateFirewallPolicyChangeProtectionResponse' {firewallPolicyChangeProtection} -> firewallPolicyChangeProtection) (\s@UpdateFirewallPolicyChangeProtectionResponse' {} a -> s {firewallPolicyChangeProtection = a} :: UpdateFirewallPolicyChangeProtectionResponse)

-- | The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
updateFirewallPolicyChangeProtectionResponse_firewallName :: Lens.Lens' UpdateFirewallPolicyChangeProtectionResponse (Prelude.Maybe Prelude.Text)
updateFirewallPolicyChangeProtectionResponse_firewallName = Lens.lens (\UpdateFirewallPolicyChangeProtectionResponse' {firewallName} -> firewallName) (\s@UpdateFirewallPolicyChangeProtectionResponse' {} a -> s {firewallName = a} :: UpdateFirewallPolicyChangeProtectionResponse)

-- | The response's http status code.
updateFirewallPolicyChangeProtectionResponse_httpStatus :: Lens.Lens' UpdateFirewallPolicyChangeProtectionResponse Prelude.Int
updateFirewallPolicyChangeProtectionResponse_httpStatus = Lens.lens (\UpdateFirewallPolicyChangeProtectionResponse' {httpStatus} -> httpStatus) (\s@UpdateFirewallPolicyChangeProtectionResponse' {} a -> s {httpStatus = a} :: UpdateFirewallPolicyChangeProtectionResponse)

instance
  Prelude.NFData
    UpdateFirewallPolicyChangeProtectionResponse
