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
-- Module      : Amazonka.NetworkFirewall.UpdateFirewallPolicyChangeProtection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the flag, @ChangeProtection@, which indicates whether it is
-- possible to change the firewall. If the flag is set to @TRUE@, the
-- firewall is protected from changes. This setting helps protect against
-- accidentally changing a firewall that\'s in use.
module Amazonka.NetworkFirewall.UpdateFirewallPolicyChangeProtection
  ( -- * Creating a Request
    UpdateFirewallPolicyChangeProtection (..),
    newUpdateFirewallPolicyChangeProtection,

    -- * Request Lenses
    updateFirewallPolicyChangeProtection_firewallArn,
    updateFirewallPolicyChangeProtection_firewallName,
    updateFirewallPolicyChangeProtection_updateToken,
    updateFirewallPolicyChangeProtection_firewallPolicyChangeProtection,

    -- * Destructuring the Response
    UpdateFirewallPolicyChangeProtectionResponse (..),
    newUpdateFirewallPolicyChangeProtectionResponse,

    -- * Response Lenses
    updateFirewallPolicyChangeProtectionResponse_firewallArn,
    updateFirewallPolicyChangeProtectionResponse_firewallName,
    updateFirewallPolicyChangeProtectionResponse_firewallPolicyChangeProtection,
    updateFirewallPolicyChangeProtectionResponse_updateToken,
    updateFirewallPolicyChangeProtectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFirewallPolicyChangeProtection' smart constructor.
data UpdateFirewallPolicyChangeProtection = UpdateFirewallPolicyChangeProtection'
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
-- 'firewallArn', 'updateFirewallPolicyChangeProtection_firewallArn' - The Amazon Resource Name (ARN) of the firewall.
--
-- You must specify the ARN or the name, and you can specify both.
--
-- 'firewallName', 'updateFirewallPolicyChangeProtection_firewallName' - The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
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
      { firewallArn =
          Prelude.Nothing,
        firewallName = Prelude.Nothing,
        updateToken = Prelude.Nothing,
        firewallPolicyChangeProtection =
          pFirewallPolicyChangeProtection_
      }

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFirewallPolicyChangeProtectionResponse'
            Prelude.<$> (x Data..?> "FirewallArn")
            Prelude.<*> (x Data..?> "FirewallName")
            Prelude.<*> (x Data..?> "FirewallPolicyChangeProtection")
            Prelude.<*> (x Data..?> "UpdateToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateFirewallPolicyChangeProtection
  where
  hashWithSalt
    _salt
    UpdateFirewallPolicyChangeProtection' {..} =
      _salt
        `Prelude.hashWithSalt` firewallArn
        `Prelude.hashWithSalt` firewallName
        `Prelude.hashWithSalt` updateToken
        `Prelude.hashWithSalt` firewallPolicyChangeProtection

instance
  Prelude.NFData
    UpdateFirewallPolicyChangeProtection
  where
  rnf UpdateFirewallPolicyChangeProtection' {..} =
    Prelude.rnf firewallArn
      `Prelude.seq` Prelude.rnf firewallName
      `Prelude.seq` Prelude.rnf updateToken
      `Prelude.seq` Prelude.rnf firewallPolicyChangeProtection

instance
  Data.ToHeaders
    UpdateFirewallPolicyChangeProtection
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "NetworkFirewall_20201112.UpdateFirewallPolicyChangeProtection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    UpdateFirewallPolicyChangeProtection
  where
  toJSON UpdateFirewallPolicyChangeProtection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FirewallArn" Data..=) Prelude.<$> firewallArn,
            ("FirewallName" Data..=) Prelude.<$> firewallName,
            ("UpdateToken" Data..=) Prelude.<$> updateToken,
            Prelude.Just
              ( "FirewallPolicyChangeProtection"
                  Data..= firewallPolicyChangeProtection
              )
          ]
      )

instance
  Data.ToPath
    UpdateFirewallPolicyChangeProtection
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    UpdateFirewallPolicyChangeProtection
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFirewallPolicyChangeProtectionResponse' smart constructor.
data UpdateFirewallPolicyChangeProtectionResponse = UpdateFirewallPolicyChangeProtectionResponse'
  { -- | The Amazon Resource Name (ARN) of the firewall.
    firewallArn :: Prelude.Maybe Prelude.Text,
    -- | The descriptive name of the firewall. You can\'t change the name of a
    -- firewall after you create it.
    firewallName :: Prelude.Maybe Prelude.Text,
    -- | A setting indicating whether the firewall is protected against a change
    -- to the firewall policy association. Use this setting to protect against
    -- accidentally modifying the firewall policy for a firewall that is in
    -- use. When you create a firewall, the operation initializes this setting
    -- to @TRUE@.
    firewallPolicyChangeProtection :: Prelude.Maybe Prelude.Bool,
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
-- Create a value of 'UpdateFirewallPolicyChangeProtectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallArn', 'updateFirewallPolicyChangeProtectionResponse_firewallArn' - The Amazon Resource Name (ARN) of the firewall.
--
-- 'firewallName', 'updateFirewallPolicyChangeProtectionResponse_firewallName' - The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
--
-- 'firewallPolicyChangeProtection', 'updateFirewallPolicyChangeProtectionResponse_firewallPolicyChangeProtection' - A setting indicating whether the firewall is protected against a change
-- to the firewall policy association. Use this setting to protect against
-- accidentally modifying the firewall policy for a firewall that is in
-- use. When you create a firewall, the operation initializes this setting
-- to @TRUE@.
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
-- 'httpStatus', 'updateFirewallPolicyChangeProtectionResponse_httpStatus' - The response's http status code.
newUpdateFirewallPolicyChangeProtectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFirewallPolicyChangeProtectionResponse
newUpdateFirewallPolicyChangeProtectionResponse
  pHttpStatus_ =
    UpdateFirewallPolicyChangeProtectionResponse'
      { firewallArn =
          Prelude.Nothing,
        firewallName =
          Prelude.Nothing,
        firewallPolicyChangeProtection =
          Prelude.Nothing,
        updateToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Amazon Resource Name (ARN) of the firewall.
updateFirewallPolicyChangeProtectionResponse_firewallArn :: Lens.Lens' UpdateFirewallPolicyChangeProtectionResponse (Prelude.Maybe Prelude.Text)
updateFirewallPolicyChangeProtectionResponse_firewallArn = Lens.lens (\UpdateFirewallPolicyChangeProtectionResponse' {firewallArn} -> firewallArn) (\s@UpdateFirewallPolicyChangeProtectionResponse' {} a -> s {firewallArn = a} :: UpdateFirewallPolicyChangeProtectionResponse)

-- | The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
updateFirewallPolicyChangeProtectionResponse_firewallName :: Lens.Lens' UpdateFirewallPolicyChangeProtectionResponse (Prelude.Maybe Prelude.Text)
updateFirewallPolicyChangeProtectionResponse_firewallName = Lens.lens (\UpdateFirewallPolicyChangeProtectionResponse' {firewallName} -> firewallName) (\s@UpdateFirewallPolicyChangeProtectionResponse' {} a -> s {firewallName = a} :: UpdateFirewallPolicyChangeProtectionResponse)

-- | A setting indicating whether the firewall is protected against a change
-- to the firewall policy association. Use this setting to protect against
-- accidentally modifying the firewall policy for a firewall that is in
-- use. When you create a firewall, the operation initializes this setting
-- to @TRUE@.
updateFirewallPolicyChangeProtectionResponse_firewallPolicyChangeProtection :: Lens.Lens' UpdateFirewallPolicyChangeProtectionResponse (Prelude.Maybe Prelude.Bool)
updateFirewallPolicyChangeProtectionResponse_firewallPolicyChangeProtection = Lens.lens (\UpdateFirewallPolicyChangeProtectionResponse' {firewallPolicyChangeProtection} -> firewallPolicyChangeProtection) (\s@UpdateFirewallPolicyChangeProtectionResponse' {} a -> s {firewallPolicyChangeProtection = a} :: UpdateFirewallPolicyChangeProtectionResponse)

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

-- | The response's http status code.
updateFirewallPolicyChangeProtectionResponse_httpStatus :: Lens.Lens' UpdateFirewallPolicyChangeProtectionResponse Prelude.Int
updateFirewallPolicyChangeProtectionResponse_httpStatus = Lens.lens (\UpdateFirewallPolicyChangeProtectionResponse' {httpStatus} -> httpStatus) (\s@UpdateFirewallPolicyChangeProtectionResponse' {} a -> s {httpStatus = a} :: UpdateFirewallPolicyChangeProtectionResponse)

instance
  Prelude.NFData
    UpdateFirewallPolicyChangeProtectionResponse
  where
  rnf UpdateFirewallPolicyChangeProtectionResponse' {..} =
    Prelude.rnf firewallArn
      `Prelude.seq` Prelude.rnf firewallName
      `Prelude.seq` Prelude.rnf firewallPolicyChangeProtection
      `Prelude.seq` Prelude.rnf updateToken
      `Prelude.seq` Prelude.rnf httpStatus
