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
-- Module      : Amazonka.NetworkFirewall.UpdateSubnetChangeProtection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.UpdateSubnetChangeProtection
  ( -- * Creating a Request
    UpdateSubnetChangeProtection (..),
    newUpdateSubnetChangeProtection,

    -- * Request Lenses
    updateSubnetChangeProtection_firewallArn,
    updateSubnetChangeProtection_firewallName,
    updateSubnetChangeProtection_updateToken,
    updateSubnetChangeProtection_subnetChangeProtection,

    -- * Destructuring the Response
    UpdateSubnetChangeProtectionResponse (..),
    newUpdateSubnetChangeProtectionResponse,

    -- * Response Lenses
    updateSubnetChangeProtectionResponse_firewallArn,
    updateSubnetChangeProtectionResponse_firewallName,
    updateSubnetChangeProtectionResponse_subnetChangeProtection,
    updateSubnetChangeProtectionResponse_updateToken,
    updateSubnetChangeProtectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSubnetChangeProtection' smart constructor.
data UpdateSubnetChangeProtection = UpdateSubnetChangeProtection'
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
    -- | A setting indicating whether the firewall is protected against changes
    -- to the subnet associations. Use this setting to protect against
    -- accidentally modifying the subnet associations for a firewall that is in
    -- use. When you create a firewall, the operation initializes this setting
    -- to @TRUE@.
    subnetChangeProtection :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSubnetChangeProtection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallArn', 'updateSubnetChangeProtection_firewallArn' - The Amazon Resource Name (ARN) of the firewall.
--
-- You must specify the ARN or the name, and you can specify both.
--
-- 'firewallName', 'updateSubnetChangeProtection_firewallName' - The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
--
-- 'updateToken', 'updateSubnetChangeProtection_updateToken' - An optional token that you can use for optimistic locking. Network
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
-- 'subnetChangeProtection', 'updateSubnetChangeProtection_subnetChangeProtection' - A setting indicating whether the firewall is protected against changes
-- to the subnet associations. Use this setting to protect against
-- accidentally modifying the subnet associations for a firewall that is in
-- use. When you create a firewall, the operation initializes this setting
-- to @TRUE@.
newUpdateSubnetChangeProtection ::
  -- | 'subnetChangeProtection'
  Prelude.Bool ->
  UpdateSubnetChangeProtection
newUpdateSubnetChangeProtection
  pSubnetChangeProtection_ =
    UpdateSubnetChangeProtection'
      { firewallArn =
          Prelude.Nothing,
        firewallName = Prelude.Nothing,
        updateToken = Prelude.Nothing,
        subnetChangeProtection =
          pSubnetChangeProtection_
      }

-- | The Amazon Resource Name (ARN) of the firewall.
--
-- You must specify the ARN or the name, and you can specify both.
updateSubnetChangeProtection_firewallArn :: Lens.Lens' UpdateSubnetChangeProtection (Prelude.Maybe Prelude.Text)
updateSubnetChangeProtection_firewallArn = Lens.lens (\UpdateSubnetChangeProtection' {firewallArn} -> firewallArn) (\s@UpdateSubnetChangeProtection' {} a -> s {firewallArn = a} :: UpdateSubnetChangeProtection)

-- | The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
updateSubnetChangeProtection_firewallName :: Lens.Lens' UpdateSubnetChangeProtection (Prelude.Maybe Prelude.Text)
updateSubnetChangeProtection_firewallName = Lens.lens (\UpdateSubnetChangeProtection' {firewallName} -> firewallName) (\s@UpdateSubnetChangeProtection' {} a -> s {firewallName = a} :: UpdateSubnetChangeProtection)

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
updateSubnetChangeProtection_updateToken :: Lens.Lens' UpdateSubnetChangeProtection (Prelude.Maybe Prelude.Text)
updateSubnetChangeProtection_updateToken = Lens.lens (\UpdateSubnetChangeProtection' {updateToken} -> updateToken) (\s@UpdateSubnetChangeProtection' {} a -> s {updateToken = a} :: UpdateSubnetChangeProtection)

-- | A setting indicating whether the firewall is protected against changes
-- to the subnet associations. Use this setting to protect against
-- accidentally modifying the subnet associations for a firewall that is in
-- use. When you create a firewall, the operation initializes this setting
-- to @TRUE@.
updateSubnetChangeProtection_subnetChangeProtection :: Lens.Lens' UpdateSubnetChangeProtection Prelude.Bool
updateSubnetChangeProtection_subnetChangeProtection = Lens.lens (\UpdateSubnetChangeProtection' {subnetChangeProtection} -> subnetChangeProtection) (\s@UpdateSubnetChangeProtection' {} a -> s {subnetChangeProtection = a} :: UpdateSubnetChangeProtection)

instance Core.AWSRequest UpdateSubnetChangeProtection where
  type
    AWSResponse UpdateSubnetChangeProtection =
      UpdateSubnetChangeProtectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSubnetChangeProtectionResponse'
            Prelude.<$> (x Data..?> "FirewallArn")
            Prelude.<*> (x Data..?> "FirewallName")
            Prelude.<*> (x Data..?> "SubnetChangeProtection")
            Prelude.<*> (x Data..?> "UpdateToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateSubnetChangeProtection
  where
  hashWithSalt _salt UpdateSubnetChangeProtection' {..} =
    _salt `Prelude.hashWithSalt` firewallArn
      `Prelude.hashWithSalt` firewallName
      `Prelude.hashWithSalt` updateToken
      `Prelude.hashWithSalt` subnetChangeProtection

instance Prelude.NFData UpdateSubnetChangeProtection where
  rnf UpdateSubnetChangeProtection' {..} =
    Prelude.rnf firewallArn
      `Prelude.seq` Prelude.rnf firewallName
      `Prelude.seq` Prelude.rnf updateToken
      `Prelude.seq` Prelude.rnf subnetChangeProtection

instance Data.ToHeaders UpdateSubnetChangeProtection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "NetworkFirewall_20201112.UpdateSubnetChangeProtection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateSubnetChangeProtection where
  toJSON UpdateSubnetChangeProtection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FirewallArn" Data..=) Prelude.<$> firewallArn,
            ("FirewallName" Data..=) Prelude.<$> firewallName,
            ("UpdateToken" Data..=) Prelude.<$> updateToken,
            Prelude.Just
              ( "SubnetChangeProtection"
                  Data..= subnetChangeProtection
              )
          ]
      )

instance Data.ToPath UpdateSubnetChangeProtection where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateSubnetChangeProtection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSubnetChangeProtectionResponse' smart constructor.
data UpdateSubnetChangeProtectionResponse = UpdateSubnetChangeProtectionResponse'
  { -- | The Amazon Resource Name (ARN) of the firewall.
    firewallArn :: Prelude.Maybe Prelude.Text,
    -- | The descriptive name of the firewall. You can\'t change the name of a
    -- firewall after you create it.
    firewallName :: Prelude.Maybe Prelude.Text,
    -- | A setting indicating whether the firewall is protected against changes
    -- to the subnet associations. Use this setting to protect against
    -- accidentally modifying the subnet associations for a firewall that is in
    -- use. When you create a firewall, the operation initializes this setting
    -- to @TRUE@.
    subnetChangeProtection :: Prelude.Maybe Prelude.Bool,
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
-- Create a value of 'UpdateSubnetChangeProtectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallArn', 'updateSubnetChangeProtectionResponse_firewallArn' - The Amazon Resource Name (ARN) of the firewall.
--
-- 'firewallName', 'updateSubnetChangeProtectionResponse_firewallName' - The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
--
-- 'subnetChangeProtection', 'updateSubnetChangeProtectionResponse_subnetChangeProtection' - A setting indicating whether the firewall is protected against changes
-- to the subnet associations. Use this setting to protect against
-- accidentally modifying the subnet associations for a firewall that is in
-- use. When you create a firewall, the operation initializes this setting
-- to @TRUE@.
--
-- 'updateToken', 'updateSubnetChangeProtectionResponse_updateToken' - An optional token that you can use for optimistic locking. Network
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
-- 'httpStatus', 'updateSubnetChangeProtectionResponse_httpStatus' - The response's http status code.
newUpdateSubnetChangeProtectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSubnetChangeProtectionResponse
newUpdateSubnetChangeProtectionResponse pHttpStatus_ =
  UpdateSubnetChangeProtectionResponse'
    { firewallArn =
        Prelude.Nothing,
      firewallName = Prelude.Nothing,
      subnetChangeProtection =
        Prelude.Nothing,
      updateToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the firewall.
updateSubnetChangeProtectionResponse_firewallArn :: Lens.Lens' UpdateSubnetChangeProtectionResponse (Prelude.Maybe Prelude.Text)
updateSubnetChangeProtectionResponse_firewallArn = Lens.lens (\UpdateSubnetChangeProtectionResponse' {firewallArn} -> firewallArn) (\s@UpdateSubnetChangeProtectionResponse' {} a -> s {firewallArn = a} :: UpdateSubnetChangeProtectionResponse)

-- | The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
updateSubnetChangeProtectionResponse_firewallName :: Lens.Lens' UpdateSubnetChangeProtectionResponse (Prelude.Maybe Prelude.Text)
updateSubnetChangeProtectionResponse_firewallName = Lens.lens (\UpdateSubnetChangeProtectionResponse' {firewallName} -> firewallName) (\s@UpdateSubnetChangeProtectionResponse' {} a -> s {firewallName = a} :: UpdateSubnetChangeProtectionResponse)

-- | A setting indicating whether the firewall is protected against changes
-- to the subnet associations. Use this setting to protect against
-- accidentally modifying the subnet associations for a firewall that is in
-- use. When you create a firewall, the operation initializes this setting
-- to @TRUE@.
updateSubnetChangeProtectionResponse_subnetChangeProtection :: Lens.Lens' UpdateSubnetChangeProtectionResponse (Prelude.Maybe Prelude.Bool)
updateSubnetChangeProtectionResponse_subnetChangeProtection = Lens.lens (\UpdateSubnetChangeProtectionResponse' {subnetChangeProtection} -> subnetChangeProtection) (\s@UpdateSubnetChangeProtectionResponse' {} a -> s {subnetChangeProtection = a} :: UpdateSubnetChangeProtectionResponse)

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
updateSubnetChangeProtectionResponse_updateToken :: Lens.Lens' UpdateSubnetChangeProtectionResponse (Prelude.Maybe Prelude.Text)
updateSubnetChangeProtectionResponse_updateToken = Lens.lens (\UpdateSubnetChangeProtectionResponse' {updateToken} -> updateToken) (\s@UpdateSubnetChangeProtectionResponse' {} a -> s {updateToken = a} :: UpdateSubnetChangeProtectionResponse)

-- | The response's http status code.
updateSubnetChangeProtectionResponse_httpStatus :: Lens.Lens' UpdateSubnetChangeProtectionResponse Prelude.Int
updateSubnetChangeProtectionResponse_httpStatus = Lens.lens (\UpdateSubnetChangeProtectionResponse' {httpStatus} -> httpStatus) (\s@UpdateSubnetChangeProtectionResponse' {} a -> s {httpStatus = a} :: UpdateSubnetChangeProtectionResponse)

instance
  Prelude.NFData
    UpdateSubnetChangeProtectionResponse
  where
  rnf UpdateSubnetChangeProtectionResponse' {..} =
    Prelude.rnf firewallArn
      `Prelude.seq` Prelude.rnf firewallName
      `Prelude.seq` Prelude.rnf subnetChangeProtection
      `Prelude.seq` Prelude.rnf updateToken
      `Prelude.seq` Prelude.rnf httpStatus
