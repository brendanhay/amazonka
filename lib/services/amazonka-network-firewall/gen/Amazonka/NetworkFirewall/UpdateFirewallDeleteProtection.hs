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
-- Module      : Amazonka.NetworkFirewall.UpdateFirewallDeleteProtection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the flag, @DeleteProtection@, which indicates whether it is
-- possible to delete the firewall. If the flag is set to @TRUE@, the
-- firewall is protected against deletion. This setting helps protect
-- against accidentally deleting a firewall that\'s in use.
module Amazonka.NetworkFirewall.UpdateFirewallDeleteProtection
  ( -- * Creating a Request
    UpdateFirewallDeleteProtection (..),
    newUpdateFirewallDeleteProtection,

    -- * Request Lenses
    updateFirewallDeleteProtection_firewallArn,
    updateFirewallDeleteProtection_firewallName,
    updateFirewallDeleteProtection_updateToken,
    updateFirewallDeleteProtection_deleteProtection,

    -- * Destructuring the Response
    UpdateFirewallDeleteProtectionResponse (..),
    newUpdateFirewallDeleteProtectionResponse,

    -- * Response Lenses
    updateFirewallDeleteProtectionResponse_deleteProtection,
    updateFirewallDeleteProtectionResponse_firewallArn,
    updateFirewallDeleteProtectionResponse_firewallName,
    updateFirewallDeleteProtectionResponse_updateToken,
    updateFirewallDeleteProtectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFirewallDeleteProtection' smart constructor.
data UpdateFirewallDeleteProtection = UpdateFirewallDeleteProtection'
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
    -- | A flag indicating whether it is possible to delete the firewall. A
    -- setting of @TRUE@ indicates that the firewall is protected against
    -- deletion. Use this setting to protect against accidentally deleting a
    -- firewall that is in use. When you create a firewall, the operation
    -- initializes this flag to @TRUE@.
    deleteProtection :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFirewallDeleteProtection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallArn', 'updateFirewallDeleteProtection_firewallArn' - The Amazon Resource Name (ARN) of the firewall.
--
-- You must specify the ARN or the name, and you can specify both.
--
-- 'firewallName', 'updateFirewallDeleteProtection_firewallName' - The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
--
-- 'updateToken', 'updateFirewallDeleteProtection_updateToken' - An optional token that you can use for optimistic locking. Network
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
-- 'deleteProtection', 'updateFirewallDeleteProtection_deleteProtection' - A flag indicating whether it is possible to delete the firewall. A
-- setting of @TRUE@ indicates that the firewall is protected against
-- deletion. Use this setting to protect against accidentally deleting a
-- firewall that is in use. When you create a firewall, the operation
-- initializes this flag to @TRUE@.
newUpdateFirewallDeleteProtection ::
  -- | 'deleteProtection'
  Prelude.Bool ->
  UpdateFirewallDeleteProtection
newUpdateFirewallDeleteProtection pDeleteProtection_ =
  UpdateFirewallDeleteProtection'
    { firewallArn =
        Prelude.Nothing,
      firewallName = Prelude.Nothing,
      updateToken = Prelude.Nothing,
      deleteProtection = pDeleteProtection_
    }

-- | The Amazon Resource Name (ARN) of the firewall.
--
-- You must specify the ARN or the name, and you can specify both.
updateFirewallDeleteProtection_firewallArn :: Lens.Lens' UpdateFirewallDeleteProtection (Prelude.Maybe Prelude.Text)
updateFirewallDeleteProtection_firewallArn = Lens.lens (\UpdateFirewallDeleteProtection' {firewallArn} -> firewallArn) (\s@UpdateFirewallDeleteProtection' {} a -> s {firewallArn = a} :: UpdateFirewallDeleteProtection)

-- | The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
updateFirewallDeleteProtection_firewallName :: Lens.Lens' UpdateFirewallDeleteProtection (Prelude.Maybe Prelude.Text)
updateFirewallDeleteProtection_firewallName = Lens.lens (\UpdateFirewallDeleteProtection' {firewallName} -> firewallName) (\s@UpdateFirewallDeleteProtection' {} a -> s {firewallName = a} :: UpdateFirewallDeleteProtection)

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
updateFirewallDeleteProtection_updateToken :: Lens.Lens' UpdateFirewallDeleteProtection (Prelude.Maybe Prelude.Text)
updateFirewallDeleteProtection_updateToken = Lens.lens (\UpdateFirewallDeleteProtection' {updateToken} -> updateToken) (\s@UpdateFirewallDeleteProtection' {} a -> s {updateToken = a} :: UpdateFirewallDeleteProtection)

-- | A flag indicating whether it is possible to delete the firewall. A
-- setting of @TRUE@ indicates that the firewall is protected against
-- deletion. Use this setting to protect against accidentally deleting a
-- firewall that is in use. When you create a firewall, the operation
-- initializes this flag to @TRUE@.
updateFirewallDeleteProtection_deleteProtection :: Lens.Lens' UpdateFirewallDeleteProtection Prelude.Bool
updateFirewallDeleteProtection_deleteProtection = Lens.lens (\UpdateFirewallDeleteProtection' {deleteProtection} -> deleteProtection) (\s@UpdateFirewallDeleteProtection' {} a -> s {deleteProtection = a} :: UpdateFirewallDeleteProtection)

instance
  Core.AWSRequest
    UpdateFirewallDeleteProtection
  where
  type
    AWSResponse UpdateFirewallDeleteProtection =
      UpdateFirewallDeleteProtectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFirewallDeleteProtectionResponse'
            Prelude.<$> (x Data..?> "DeleteProtection")
            Prelude.<*> (x Data..?> "FirewallArn")
            Prelude.<*> (x Data..?> "FirewallName")
            Prelude.<*> (x Data..?> "UpdateToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateFirewallDeleteProtection
  where
  hashWithSalt
    _salt
    UpdateFirewallDeleteProtection' {..} =
      _salt `Prelude.hashWithSalt` firewallArn
        `Prelude.hashWithSalt` firewallName
        `Prelude.hashWithSalt` updateToken
        `Prelude.hashWithSalt` deleteProtection

instance
  Prelude.NFData
    UpdateFirewallDeleteProtection
  where
  rnf UpdateFirewallDeleteProtection' {..} =
    Prelude.rnf firewallArn
      `Prelude.seq` Prelude.rnf firewallName
      `Prelude.seq` Prelude.rnf updateToken
      `Prelude.seq` Prelude.rnf deleteProtection

instance
  Data.ToHeaders
    UpdateFirewallDeleteProtection
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "NetworkFirewall_20201112.UpdateFirewallDeleteProtection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateFirewallDeleteProtection where
  toJSON UpdateFirewallDeleteProtection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FirewallArn" Data..=) Prelude.<$> firewallArn,
            ("FirewallName" Data..=) Prelude.<$> firewallName,
            ("UpdateToken" Data..=) Prelude.<$> updateToken,
            Prelude.Just
              ("DeleteProtection" Data..= deleteProtection)
          ]
      )

instance Data.ToPath UpdateFirewallDeleteProtection where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateFirewallDeleteProtection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFirewallDeleteProtectionResponse' smart constructor.
data UpdateFirewallDeleteProtectionResponse = UpdateFirewallDeleteProtectionResponse'
  { -- | A flag indicating whether it is possible to delete the firewall. A
    -- setting of @TRUE@ indicates that the firewall is protected against
    -- deletion. Use this setting to protect against accidentally deleting a
    -- firewall that is in use. When you create a firewall, the operation
    -- initializes this flag to @TRUE@.
    deleteProtection :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the firewall.
    firewallArn :: Prelude.Maybe Prelude.Text,
    -- | The descriptive name of the firewall. You can\'t change the name of a
    -- firewall after you create it.
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
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFirewallDeleteProtectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteProtection', 'updateFirewallDeleteProtectionResponse_deleteProtection' - A flag indicating whether it is possible to delete the firewall. A
-- setting of @TRUE@ indicates that the firewall is protected against
-- deletion. Use this setting to protect against accidentally deleting a
-- firewall that is in use. When you create a firewall, the operation
-- initializes this flag to @TRUE@.
--
-- 'firewallArn', 'updateFirewallDeleteProtectionResponse_firewallArn' - The Amazon Resource Name (ARN) of the firewall.
--
-- 'firewallName', 'updateFirewallDeleteProtectionResponse_firewallName' - The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
--
-- 'updateToken', 'updateFirewallDeleteProtectionResponse_updateToken' - An optional token that you can use for optimistic locking. Network
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
-- 'httpStatus', 'updateFirewallDeleteProtectionResponse_httpStatus' - The response's http status code.
newUpdateFirewallDeleteProtectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFirewallDeleteProtectionResponse
newUpdateFirewallDeleteProtectionResponse
  pHttpStatus_ =
    UpdateFirewallDeleteProtectionResponse'
      { deleteProtection =
          Prelude.Nothing,
        firewallArn = Prelude.Nothing,
        firewallName = Prelude.Nothing,
        updateToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A flag indicating whether it is possible to delete the firewall. A
-- setting of @TRUE@ indicates that the firewall is protected against
-- deletion. Use this setting to protect against accidentally deleting a
-- firewall that is in use. When you create a firewall, the operation
-- initializes this flag to @TRUE@.
updateFirewallDeleteProtectionResponse_deleteProtection :: Lens.Lens' UpdateFirewallDeleteProtectionResponse (Prelude.Maybe Prelude.Bool)
updateFirewallDeleteProtectionResponse_deleteProtection = Lens.lens (\UpdateFirewallDeleteProtectionResponse' {deleteProtection} -> deleteProtection) (\s@UpdateFirewallDeleteProtectionResponse' {} a -> s {deleteProtection = a} :: UpdateFirewallDeleteProtectionResponse)

-- | The Amazon Resource Name (ARN) of the firewall.
updateFirewallDeleteProtectionResponse_firewallArn :: Lens.Lens' UpdateFirewallDeleteProtectionResponse (Prelude.Maybe Prelude.Text)
updateFirewallDeleteProtectionResponse_firewallArn = Lens.lens (\UpdateFirewallDeleteProtectionResponse' {firewallArn} -> firewallArn) (\s@UpdateFirewallDeleteProtectionResponse' {} a -> s {firewallArn = a} :: UpdateFirewallDeleteProtectionResponse)

-- | The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
updateFirewallDeleteProtectionResponse_firewallName :: Lens.Lens' UpdateFirewallDeleteProtectionResponse (Prelude.Maybe Prelude.Text)
updateFirewallDeleteProtectionResponse_firewallName = Lens.lens (\UpdateFirewallDeleteProtectionResponse' {firewallName} -> firewallName) (\s@UpdateFirewallDeleteProtectionResponse' {} a -> s {firewallName = a} :: UpdateFirewallDeleteProtectionResponse)

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
updateFirewallDeleteProtectionResponse_updateToken :: Lens.Lens' UpdateFirewallDeleteProtectionResponse (Prelude.Maybe Prelude.Text)
updateFirewallDeleteProtectionResponse_updateToken = Lens.lens (\UpdateFirewallDeleteProtectionResponse' {updateToken} -> updateToken) (\s@UpdateFirewallDeleteProtectionResponse' {} a -> s {updateToken = a} :: UpdateFirewallDeleteProtectionResponse)

-- | The response's http status code.
updateFirewallDeleteProtectionResponse_httpStatus :: Lens.Lens' UpdateFirewallDeleteProtectionResponse Prelude.Int
updateFirewallDeleteProtectionResponse_httpStatus = Lens.lens (\UpdateFirewallDeleteProtectionResponse' {httpStatus} -> httpStatus) (\s@UpdateFirewallDeleteProtectionResponse' {} a -> s {httpStatus = a} :: UpdateFirewallDeleteProtectionResponse)

instance
  Prelude.NFData
    UpdateFirewallDeleteProtectionResponse
  where
  rnf UpdateFirewallDeleteProtectionResponse' {..} =
    Prelude.rnf deleteProtection
      `Prelude.seq` Prelude.rnf firewallArn
      `Prelude.seq` Prelude.rnf firewallName
      `Prelude.seq` Prelude.rnf updateToken
      `Prelude.seq` Prelude.rnf httpStatus
