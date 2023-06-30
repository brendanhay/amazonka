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
-- Module      : Amazonka.NetworkFirewall.DeleteFirewallPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified FirewallPolicy.
module Amazonka.NetworkFirewall.DeleteFirewallPolicy
  ( -- * Creating a Request
    DeleteFirewallPolicy (..),
    newDeleteFirewallPolicy,

    -- * Request Lenses
    deleteFirewallPolicy_firewallPolicyArn,
    deleteFirewallPolicy_firewallPolicyName,

    -- * Destructuring the Response
    DeleteFirewallPolicyResponse (..),
    newDeleteFirewallPolicyResponse,

    -- * Response Lenses
    deleteFirewallPolicyResponse_httpStatus,
    deleteFirewallPolicyResponse_firewallPolicyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteFirewallPolicy' smart constructor.
data DeleteFirewallPolicy = DeleteFirewallPolicy'
  { -- | The Amazon Resource Name (ARN) of the firewall policy.
    --
    -- You must specify the ARN or the name, and you can specify both.
    firewallPolicyArn :: Prelude.Maybe Prelude.Text,
    -- | The descriptive name of the firewall policy. You can\'t change the name
    -- of a firewall policy after you create it.
    --
    -- You must specify the ARN or the name, and you can specify both.
    firewallPolicyName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFirewallPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallPolicyArn', 'deleteFirewallPolicy_firewallPolicyArn' - The Amazon Resource Name (ARN) of the firewall policy.
--
-- You must specify the ARN or the name, and you can specify both.
--
-- 'firewallPolicyName', 'deleteFirewallPolicy_firewallPolicyName' - The descriptive name of the firewall policy. You can\'t change the name
-- of a firewall policy after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
newDeleteFirewallPolicy ::
  DeleteFirewallPolicy
newDeleteFirewallPolicy =
  DeleteFirewallPolicy'
    { firewallPolicyArn =
        Prelude.Nothing,
      firewallPolicyName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the firewall policy.
--
-- You must specify the ARN or the name, and you can specify both.
deleteFirewallPolicy_firewallPolicyArn :: Lens.Lens' DeleteFirewallPolicy (Prelude.Maybe Prelude.Text)
deleteFirewallPolicy_firewallPolicyArn = Lens.lens (\DeleteFirewallPolicy' {firewallPolicyArn} -> firewallPolicyArn) (\s@DeleteFirewallPolicy' {} a -> s {firewallPolicyArn = a} :: DeleteFirewallPolicy)

-- | The descriptive name of the firewall policy. You can\'t change the name
-- of a firewall policy after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
deleteFirewallPolicy_firewallPolicyName :: Lens.Lens' DeleteFirewallPolicy (Prelude.Maybe Prelude.Text)
deleteFirewallPolicy_firewallPolicyName = Lens.lens (\DeleteFirewallPolicy' {firewallPolicyName} -> firewallPolicyName) (\s@DeleteFirewallPolicy' {} a -> s {firewallPolicyName = a} :: DeleteFirewallPolicy)

instance Core.AWSRequest DeleteFirewallPolicy where
  type
    AWSResponse DeleteFirewallPolicy =
      DeleteFirewallPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteFirewallPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "FirewallPolicyResponse")
      )

instance Prelude.Hashable DeleteFirewallPolicy where
  hashWithSalt _salt DeleteFirewallPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` firewallPolicyArn
      `Prelude.hashWithSalt` firewallPolicyName

instance Prelude.NFData DeleteFirewallPolicy where
  rnf DeleteFirewallPolicy' {..} =
    Prelude.rnf firewallPolicyArn
      `Prelude.seq` Prelude.rnf firewallPolicyName

instance Data.ToHeaders DeleteFirewallPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "NetworkFirewall_20201112.DeleteFirewallPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteFirewallPolicy where
  toJSON DeleteFirewallPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FirewallPolicyArn" Data..=)
              Prelude.<$> firewallPolicyArn,
            ("FirewallPolicyName" Data..=)
              Prelude.<$> firewallPolicyName
          ]
      )

instance Data.ToPath DeleteFirewallPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteFirewallPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFirewallPolicyResponse' smart constructor.
data DeleteFirewallPolicyResponse = DeleteFirewallPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The object containing the definition of the FirewallPolicyResponse that
    -- you asked to delete.
    firewallPolicyResponse :: FirewallPolicyResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFirewallPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteFirewallPolicyResponse_httpStatus' - The response's http status code.
--
-- 'firewallPolicyResponse', 'deleteFirewallPolicyResponse_firewallPolicyResponse' - The object containing the definition of the FirewallPolicyResponse that
-- you asked to delete.
newDeleteFirewallPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'firewallPolicyResponse'
  FirewallPolicyResponse ->
  DeleteFirewallPolicyResponse
newDeleteFirewallPolicyResponse
  pHttpStatus_
  pFirewallPolicyResponse_ =
    DeleteFirewallPolicyResponse'
      { httpStatus =
          pHttpStatus_,
        firewallPolicyResponse =
          pFirewallPolicyResponse_
      }

-- | The response's http status code.
deleteFirewallPolicyResponse_httpStatus :: Lens.Lens' DeleteFirewallPolicyResponse Prelude.Int
deleteFirewallPolicyResponse_httpStatus = Lens.lens (\DeleteFirewallPolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteFirewallPolicyResponse' {} a -> s {httpStatus = a} :: DeleteFirewallPolicyResponse)

-- | The object containing the definition of the FirewallPolicyResponse that
-- you asked to delete.
deleteFirewallPolicyResponse_firewallPolicyResponse :: Lens.Lens' DeleteFirewallPolicyResponse FirewallPolicyResponse
deleteFirewallPolicyResponse_firewallPolicyResponse = Lens.lens (\DeleteFirewallPolicyResponse' {firewallPolicyResponse} -> firewallPolicyResponse) (\s@DeleteFirewallPolicyResponse' {} a -> s {firewallPolicyResponse = a} :: DeleteFirewallPolicyResponse)

instance Prelude.NFData DeleteFirewallPolicyResponse where
  rnf DeleteFirewallPolicyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf firewallPolicyResponse
