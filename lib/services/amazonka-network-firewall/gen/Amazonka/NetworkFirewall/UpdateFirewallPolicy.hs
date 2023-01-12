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
-- Module      : Amazonka.NetworkFirewall.UpdateFirewallPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the properties of the specified firewall policy.
module Amazonka.NetworkFirewall.UpdateFirewallPolicy
  ( -- * Creating a Request
    UpdateFirewallPolicy (..),
    newUpdateFirewallPolicy,

    -- * Request Lenses
    updateFirewallPolicy_description,
    updateFirewallPolicy_dryRun,
    updateFirewallPolicy_encryptionConfiguration,
    updateFirewallPolicy_firewallPolicyArn,
    updateFirewallPolicy_firewallPolicyName,
    updateFirewallPolicy_updateToken,
    updateFirewallPolicy_firewallPolicy,

    -- * Destructuring the Response
    UpdateFirewallPolicyResponse (..),
    newUpdateFirewallPolicyResponse,

    -- * Response Lenses
    updateFirewallPolicyResponse_httpStatus,
    updateFirewallPolicyResponse_updateToken,
    updateFirewallPolicyResponse_firewallPolicyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFirewallPolicy' smart constructor.
data UpdateFirewallPolicy = UpdateFirewallPolicy'
  { -- | A description of the firewall policy.
    description :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether you want Network Firewall to just check the validity
    -- of the request, rather than run the request.
    --
    -- If set to @TRUE@, Network Firewall checks whether the request can run
    -- successfully, but doesn\'t actually make the requested changes. The call
    -- returns the value that the request would return if you ran it with dry
    -- run set to @FALSE@, but doesn\'t make additions or changes to your
    -- resources. This option allows you to make sure that you have the
    -- required permissions to run the request and that your request parameters
    -- are valid.
    --
    -- If set to @FALSE@, Network Firewall makes the requested changes to your
    -- resources.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | A complex type that contains settings for encryption of your firewall
    -- policy resources.
    encryptionConfiguration :: Prelude.Maybe EncryptionConfiguration,
    -- | The Amazon Resource Name (ARN) of the firewall policy.
    --
    -- You must specify the ARN or the name, and you can specify both.
    firewallPolicyArn :: Prelude.Maybe Prelude.Text,
    -- | The descriptive name of the firewall policy. You can\'t change the name
    -- of a firewall policy after you create it.
    --
    -- You must specify the ARN or the name, and you can specify both.
    firewallPolicyName :: Prelude.Maybe Prelude.Text,
    -- | A token used for optimistic locking. Network Firewall returns a token to
    -- your requests that access the firewall policy. The token marks the state
    -- of the policy resource at the time of the request.
    --
    -- To make changes to the policy, you provide the token in your request.
    -- Network Firewall uses the token to ensure that the policy hasn\'t
    -- changed since you last retrieved it. If it has changed, the operation
    -- fails with an @InvalidTokenException@. If this happens, retrieve the
    -- firewall policy again to get a current copy of it with current token.
    -- Reapply your changes as needed, then try the operation again using the
    -- new token.
    updateToken :: Prelude.Text,
    -- | The updated firewall policy to use for the firewall.
    firewallPolicy :: FirewallPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFirewallPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateFirewallPolicy_description' - A description of the firewall policy.
--
-- 'dryRun', 'updateFirewallPolicy_dryRun' - Indicates whether you want Network Firewall to just check the validity
-- of the request, rather than run the request.
--
-- If set to @TRUE@, Network Firewall checks whether the request can run
-- successfully, but doesn\'t actually make the requested changes. The call
-- returns the value that the request would return if you ran it with dry
-- run set to @FALSE@, but doesn\'t make additions or changes to your
-- resources. This option allows you to make sure that you have the
-- required permissions to run the request and that your request parameters
-- are valid.
--
-- If set to @FALSE@, Network Firewall makes the requested changes to your
-- resources.
--
-- 'encryptionConfiguration', 'updateFirewallPolicy_encryptionConfiguration' - A complex type that contains settings for encryption of your firewall
-- policy resources.
--
-- 'firewallPolicyArn', 'updateFirewallPolicy_firewallPolicyArn' - The Amazon Resource Name (ARN) of the firewall policy.
--
-- You must specify the ARN or the name, and you can specify both.
--
-- 'firewallPolicyName', 'updateFirewallPolicy_firewallPolicyName' - The descriptive name of the firewall policy. You can\'t change the name
-- of a firewall policy after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
--
-- 'updateToken', 'updateFirewallPolicy_updateToken' - A token used for optimistic locking. Network Firewall returns a token to
-- your requests that access the firewall policy. The token marks the state
-- of the policy resource at the time of the request.
--
-- To make changes to the policy, you provide the token in your request.
-- Network Firewall uses the token to ensure that the policy hasn\'t
-- changed since you last retrieved it. If it has changed, the operation
-- fails with an @InvalidTokenException@. If this happens, retrieve the
-- firewall policy again to get a current copy of it with current token.
-- Reapply your changes as needed, then try the operation again using the
-- new token.
--
-- 'firewallPolicy', 'updateFirewallPolicy_firewallPolicy' - The updated firewall policy to use for the firewall.
newUpdateFirewallPolicy ::
  -- | 'updateToken'
  Prelude.Text ->
  -- | 'firewallPolicy'
  FirewallPolicy ->
  UpdateFirewallPolicy
newUpdateFirewallPolicy
  pUpdateToken_
  pFirewallPolicy_ =
    UpdateFirewallPolicy'
      { description =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        encryptionConfiguration = Prelude.Nothing,
        firewallPolicyArn = Prelude.Nothing,
        firewallPolicyName = Prelude.Nothing,
        updateToken = pUpdateToken_,
        firewallPolicy = pFirewallPolicy_
      }

-- | A description of the firewall policy.
updateFirewallPolicy_description :: Lens.Lens' UpdateFirewallPolicy (Prelude.Maybe Prelude.Text)
updateFirewallPolicy_description = Lens.lens (\UpdateFirewallPolicy' {description} -> description) (\s@UpdateFirewallPolicy' {} a -> s {description = a} :: UpdateFirewallPolicy)

-- | Indicates whether you want Network Firewall to just check the validity
-- of the request, rather than run the request.
--
-- If set to @TRUE@, Network Firewall checks whether the request can run
-- successfully, but doesn\'t actually make the requested changes. The call
-- returns the value that the request would return if you ran it with dry
-- run set to @FALSE@, but doesn\'t make additions or changes to your
-- resources. This option allows you to make sure that you have the
-- required permissions to run the request and that your request parameters
-- are valid.
--
-- If set to @FALSE@, Network Firewall makes the requested changes to your
-- resources.
updateFirewallPolicy_dryRun :: Lens.Lens' UpdateFirewallPolicy (Prelude.Maybe Prelude.Bool)
updateFirewallPolicy_dryRun = Lens.lens (\UpdateFirewallPolicy' {dryRun} -> dryRun) (\s@UpdateFirewallPolicy' {} a -> s {dryRun = a} :: UpdateFirewallPolicy)

-- | A complex type that contains settings for encryption of your firewall
-- policy resources.
updateFirewallPolicy_encryptionConfiguration :: Lens.Lens' UpdateFirewallPolicy (Prelude.Maybe EncryptionConfiguration)
updateFirewallPolicy_encryptionConfiguration = Lens.lens (\UpdateFirewallPolicy' {encryptionConfiguration} -> encryptionConfiguration) (\s@UpdateFirewallPolicy' {} a -> s {encryptionConfiguration = a} :: UpdateFirewallPolicy)

-- | The Amazon Resource Name (ARN) of the firewall policy.
--
-- You must specify the ARN or the name, and you can specify both.
updateFirewallPolicy_firewallPolicyArn :: Lens.Lens' UpdateFirewallPolicy (Prelude.Maybe Prelude.Text)
updateFirewallPolicy_firewallPolicyArn = Lens.lens (\UpdateFirewallPolicy' {firewallPolicyArn} -> firewallPolicyArn) (\s@UpdateFirewallPolicy' {} a -> s {firewallPolicyArn = a} :: UpdateFirewallPolicy)

-- | The descriptive name of the firewall policy. You can\'t change the name
-- of a firewall policy after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
updateFirewallPolicy_firewallPolicyName :: Lens.Lens' UpdateFirewallPolicy (Prelude.Maybe Prelude.Text)
updateFirewallPolicy_firewallPolicyName = Lens.lens (\UpdateFirewallPolicy' {firewallPolicyName} -> firewallPolicyName) (\s@UpdateFirewallPolicy' {} a -> s {firewallPolicyName = a} :: UpdateFirewallPolicy)

-- | A token used for optimistic locking. Network Firewall returns a token to
-- your requests that access the firewall policy. The token marks the state
-- of the policy resource at the time of the request.
--
-- To make changes to the policy, you provide the token in your request.
-- Network Firewall uses the token to ensure that the policy hasn\'t
-- changed since you last retrieved it. If it has changed, the operation
-- fails with an @InvalidTokenException@. If this happens, retrieve the
-- firewall policy again to get a current copy of it with current token.
-- Reapply your changes as needed, then try the operation again using the
-- new token.
updateFirewallPolicy_updateToken :: Lens.Lens' UpdateFirewallPolicy Prelude.Text
updateFirewallPolicy_updateToken = Lens.lens (\UpdateFirewallPolicy' {updateToken} -> updateToken) (\s@UpdateFirewallPolicy' {} a -> s {updateToken = a} :: UpdateFirewallPolicy)

-- | The updated firewall policy to use for the firewall.
updateFirewallPolicy_firewallPolicy :: Lens.Lens' UpdateFirewallPolicy FirewallPolicy
updateFirewallPolicy_firewallPolicy = Lens.lens (\UpdateFirewallPolicy' {firewallPolicy} -> firewallPolicy) (\s@UpdateFirewallPolicy' {} a -> s {firewallPolicy = a} :: UpdateFirewallPolicy)

instance Core.AWSRequest UpdateFirewallPolicy where
  type
    AWSResponse UpdateFirewallPolicy =
      UpdateFirewallPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFirewallPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "UpdateToken")
            Prelude.<*> (x Data..:> "FirewallPolicyResponse")
      )

instance Prelude.Hashable UpdateFirewallPolicy where
  hashWithSalt _salt UpdateFirewallPolicy' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` encryptionConfiguration
      `Prelude.hashWithSalt` firewallPolicyArn
      `Prelude.hashWithSalt` firewallPolicyName
      `Prelude.hashWithSalt` updateToken
      `Prelude.hashWithSalt` firewallPolicy

instance Prelude.NFData UpdateFirewallPolicy where
  rnf UpdateFirewallPolicy' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf encryptionConfiguration
      `Prelude.seq` Prelude.rnf firewallPolicyArn
      `Prelude.seq` Prelude.rnf firewallPolicyName
      `Prelude.seq` Prelude.rnf updateToken
      `Prelude.seq` Prelude.rnf firewallPolicy

instance Data.ToHeaders UpdateFirewallPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "NetworkFirewall_20201112.UpdateFirewallPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateFirewallPolicy where
  toJSON UpdateFirewallPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("DryRun" Data..=) Prelude.<$> dryRun,
            ("EncryptionConfiguration" Data..=)
              Prelude.<$> encryptionConfiguration,
            ("FirewallPolicyArn" Data..=)
              Prelude.<$> firewallPolicyArn,
            ("FirewallPolicyName" Data..=)
              Prelude.<$> firewallPolicyName,
            Prelude.Just ("UpdateToken" Data..= updateToken),
            Prelude.Just
              ("FirewallPolicy" Data..= firewallPolicy)
          ]
      )

instance Data.ToPath UpdateFirewallPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateFirewallPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFirewallPolicyResponse' smart constructor.
data UpdateFirewallPolicyResponse = UpdateFirewallPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A token used for optimistic locking. Network Firewall returns a token to
    -- your requests that access the firewall policy. The token marks the state
    -- of the policy resource at the time of the request.
    --
    -- To make changes to the policy, you provide the token in your request.
    -- Network Firewall uses the token to ensure that the policy hasn\'t
    -- changed since you last retrieved it. If it has changed, the operation
    -- fails with an @InvalidTokenException@. If this happens, retrieve the
    -- firewall policy again to get a current copy of it with current token.
    -- Reapply your changes as needed, then try the operation again using the
    -- new token.
    updateToken :: Prelude.Text,
    -- | The high-level properties of a firewall policy. This, along with the
    -- FirewallPolicy, define the policy. You can retrieve all objects for a
    -- firewall policy by calling DescribeFirewallPolicy.
    firewallPolicyResponse :: FirewallPolicyResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFirewallPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateFirewallPolicyResponse_httpStatus' - The response's http status code.
--
-- 'updateToken', 'updateFirewallPolicyResponse_updateToken' - A token used for optimistic locking. Network Firewall returns a token to
-- your requests that access the firewall policy. The token marks the state
-- of the policy resource at the time of the request.
--
-- To make changes to the policy, you provide the token in your request.
-- Network Firewall uses the token to ensure that the policy hasn\'t
-- changed since you last retrieved it. If it has changed, the operation
-- fails with an @InvalidTokenException@. If this happens, retrieve the
-- firewall policy again to get a current copy of it with current token.
-- Reapply your changes as needed, then try the operation again using the
-- new token.
--
-- 'firewallPolicyResponse', 'updateFirewallPolicyResponse_firewallPolicyResponse' - The high-level properties of a firewall policy. This, along with the
-- FirewallPolicy, define the policy. You can retrieve all objects for a
-- firewall policy by calling DescribeFirewallPolicy.
newUpdateFirewallPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'updateToken'
  Prelude.Text ->
  -- | 'firewallPolicyResponse'
  FirewallPolicyResponse ->
  UpdateFirewallPolicyResponse
newUpdateFirewallPolicyResponse
  pHttpStatus_
  pUpdateToken_
  pFirewallPolicyResponse_ =
    UpdateFirewallPolicyResponse'
      { httpStatus =
          pHttpStatus_,
        updateToken = pUpdateToken_,
        firewallPolicyResponse =
          pFirewallPolicyResponse_
      }

-- | The response's http status code.
updateFirewallPolicyResponse_httpStatus :: Lens.Lens' UpdateFirewallPolicyResponse Prelude.Int
updateFirewallPolicyResponse_httpStatus = Lens.lens (\UpdateFirewallPolicyResponse' {httpStatus} -> httpStatus) (\s@UpdateFirewallPolicyResponse' {} a -> s {httpStatus = a} :: UpdateFirewallPolicyResponse)

-- | A token used for optimistic locking. Network Firewall returns a token to
-- your requests that access the firewall policy. The token marks the state
-- of the policy resource at the time of the request.
--
-- To make changes to the policy, you provide the token in your request.
-- Network Firewall uses the token to ensure that the policy hasn\'t
-- changed since you last retrieved it. If it has changed, the operation
-- fails with an @InvalidTokenException@. If this happens, retrieve the
-- firewall policy again to get a current copy of it with current token.
-- Reapply your changes as needed, then try the operation again using the
-- new token.
updateFirewallPolicyResponse_updateToken :: Lens.Lens' UpdateFirewallPolicyResponse Prelude.Text
updateFirewallPolicyResponse_updateToken = Lens.lens (\UpdateFirewallPolicyResponse' {updateToken} -> updateToken) (\s@UpdateFirewallPolicyResponse' {} a -> s {updateToken = a} :: UpdateFirewallPolicyResponse)

-- | The high-level properties of a firewall policy. This, along with the
-- FirewallPolicy, define the policy. You can retrieve all objects for a
-- firewall policy by calling DescribeFirewallPolicy.
updateFirewallPolicyResponse_firewallPolicyResponse :: Lens.Lens' UpdateFirewallPolicyResponse FirewallPolicyResponse
updateFirewallPolicyResponse_firewallPolicyResponse = Lens.lens (\UpdateFirewallPolicyResponse' {firewallPolicyResponse} -> firewallPolicyResponse) (\s@UpdateFirewallPolicyResponse' {} a -> s {firewallPolicyResponse = a} :: UpdateFirewallPolicyResponse)

instance Prelude.NFData UpdateFirewallPolicyResponse where
  rnf UpdateFirewallPolicyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf updateToken
      `Prelude.seq` Prelude.rnf firewallPolicyResponse
