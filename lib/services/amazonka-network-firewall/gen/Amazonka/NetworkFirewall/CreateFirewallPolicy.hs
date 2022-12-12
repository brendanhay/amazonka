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
-- Module      : Amazonka.NetworkFirewall.CreateFirewallPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the firewall policy for the firewall according to the
-- specifications.
--
-- An Network Firewall firewall policy defines the behavior of a firewall,
-- in a collection of stateless and stateful rule groups and other
-- settings. You can use one firewall policy for multiple firewalls.
module Amazonka.NetworkFirewall.CreateFirewallPolicy
  ( -- * Creating a Request
    CreateFirewallPolicy (..),
    newCreateFirewallPolicy,

    -- * Request Lenses
    createFirewallPolicy_description,
    createFirewallPolicy_dryRun,
    createFirewallPolicy_encryptionConfiguration,
    createFirewallPolicy_tags,
    createFirewallPolicy_firewallPolicyName,
    createFirewallPolicy_firewallPolicy,

    -- * Destructuring the Response
    CreateFirewallPolicyResponse (..),
    newCreateFirewallPolicyResponse,

    -- * Response Lenses
    createFirewallPolicyResponse_httpStatus,
    createFirewallPolicyResponse_updateToken,
    createFirewallPolicyResponse_firewallPolicyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFirewallPolicy' smart constructor.
data CreateFirewallPolicy = CreateFirewallPolicy'
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
    -- | The key:value pairs to associate with the resource.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The descriptive name of the firewall policy. You can\'t change the name
    -- of a firewall policy after you create it.
    firewallPolicyName :: Prelude.Text,
    -- | The rule groups and policy actions to use in the firewall policy.
    firewallPolicy :: FirewallPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFirewallPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createFirewallPolicy_description' - A description of the firewall policy.
--
-- 'dryRun', 'createFirewallPolicy_dryRun' - Indicates whether you want Network Firewall to just check the validity
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
-- 'encryptionConfiguration', 'createFirewallPolicy_encryptionConfiguration' - A complex type that contains settings for encryption of your firewall
-- policy resources.
--
-- 'tags', 'createFirewallPolicy_tags' - The key:value pairs to associate with the resource.
--
-- 'firewallPolicyName', 'createFirewallPolicy_firewallPolicyName' - The descriptive name of the firewall policy. You can\'t change the name
-- of a firewall policy after you create it.
--
-- 'firewallPolicy', 'createFirewallPolicy_firewallPolicy' - The rule groups and policy actions to use in the firewall policy.
newCreateFirewallPolicy ::
  -- | 'firewallPolicyName'
  Prelude.Text ->
  -- | 'firewallPolicy'
  FirewallPolicy ->
  CreateFirewallPolicy
newCreateFirewallPolicy
  pFirewallPolicyName_
  pFirewallPolicy_ =
    CreateFirewallPolicy'
      { description =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        encryptionConfiguration = Prelude.Nothing,
        tags = Prelude.Nothing,
        firewallPolicyName = pFirewallPolicyName_,
        firewallPolicy = pFirewallPolicy_
      }

-- | A description of the firewall policy.
createFirewallPolicy_description :: Lens.Lens' CreateFirewallPolicy (Prelude.Maybe Prelude.Text)
createFirewallPolicy_description = Lens.lens (\CreateFirewallPolicy' {description} -> description) (\s@CreateFirewallPolicy' {} a -> s {description = a} :: CreateFirewallPolicy)

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
createFirewallPolicy_dryRun :: Lens.Lens' CreateFirewallPolicy (Prelude.Maybe Prelude.Bool)
createFirewallPolicy_dryRun = Lens.lens (\CreateFirewallPolicy' {dryRun} -> dryRun) (\s@CreateFirewallPolicy' {} a -> s {dryRun = a} :: CreateFirewallPolicy)

-- | A complex type that contains settings for encryption of your firewall
-- policy resources.
createFirewallPolicy_encryptionConfiguration :: Lens.Lens' CreateFirewallPolicy (Prelude.Maybe EncryptionConfiguration)
createFirewallPolicy_encryptionConfiguration = Lens.lens (\CreateFirewallPolicy' {encryptionConfiguration} -> encryptionConfiguration) (\s@CreateFirewallPolicy' {} a -> s {encryptionConfiguration = a} :: CreateFirewallPolicy)

-- | The key:value pairs to associate with the resource.
createFirewallPolicy_tags :: Lens.Lens' CreateFirewallPolicy (Prelude.Maybe (Prelude.NonEmpty Tag))
createFirewallPolicy_tags = Lens.lens (\CreateFirewallPolicy' {tags} -> tags) (\s@CreateFirewallPolicy' {} a -> s {tags = a} :: CreateFirewallPolicy) Prelude.. Lens.mapping Lens.coerced

-- | The descriptive name of the firewall policy. You can\'t change the name
-- of a firewall policy after you create it.
createFirewallPolicy_firewallPolicyName :: Lens.Lens' CreateFirewallPolicy Prelude.Text
createFirewallPolicy_firewallPolicyName = Lens.lens (\CreateFirewallPolicy' {firewallPolicyName} -> firewallPolicyName) (\s@CreateFirewallPolicy' {} a -> s {firewallPolicyName = a} :: CreateFirewallPolicy)

-- | The rule groups and policy actions to use in the firewall policy.
createFirewallPolicy_firewallPolicy :: Lens.Lens' CreateFirewallPolicy FirewallPolicy
createFirewallPolicy_firewallPolicy = Lens.lens (\CreateFirewallPolicy' {firewallPolicy} -> firewallPolicy) (\s@CreateFirewallPolicy' {} a -> s {firewallPolicy = a} :: CreateFirewallPolicy)

instance Core.AWSRequest CreateFirewallPolicy where
  type
    AWSResponse CreateFirewallPolicy =
      CreateFirewallPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFirewallPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "UpdateToken")
            Prelude.<*> (x Data..:> "FirewallPolicyResponse")
      )

instance Prelude.Hashable CreateFirewallPolicy where
  hashWithSalt _salt CreateFirewallPolicy' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` encryptionConfiguration
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` firewallPolicyName
      `Prelude.hashWithSalt` firewallPolicy

instance Prelude.NFData CreateFirewallPolicy where
  rnf CreateFirewallPolicy' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf encryptionConfiguration
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf firewallPolicyName
      `Prelude.seq` Prelude.rnf firewallPolicy

instance Data.ToHeaders CreateFirewallPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "NetworkFirewall_20201112.CreateFirewallPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateFirewallPolicy where
  toJSON CreateFirewallPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("DryRun" Data..=) Prelude.<$> dryRun,
            ("EncryptionConfiguration" Data..=)
              Prelude.<$> encryptionConfiguration,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("FirewallPolicyName" Data..= firewallPolicyName),
            Prelude.Just
              ("FirewallPolicy" Data..= firewallPolicy)
          ]
      )

instance Data.ToPath CreateFirewallPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateFirewallPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFirewallPolicyResponse' smart constructor.
data CreateFirewallPolicyResponse = CreateFirewallPolicyResponse'
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
-- Create a value of 'CreateFirewallPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createFirewallPolicyResponse_httpStatus' - The response's http status code.
--
-- 'updateToken', 'createFirewallPolicyResponse_updateToken' - A token used for optimistic locking. Network Firewall returns a token to
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
-- 'firewallPolicyResponse', 'createFirewallPolicyResponse_firewallPolicyResponse' - The high-level properties of a firewall policy. This, along with the
-- FirewallPolicy, define the policy. You can retrieve all objects for a
-- firewall policy by calling DescribeFirewallPolicy.
newCreateFirewallPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'updateToken'
  Prelude.Text ->
  -- | 'firewallPolicyResponse'
  FirewallPolicyResponse ->
  CreateFirewallPolicyResponse
newCreateFirewallPolicyResponse
  pHttpStatus_
  pUpdateToken_
  pFirewallPolicyResponse_ =
    CreateFirewallPolicyResponse'
      { httpStatus =
          pHttpStatus_,
        updateToken = pUpdateToken_,
        firewallPolicyResponse =
          pFirewallPolicyResponse_
      }

-- | The response's http status code.
createFirewallPolicyResponse_httpStatus :: Lens.Lens' CreateFirewallPolicyResponse Prelude.Int
createFirewallPolicyResponse_httpStatus = Lens.lens (\CreateFirewallPolicyResponse' {httpStatus} -> httpStatus) (\s@CreateFirewallPolicyResponse' {} a -> s {httpStatus = a} :: CreateFirewallPolicyResponse)

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
createFirewallPolicyResponse_updateToken :: Lens.Lens' CreateFirewallPolicyResponse Prelude.Text
createFirewallPolicyResponse_updateToken = Lens.lens (\CreateFirewallPolicyResponse' {updateToken} -> updateToken) (\s@CreateFirewallPolicyResponse' {} a -> s {updateToken = a} :: CreateFirewallPolicyResponse)

-- | The high-level properties of a firewall policy. This, along with the
-- FirewallPolicy, define the policy. You can retrieve all objects for a
-- firewall policy by calling DescribeFirewallPolicy.
createFirewallPolicyResponse_firewallPolicyResponse :: Lens.Lens' CreateFirewallPolicyResponse FirewallPolicyResponse
createFirewallPolicyResponse_firewallPolicyResponse = Lens.lens (\CreateFirewallPolicyResponse' {firewallPolicyResponse} -> firewallPolicyResponse) (\s@CreateFirewallPolicyResponse' {} a -> s {firewallPolicyResponse = a} :: CreateFirewallPolicyResponse)

instance Prelude.NFData CreateFirewallPolicyResponse where
  rnf CreateFirewallPolicyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf updateToken
      `Prelude.seq` Prelude.rnf firewallPolicyResponse
