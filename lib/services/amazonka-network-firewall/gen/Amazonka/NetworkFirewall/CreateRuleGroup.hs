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
-- Module      : Amazonka.NetworkFirewall.CreateRuleGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the specified stateless or stateful rule group, which includes
-- the rules for network traffic inspection, a capacity setting, and tags.
--
-- You provide your rule group specification in your request using either
-- @RuleGroup@ or @Rules@.
module Amazonka.NetworkFirewall.CreateRuleGroup
  ( -- * Creating a Request
    CreateRuleGroup (..),
    newCreateRuleGroup,

    -- * Request Lenses
    createRuleGroup_description,
    createRuleGroup_dryRun,
    createRuleGroup_encryptionConfiguration,
    createRuleGroup_ruleGroup,
    createRuleGroup_rules,
    createRuleGroup_sourceMetadata,
    createRuleGroup_tags,
    createRuleGroup_ruleGroupName,
    createRuleGroup_type,
    createRuleGroup_capacity,

    -- * Destructuring the Response
    CreateRuleGroupResponse (..),
    newCreateRuleGroupResponse,

    -- * Response Lenses
    createRuleGroupResponse_httpStatus,
    createRuleGroupResponse_updateToken,
    createRuleGroupResponse_ruleGroupResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateRuleGroup' smart constructor.
data CreateRuleGroup = CreateRuleGroup'
  { -- | A description of the rule group.
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
    -- | A complex type that contains settings for encryption of your rule group
    -- resources.
    encryptionConfiguration :: Prelude.Maybe EncryptionConfiguration,
    -- | An object that defines the rule group rules.
    --
    -- You must provide either this rule group setting or a @Rules@ setting,
    -- but not both.
    ruleGroup :: Prelude.Maybe RuleGroup,
    -- | A string containing stateful rule group rules specifications in Suricata
    -- flat format, with one rule per line. Use this to import your existing
    -- Suricata compatible rule groups.
    --
    -- You must provide either this rules setting or a populated @RuleGroup@
    -- setting, but not both.
    --
    -- You can provide your rule group specification in Suricata flat format
    -- through this setting when you create or update your rule group. The call
    -- response returns a RuleGroup object that Network Firewall has populated
    -- from your string.
    rules :: Prelude.Maybe Prelude.Text,
    -- | A complex type that contains metadata about the rule group that your own
    -- rule group is copied from. You can use the metadata to keep track of
    -- updates made to the originating rule group.
    sourceMetadata :: Prelude.Maybe SourceMetadata,
    -- | The key:value pairs to associate with the resource.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The descriptive name of the rule group. You can\'t change the name of a
    -- rule group after you create it.
    ruleGroupName :: Prelude.Text,
    -- | Indicates whether the rule group is stateless or stateful. If the rule
    -- group is stateless, it contains stateless rules. If it is stateful, it
    -- contains stateful rules.
    type' :: RuleGroupType,
    -- | The maximum operating resources that this rule group can use. Rule group
    -- capacity is fixed at creation. When you update a rule group, you are
    -- limited to this capacity. When you reference a rule group from a
    -- firewall policy, Network Firewall reserves this capacity for the rule
    -- group.
    --
    -- You can retrieve the capacity that would be required for a rule group
    -- before you create the rule group by calling CreateRuleGroup with
    -- @DryRun@ set to @TRUE@.
    --
    -- You can\'t change or exceed this capacity when you update the rule
    -- group, so leave room for your rule group to grow.
    --
    -- __Capacity for a stateless rule group__
    --
    -- For a stateless rule group, the capacity required is the sum of the
    -- capacity requirements of the individual rules that you expect to have in
    -- the rule group.
    --
    -- To calculate the capacity requirement of a single rule, multiply the
    -- capacity requirement values of each of the rule\'s match settings:
    --
    -- -   A match setting with no criteria specified has a value of 1.
    --
    -- -   A match setting with @Any@ specified has a value of 1.
    --
    -- -   All other match settings have a value equal to the number of
    --     elements provided in the setting. For example, a protocol setting
    --     [\"UDP\"] and a source setting [\"10.0.0.0\/24\"] each have a value
    --     of 1. A protocol setting [\"UDP\",\"TCP\"] has a value of 2. A
    --     source setting [\"10.0.0.0\/24\",\"10.0.0.1\/24\",\"10.0.0.2\/24\"]
    --     has a value of 3.
    --
    -- A rule with no criteria specified in any of its match settings has a
    -- capacity requirement of 1. A rule with protocol setting
    -- [\"UDP\",\"TCP\"], source setting
    -- [\"10.0.0.0\/24\",\"10.0.0.1\/24\",\"10.0.0.2\/24\"], and a single
    -- specification or no specification for each of the other match settings
    -- has a capacity requirement of 6.
    --
    -- __Capacity for a stateful rule group__
    --
    -- For a stateful rule group, the minimum capacity required is the number
    -- of individual rules that you expect to have in the rule group.
    capacity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRuleGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createRuleGroup_description' - A description of the rule group.
--
-- 'dryRun', 'createRuleGroup_dryRun' - Indicates whether you want Network Firewall to just check the validity
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
-- 'encryptionConfiguration', 'createRuleGroup_encryptionConfiguration' - A complex type that contains settings for encryption of your rule group
-- resources.
--
-- 'ruleGroup', 'createRuleGroup_ruleGroup' - An object that defines the rule group rules.
--
-- You must provide either this rule group setting or a @Rules@ setting,
-- but not both.
--
-- 'rules', 'createRuleGroup_rules' - A string containing stateful rule group rules specifications in Suricata
-- flat format, with one rule per line. Use this to import your existing
-- Suricata compatible rule groups.
--
-- You must provide either this rules setting or a populated @RuleGroup@
-- setting, but not both.
--
-- You can provide your rule group specification in Suricata flat format
-- through this setting when you create or update your rule group. The call
-- response returns a RuleGroup object that Network Firewall has populated
-- from your string.
--
-- 'sourceMetadata', 'createRuleGroup_sourceMetadata' - A complex type that contains metadata about the rule group that your own
-- rule group is copied from. You can use the metadata to keep track of
-- updates made to the originating rule group.
--
-- 'tags', 'createRuleGroup_tags' - The key:value pairs to associate with the resource.
--
-- 'ruleGroupName', 'createRuleGroup_ruleGroupName' - The descriptive name of the rule group. You can\'t change the name of a
-- rule group after you create it.
--
-- 'type'', 'createRuleGroup_type' - Indicates whether the rule group is stateless or stateful. If the rule
-- group is stateless, it contains stateless rules. If it is stateful, it
-- contains stateful rules.
--
-- 'capacity', 'createRuleGroup_capacity' - The maximum operating resources that this rule group can use. Rule group
-- capacity is fixed at creation. When you update a rule group, you are
-- limited to this capacity. When you reference a rule group from a
-- firewall policy, Network Firewall reserves this capacity for the rule
-- group.
--
-- You can retrieve the capacity that would be required for a rule group
-- before you create the rule group by calling CreateRuleGroup with
-- @DryRun@ set to @TRUE@.
--
-- You can\'t change or exceed this capacity when you update the rule
-- group, so leave room for your rule group to grow.
--
-- __Capacity for a stateless rule group__
--
-- For a stateless rule group, the capacity required is the sum of the
-- capacity requirements of the individual rules that you expect to have in
-- the rule group.
--
-- To calculate the capacity requirement of a single rule, multiply the
-- capacity requirement values of each of the rule\'s match settings:
--
-- -   A match setting with no criteria specified has a value of 1.
--
-- -   A match setting with @Any@ specified has a value of 1.
--
-- -   All other match settings have a value equal to the number of
--     elements provided in the setting. For example, a protocol setting
--     [\"UDP\"] and a source setting [\"10.0.0.0\/24\"] each have a value
--     of 1. A protocol setting [\"UDP\",\"TCP\"] has a value of 2. A
--     source setting [\"10.0.0.0\/24\",\"10.0.0.1\/24\",\"10.0.0.2\/24\"]
--     has a value of 3.
--
-- A rule with no criteria specified in any of its match settings has a
-- capacity requirement of 1. A rule with protocol setting
-- [\"UDP\",\"TCP\"], source setting
-- [\"10.0.0.0\/24\",\"10.0.0.1\/24\",\"10.0.0.2\/24\"], and a single
-- specification or no specification for each of the other match settings
-- has a capacity requirement of 6.
--
-- __Capacity for a stateful rule group__
--
-- For a stateful rule group, the minimum capacity required is the number
-- of individual rules that you expect to have in the rule group.
newCreateRuleGroup ::
  -- | 'ruleGroupName'
  Prelude.Text ->
  -- | 'type''
  RuleGroupType ->
  -- | 'capacity'
  Prelude.Int ->
  CreateRuleGroup
newCreateRuleGroup pRuleGroupName_ pType_ pCapacity_ =
  CreateRuleGroup'
    { description = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      encryptionConfiguration = Prelude.Nothing,
      ruleGroup = Prelude.Nothing,
      rules = Prelude.Nothing,
      sourceMetadata = Prelude.Nothing,
      tags = Prelude.Nothing,
      ruleGroupName = pRuleGroupName_,
      type' = pType_,
      capacity = pCapacity_
    }

-- | A description of the rule group.
createRuleGroup_description :: Lens.Lens' CreateRuleGroup (Prelude.Maybe Prelude.Text)
createRuleGroup_description = Lens.lens (\CreateRuleGroup' {description} -> description) (\s@CreateRuleGroup' {} a -> s {description = a} :: CreateRuleGroup)

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
createRuleGroup_dryRun :: Lens.Lens' CreateRuleGroup (Prelude.Maybe Prelude.Bool)
createRuleGroup_dryRun = Lens.lens (\CreateRuleGroup' {dryRun} -> dryRun) (\s@CreateRuleGroup' {} a -> s {dryRun = a} :: CreateRuleGroup)

-- | A complex type that contains settings for encryption of your rule group
-- resources.
createRuleGroup_encryptionConfiguration :: Lens.Lens' CreateRuleGroup (Prelude.Maybe EncryptionConfiguration)
createRuleGroup_encryptionConfiguration = Lens.lens (\CreateRuleGroup' {encryptionConfiguration} -> encryptionConfiguration) (\s@CreateRuleGroup' {} a -> s {encryptionConfiguration = a} :: CreateRuleGroup)

-- | An object that defines the rule group rules.
--
-- You must provide either this rule group setting or a @Rules@ setting,
-- but not both.
createRuleGroup_ruleGroup :: Lens.Lens' CreateRuleGroup (Prelude.Maybe RuleGroup)
createRuleGroup_ruleGroup = Lens.lens (\CreateRuleGroup' {ruleGroup} -> ruleGroup) (\s@CreateRuleGroup' {} a -> s {ruleGroup = a} :: CreateRuleGroup)

-- | A string containing stateful rule group rules specifications in Suricata
-- flat format, with one rule per line. Use this to import your existing
-- Suricata compatible rule groups.
--
-- You must provide either this rules setting or a populated @RuleGroup@
-- setting, but not both.
--
-- You can provide your rule group specification in Suricata flat format
-- through this setting when you create or update your rule group. The call
-- response returns a RuleGroup object that Network Firewall has populated
-- from your string.
createRuleGroup_rules :: Lens.Lens' CreateRuleGroup (Prelude.Maybe Prelude.Text)
createRuleGroup_rules = Lens.lens (\CreateRuleGroup' {rules} -> rules) (\s@CreateRuleGroup' {} a -> s {rules = a} :: CreateRuleGroup)

-- | A complex type that contains metadata about the rule group that your own
-- rule group is copied from. You can use the metadata to keep track of
-- updates made to the originating rule group.
createRuleGroup_sourceMetadata :: Lens.Lens' CreateRuleGroup (Prelude.Maybe SourceMetadata)
createRuleGroup_sourceMetadata = Lens.lens (\CreateRuleGroup' {sourceMetadata} -> sourceMetadata) (\s@CreateRuleGroup' {} a -> s {sourceMetadata = a} :: CreateRuleGroup)

-- | The key:value pairs to associate with the resource.
createRuleGroup_tags :: Lens.Lens' CreateRuleGroup (Prelude.Maybe (Prelude.NonEmpty Tag))
createRuleGroup_tags = Lens.lens (\CreateRuleGroup' {tags} -> tags) (\s@CreateRuleGroup' {} a -> s {tags = a} :: CreateRuleGroup) Prelude.. Lens.mapping Lens.coerced

-- | The descriptive name of the rule group. You can\'t change the name of a
-- rule group after you create it.
createRuleGroup_ruleGroupName :: Lens.Lens' CreateRuleGroup Prelude.Text
createRuleGroup_ruleGroupName = Lens.lens (\CreateRuleGroup' {ruleGroupName} -> ruleGroupName) (\s@CreateRuleGroup' {} a -> s {ruleGroupName = a} :: CreateRuleGroup)

-- | Indicates whether the rule group is stateless or stateful. If the rule
-- group is stateless, it contains stateless rules. If it is stateful, it
-- contains stateful rules.
createRuleGroup_type :: Lens.Lens' CreateRuleGroup RuleGroupType
createRuleGroup_type = Lens.lens (\CreateRuleGroup' {type'} -> type') (\s@CreateRuleGroup' {} a -> s {type' = a} :: CreateRuleGroup)

-- | The maximum operating resources that this rule group can use. Rule group
-- capacity is fixed at creation. When you update a rule group, you are
-- limited to this capacity. When you reference a rule group from a
-- firewall policy, Network Firewall reserves this capacity for the rule
-- group.
--
-- You can retrieve the capacity that would be required for a rule group
-- before you create the rule group by calling CreateRuleGroup with
-- @DryRun@ set to @TRUE@.
--
-- You can\'t change or exceed this capacity when you update the rule
-- group, so leave room for your rule group to grow.
--
-- __Capacity for a stateless rule group__
--
-- For a stateless rule group, the capacity required is the sum of the
-- capacity requirements of the individual rules that you expect to have in
-- the rule group.
--
-- To calculate the capacity requirement of a single rule, multiply the
-- capacity requirement values of each of the rule\'s match settings:
--
-- -   A match setting with no criteria specified has a value of 1.
--
-- -   A match setting with @Any@ specified has a value of 1.
--
-- -   All other match settings have a value equal to the number of
--     elements provided in the setting. For example, a protocol setting
--     [\"UDP\"] and a source setting [\"10.0.0.0\/24\"] each have a value
--     of 1. A protocol setting [\"UDP\",\"TCP\"] has a value of 2. A
--     source setting [\"10.0.0.0\/24\",\"10.0.0.1\/24\",\"10.0.0.2\/24\"]
--     has a value of 3.
--
-- A rule with no criteria specified in any of its match settings has a
-- capacity requirement of 1. A rule with protocol setting
-- [\"UDP\",\"TCP\"], source setting
-- [\"10.0.0.0\/24\",\"10.0.0.1\/24\",\"10.0.0.2\/24\"], and a single
-- specification or no specification for each of the other match settings
-- has a capacity requirement of 6.
--
-- __Capacity for a stateful rule group__
--
-- For a stateful rule group, the minimum capacity required is the number
-- of individual rules that you expect to have in the rule group.
createRuleGroup_capacity :: Lens.Lens' CreateRuleGroup Prelude.Int
createRuleGroup_capacity = Lens.lens (\CreateRuleGroup' {capacity} -> capacity) (\s@CreateRuleGroup' {} a -> s {capacity = a} :: CreateRuleGroup)

instance Core.AWSRequest CreateRuleGroup where
  type
    AWSResponse CreateRuleGroup =
      CreateRuleGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRuleGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "UpdateToken")
            Prelude.<*> (x Data..:> "RuleGroupResponse")
      )

instance Prelude.Hashable CreateRuleGroup where
  hashWithSalt _salt CreateRuleGroup' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` encryptionConfiguration
      `Prelude.hashWithSalt` ruleGroup
      `Prelude.hashWithSalt` rules
      `Prelude.hashWithSalt` sourceMetadata
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` ruleGroupName
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` capacity

instance Prelude.NFData CreateRuleGroup where
  rnf CreateRuleGroup' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf encryptionConfiguration
      `Prelude.seq` Prelude.rnf ruleGroup
      `Prelude.seq` Prelude.rnf rules
      `Prelude.seq` Prelude.rnf sourceMetadata
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf ruleGroupName
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf capacity

instance Data.ToHeaders CreateRuleGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "NetworkFirewall_20201112.CreateRuleGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateRuleGroup where
  toJSON CreateRuleGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("DryRun" Data..=) Prelude.<$> dryRun,
            ("EncryptionConfiguration" Data..=)
              Prelude.<$> encryptionConfiguration,
            ("RuleGroup" Data..=) Prelude.<$> ruleGroup,
            ("Rules" Data..=) Prelude.<$> rules,
            ("SourceMetadata" Data..=)
              Prelude.<$> sourceMetadata,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("RuleGroupName" Data..= ruleGroupName),
            Prelude.Just ("Type" Data..= type'),
            Prelude.Just ("Capacity" Data..= capacity)
          ]
      )

instance Data.ToPath CreateRuleGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateRuleGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRuleGroupResponse' smart constructor.
data CreateRuleGroupResponse = CreateRuleGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A token used for optimistic locking. Network Firewall returns a token to
    -- your requests that access the rule group. The token marks the state of
    -- the rule group resource at the time of the request.
    --
    -- To make changes to the rule group, you provide the token in your
    -- request. Network Firewall uses the token to ensure that the rule group
    -- hasn\'t changed since you last retrieved it. If it has changed, the
    -- operation fails with an @InvalidTokenException@. If this happens,
    -- retrieve the rule group again to get a current copy of it with a current
    -- token. Reapply your changes as needed, then try the operation again
    -- using the new token.
    updateToken :: Prelude.Text,
    -- | The high-level properties of a rule group. This, along with the
    -- RuleGroup, define the rule group. You can retrieve all objects for a
    -- rule group by calling DescribeRuleGroup.
    ruleGroupResponse :: RuleGroupResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRuleGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createRuleGroupResponse_httpStatus' - The response's http status code.
--
-- 'updateToken', 'createRuleGroupResponse_updateToken' - A token used for optimistic locking. Network Firewall returns a token to
-- your requests that access the rule group. The token marks the state of
-- the rule group resource at the time of the request.
--
-- To make changes to the rule group, you provide the token in your
-- request. Network Firewall uses the token to ensure that the rule group
-- hasn\'t changed since you last retrieved it. If it has changed, the
-- operation fails with an @InvalidTokenException@. If this happens,
-- retrieve the rule group again to get a current copy of it with a current
-- token. Reapply your changes as needed, then try the operation again
-- using the new token.
--
-- 'ruleGroupResponse', 'createRuleGroupResponse_ruleGroupResponse' - The high-level properties of a rule group. This, along with the
-- RuleGroup, define the rule group. You can retrieve all objects for a
-- rule group by calling DescribeRuleGroup.
newCreateRuleGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'updateToken'
  Prelude.Text ->
  -- | 'ruleGroupResponse'
  RuleGroupResponse ->
  CreateRuleGroupResponse
newCreateRuleGroupResponse
  pHttpStatus_
  pUpdateToken_
  pRuleGroupResponse_ =
    CreateRuleGroupResponse'
      { httpStatus = pHttpStatus_,
        updateToken = pUpdateToken_,
        ruleGroupResponse = pRuleGroupResponse_
      }

-- | The response's http status code.
createRuleGroupResponse_httpStatus :: Lens.Lens' CreateRuleGroupResponse Prelude.Int
createRuleGroupResponse_httpStatus = Lens.lens (\CreateRuleGroupResponse' {httpStatus} -> httpStatus) (\s@CreateRuleGroupResponse' {} a -> s {httpStatus = a} :: CreateRuleGroupResponse)

-- | A token used for optimistic locking. Network Firewall returns a token to
-- your requests that access the rule group. The token marks the state of
-- the rule group resource at the time of the request.
--
-- To make changes to the rule group, you provide the token in your
-- request. Network Firewall uses the token to ensure that the rule group
-- hasn\'t changed since you last retrieved it. If it has changed, the
-- operation fails with an @InvalidTokenException@. If this happens,
-- retrieve the rule group again to get a current copy of it with a current
-- token. Reapply your changes as needed, then try the operation again
-- using the new token.
createRuleGroupResponse_updateToken :: Lens.Lens' CreateRuleGroupResponse Prelude.Text
createRuleGroupResponse_updateToken = Lens.lens (\CreateRuleGroupResponse' {updateToken} -> updateToken) (\s@CreateRuleGroupResponse' {} a -> s {updateToken = a} :: CreateRuleGroupResponse)

-- | The high-level properties of a rule group. This, along with the
-- RuleGroup, define the rule group. You can retrieve all objects for a
-- rule group by calling DescribeRuleGroup.
createRuleGroupResponse_ruleGroupResponse :: Lens.Lens' CreateRuleGroupResponse RuleGroupResponse
createRuleGroupResponse_ruleGroupResponse = Lens.lens (\CreateRuleGroupResponse' {ruleGroupResponse} -> ruleGroupResponse) (\s@CreateRuleGroupResponse' {} a -> s {ruleGroupResponse = a} :: CreateRuleGroupResponse)

instance Prelude.NFData CreateRuleGroupResponse where
  rnf CreateRuleGroupResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf updateToken
      `Prelude.seq` Prelude.rnf ruleGroupResponse
