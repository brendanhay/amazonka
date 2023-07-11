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
-- Module      : Amazonka.Route53Resolver.CreateFirewallRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a single DNS Firewall rule in the specified rule group, using
-- the specified domain list.
module Amazonka.Route53Resolver.CreateFirewallRule
  ( -- * Creating a Request
    CreateFirewallRule (..),
    newCreateFirewallRule,

    -- * Request Lenses
    createFirewallRule_blockOverrideDnsType,
    createFirewallRule_blockOverrideDomain,
    createFirewallRule_blockOverrideTtl,
    createFirewallRule_blockResponse,
    createFirewallRule_creatorRequestId,
    createFirewallRule_firewallRuleGroupId,
    createFirewallRule_firewallDomainListId,
    createFirewallRule_priority,
    createFirewallRule_action,
    createFirewallRule_name,

    -- * Destructuring the Response
    CreateFirewallRuleResponse (..),
    newCreateFirewallRuleResponse,

    -- * Response Lenses
    createFirewallRuleResponse_firewallRule,
    createFirewallRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newCreateFirewallRule' smart constructor.
data CreateFirewallRule = CreateFirewallRule'
  { -- | The DNS record\'s type. This determines the format of the record value
    -- that you provided in @BlockOverrideDomain@. Used for the rule action
    -- @BLOCK@ with a @BlockResponse@ setting of @OVERRIDE@.
    --
    -- This setting is required if the @BlockResponse@ setting is @OVERRIDE@.
    blockOverrideDnsType :: Prelude.Maybe BlockOverrideDnsType,
    -- | The custom DNS record to send back in response to the query. Used for
    -- the rule action @BLOCK@ with a @BlockResponse@ setting of @OVERRIDE@.
    --
    -- This setting is required if the @BlockResponse@ setting is @OVERRIDE@.
    blockOverrideDomain :: Prelude.Maybe Prelude.Text,
    -- | The recommended amount of time, in seconds, for the DNS resolver or web
    -- browser to cache the provided override record. Used for the rule action
    -- @BLOCK@ with a @BlockResponse@ setting of @OVERRIDE@.
    --
    -- This setting is required if the @BlockResponse@ setting is @OVERRIDE@.
    blockOverrideTtl :: Prelude.Maybe Prelude.Natural,
    -- | The way that you want DNS Firewall to block the request, used with the
    -- rule action setting @BLOCK@.
    --
    -- -   @NODATA@ - Respond indicating that the query was successful, but no
    --     response is available for it.
    --
    -- -   @NXDOMAIN@ - Respond indicating that the domain name that\'s in the
    --     query doesn\'t exist.
    --
    -- -   @OVERRIDE@ - Provide a custom override in the response. This option
    --     requires custom handling details in the rule\'s @BlockOverride*@
    --     settings.
    --
    -- This setting is required if the rule action setting is @BLOCK@.
    blockResponse :: Prelude.Maybe BlockResponse,
    -- | A unique string that identifies the request and that allows you to retry
    -- failed requests without the risk of running the operation twice.
    -- @CreatorRequestId@ can be any unique string, for example, a date\/time
    -- stamp.
    creatorRequestId :: Prelude.Text,
    -- | The unique identifier of the firewall rule group where you want to
    -- create the rule.
    firewallRuleGroupId :: Prelude.Text,
    -- | The ID of the domain list that you want to use in the rule.
    firewallDomainListId :: Prelude.Text,
    -- | The setting that determines the processing order of the rule in the rule
    -- group. DNS Firewall processes the rules in a rule group by order of
    -- priority, starting from the lowest setting.
    --
    -- You must specify a unique priority for each rule in a rule group. To
    -- make it easier to insert rules later, leave space between the numbers,
    -- for example, use 100, 200, and so on. You can change the priority
    -- setting for the rules in a rule group at any time.
    priority :: Prelude.Int,
    -- | The action that DNS Firewall should take on a DNS query when it matches
    -- one of the domains in the rule\'s domain list:
    --
    -- -   @ALLOW@ - Permit the request to go through.
    --
    -- -   @ALERT@ - Permit the request and send metrics and logs to Cloud
    --     Watch.
    --
    -- -   @BLOCK@ - Disallow the request. This option requires additional
    --     details in the rule\'s @BlockResponse@.
    action :: Action,
    -- | A name that lets you identify the rule in the rule group.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFirewallRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blockOverrideDnsType', 'createFirewallRule_blockOverrideDnsType' - The DNS record\'s type. This determines the format of the record value
-- that you provided in @BlockOverrideDomain@. Used for the rule action
-- @BLOCK@ with a @BlockResponse@ setting of @OVERRIDE@.
--
-- This setting is required if the @BlockResponse@ setting is @OVERRIDE@.
--
-- 'blockOverrideDomain', 'createFirewallRule_blockOverrideDomain' - The custom DNS record to send back in response to the query. Used for
-- the rule action @BLOCK@ with a @BlockResponse@ setting of @OVERRIDE@.
--
-- This setting is required if the @BlockResponse@ setting is @OVERRIDE@.
--
-- 'blockOverrideTtl', 'createFirewallRule_blockOverrideTtl' - The recommended amount of time, in seconds, for the DNS resolver or web
-- browser to cache the provided override record. Used for the rule action
-- @BLOCK@ with a @BlockResponse@ setting of @OVERRIDE@.
--
-- This setting is required if the @BlockResponse@ setting is @OVERRIDE@.
--
-- 'blockResponse', 'createFirewallRule_blockResponse' - The way that you want DNS Firewall to block the request, used with the
-- rule action setting @BLOCK@.
--
-- -   @NODATA@ - Respond indicating that the query was successful, but no
--     response is available for it.
--
-- -   @NXDOMAIN@ - Respond indicating that the domain name that\'s in the
--     query doesn\'t exist.
--
-- -   @OVERRIDE@ - Provide a custom override in the response. This option
--     requires custom handling details in the rule\'s @BlockOverride*@
--     settings.
--
-- This setting is required if the rule action setting is @BLOCK@.
--
-- 'creatorRequestId', 'createFirewallRule_creatorRequestId' - A unique string that identifies the request and that allows you to retry
-- failed requests without the risk of running the operation twice.
-- @CreatorRequestId@ can be any unique string, for example, a date\/time
-- stamp.
--
-- 'firewallRuleGroupId', 'createFirewallRule_firewallRuleGroupId' - The unique identifier of the firewall rule group where you want to
-- create the rule.
--
-- 'firewallDomainListId', 'createFirewallRule_firewallDomainListId' - The ID of the domain list that you want to use in the rule.
--
-- 'priority', 'createFirewallRule_priority' - The setting that determines the processing order of the rule in the rule
-- group. DNS Firewall processes the rules in a rule group by order of
-- priority, starting from the lowest setting.
--
-- You must specify a unique priority for each rule in a rule group. To
-- make it easier to insert rules later, leave space between the numbers,
-- for example, use 100, 200, and so on. You can change the priority
-- setting for the rules in a rule group at any time.
--
-- 'action', 'createFirewallRule_action' - The action that DNS Firewall should take on a DNS query when it matches
-- one of the domains in the rule\'s domain list:
--
-- -   @ALLOW@ - Permit the request to go through.
--
-- -   @ALERT@ - Permit the request and send metrics and logs to Cloud
--     Watch.
--
-- -   @BLOCK@ - Disallow the request. This option requires additional
--     details in the rule\'s @BlockResponse@.
--
-- 'name', 'createFirewallRule_name' - A name that lets you identify the rule in the rule group.
newCreateFirewallRule ::
  -- | 'creatorRequestId'
  Prelude.Text ->
  -- | 'firewallRuleGroupId'
  Prelude.Text ->
  -- | 'firewallDomainListId'
  Prelude.Text ->
  -- | 'priority'
  Prelude.Int ->
  -- | 'action'
  Action ->
  -- | 'name'
  Prelude.Text ->
  CreateFirewallRule
newCreateFirewallRule
  pCreatorRequestId_
  pFirewallRuleGroupId_
  pFirewallDomainListId_
  pPriority_
  pAction_
  pName_ =
    CreateFirewallRule'
      { blockOverrideDnsType =
          Prelude.Nothing,
        blockOverrideDomain = Prelude.Nothing,
        blockOverrideTtl = Prelude.Nothing,
        blockResponse = Prelude.Nothing,
        creatorRequestId = pCreatorRequestId_,
        firewallRuleGroupId = pFirewallRuleGroupId_,
        firewallDomainListId = pFirewallDomainListId_,
        priority = pPriority_,
        action = pAction_,
        name = pName_
      }

-- | The DNS record\'s type. This determines the format of the record value
-- that you provided in @BlockOverrideDomain@. Used for the rule action
-- @BLOCK@ with a @BlockResponse@ setting of @OVERRIDE@.
--
-- This setting is required if the @BlockResponse@ setting is @OVERRIDE@.
createFirewallRule_blockOverrideDnsType :: Lens.Lens' CreateFirewallRule (Prelude.Maybe BlockOverrideDnsType)
createFirewallRule_blockOverrideDnsType = Lens.lens (\CreateFirewallRule' {blockOverrideDnsType} -> blockOverrideDnsType) (\s@CreateFirewallRule' {} a -> s {blockOverrideDnsType = a} :: CreateFirewallRule)

-- | The custom DNS record to send back in response to the query. Used for
-- the rule action @BLOCK@ with a @BlockResponse@ setting of @OVERRIDE@.
--
-- This setting is required if the @BlockResponse@ setting is @OVERRIDE@.
createFirewallRule_blockOverrideDomain :: Lens.Lens' CreateFirewallRule (Prelude.Maybe Prelude.Text)
createFirewallRule_blockOverrideDomain = Lens.lens (\CreateFirewallRule' {blockOverrideDomain} -> blockOverrideDomain) (\s@CreateFirewallRule' {} a -> s {blockOverrideDomain = a} :: CreateFirewallRule)

-- | The recommended amount of time, in seconds, for the DNS resolver or web
-- browser to cache the provided override record. Used for the rule action
-- @BLOCK@ with a @BlockResponse@ setting of @OVERRIDE@.
--
-- This setting is required if the @BlockResponse@ setting is @OVERRIDE@.
createFirewallRule_blockOverrideTtl :: Lens.Lens' CreateFirewallRule (Prelude.Maybe Prelude.Natural)
createFirewallRule_blockOverrideTtl = Lens.lens (\CreateFirewallRule' {blockOverrideTtl} -> blockOverrideTtl) (\s@CreateFirewallRule' {} a -> s {blockOverrideTtl = a} :: CreateFirewallRule)

-- | The way that you want DNS Firewall to block the request, used with the
-- rule action setting @BLOCK@.
--
-- -   @NODATA@ - Respond indicating that the query was successful, but no
--     response is available for it.
--
-- -   @NXDOMAIN@ - Respond indicating that the domain name that\'s in the
--     query doesn\'t exist.
--
-- -   @OVERRIDE@ - Provide a custom override in the response. This option
--     requires custom handling details in the rule\'s @BlockOverride*@
--     settings.
--
-- This setting is required if the rule action setting is @BLOCK@.
createFirewallRule_blockResponse :: Lens.Lens' CreateFirewallRule (Prelude.Maybe BlockResponse)
createFirewallRule_blockResponse = Lens.lens (\CreateFirewallRule' {blockResponse} -> blockResponse) (\s@CreateFirewallRule' {} a -> s {blockResponse = a} :: CreateFirewallRule)

-- | A unique string that identifies the request and that allows you to retry
-- failed requests without the risk of running the operation twice.
-- @CreatorRequestId@ can be any unique string, for example, a date\/time
-- stamp.
createFirewallRule_creatorRequestId :: Lens.Lens' CreateFirewallRule Prelude.Text
createFirewallRule_creatorRequestId = Lens.lens (\CreateFirewallRule' {creatorRequestId} -> creatorRequestId) (\s@CreateFirewallRule' {} a -> s {creatorRequestId = a} :: CreateFirewallRule)

-- | The unique identifier of the firewall rule group where you want to
-- create the rule.
createFirewallRule_firewallRuleGroupId :: Lens.Lens' CreateFirewallRule Prelude.Text
createFirewallRule_firewallRuleGroupId = Lens.lens (\CreateFirewallRule' {firewallRuleGroupId} -> firewallRuleGroupId) (\s@CreateFirewallRule' {} a -> s {firewallRuleGroupId = a} :: CreateFirewallRule)

-- | The ID of the domain list that you want to use in the rule.
createFirewallRule_firewallDomainListId :: Lens.Lens' CreateFirewallRule Prelude.Text
createFirewallRule_firewallDomainListId = Lens.lens (\CreateFirewallRule' {firewallDomainListId} -> firewallDomainListId) (\s@CreateFirewallRule' {} a -> s {firewallDomainListId = a} :: CreateFirewallRule)

-- | The setting that determines the processing order of the rule in the rule
-- group. DNS Firewall processes the rules in a rule group by order of
-- priority, starting from the lowest setting.
--
-- You must specify a unique priority for each rule in a rule group. To
-- make it easier to insert rules later, leave space between the numbers,
-- for example, use 100, 200, and so on. You can change the priority
-- setting for the rules in a rule group at any time.
createFirewallRule_priority :: Lens.Lens' CreateFirewallRule Prelude.Int
createFirewallRule_priority = Lens.lens (\CreateFirewallRule' {priority} -> priority) (\s@CreateFirewallRule' {} a -> s {priority = a} :: CreateFirewallRule)

-- | The action that DNS Firewall should take on a DNS query when it matches
-- one of the domains in the rule\'s domain list:
--
-- -   @ALLOW@ - Permit the request to go through.
--
-- -   @ALERT@ - Permit the request and send metrics and logs to Cloud
--     Watch.
--
-- -   @BLOCK@ - Disallow the request. This option requires additional
--     details in the rule\'s @BlockResponse@.
createFirewallRule_action :: Lens.Lens' CreateFirewallRule Action
createFirewallRule_action = Lens.lens (\CreateFirewallRule' {action} -> action) (\s@CreateFirewallRule' {} a -> s {action = a} :: CreateFirewallRule)

-- | A name that lets you identify the rule in the rule group.
createFirewallRule_name :: Lens.Lens' CreateFirewallRule Prelude.Text
createFirewallRule_name = Lens.lens (\CreateFirewallRule' {name} -> name) (\s@CreateFirewallRule' {} a -> s {name = a} :: CreateFirewallRule)

instance Core.AWSRequest CreateFirewallRule where
  type
    AWSResponse CreateFirewallRule =
      CreateFirewallRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFirewallRuleResponse'
            Prelude.<$> (x Data..?> "FirewallRule")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFirewallRule where
  hashWithSalt _salt CreateFirewallRule' {..} =
    _salt
      `Prelude.hashWithSalt` blockOverrideDnsType
      `Prelude.hashWithSalt` blockOverrideDomain
      `Prelude.hashWithSalt` blockOverrideTtl
      `Prelude.hashWithSalt` blockResponse
      `Prelude.hashWithSalt` creatorRequestId
      `Prelude.hashWithSalt` firewallRuleGroupId
      `Prelude.hashWithSalt` firewallDomainListId
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateFirewallRule where
  rnf CreateFirewallRule' {..} =
    Prelude.rnf blockOverrideDnsType
      `Prelude.seq` Prelude.rnf blockOverrideDomain
      `Prelude.seq` Prelude.rnf blockOverrideTtl
      `Prelude.seq` Prelude.rnf blockResponse
      `Prelude.seq` Prelude.rnf creatorRequestId
      `Prelude.seq` Prelude.rnf firewallRuleGroupId
      `Prelude.seq` Prelude.rnf firewallDomainListId
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateFirewallRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.CreateFirewallRule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateFirewallRule where
  toJSON CreateFirewallRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BlockOverrideDnsType" Data..=)
              Prelude.<$> blockOverrideDnsType,
            ("BlockOverrideDomain" Data..=)
              Prelude.<$> blockOverrideDomain,
            ("BlockOverrideTtl" Data..=)
              Prelude.<$> blockOverrideTtl,
            ("BlockResponse" Data..=) Prelude.<$> blockResponse,
            Prelude.Just
              ("CreatorRequestId" Data..= creatorRequestId),
            Prelude.Just
              ("FirewallRuleGroupId" Data..= firewallRuleGroupId),
            Prelude.Just
              ( "FirewallDomainListId"
                  Data..= firewallDomainListId
              ),
            Prelude.Just ("Priority" Data..= priority),
            Prelude.Just ("Action" Data..= action),
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateFirewallRule where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateFirewallRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFirewallRuleResponse' smart constructor.
data CreateFirewallRuleResponse = CreateFirewallRuleResponse'
  { -- | The firewall rule that you just created.
    firewallRule :: Prelude.Maybe FirewallRule,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFirewallRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallRule', 'createFirewallRuleResponse_firewallRule' - The firewall rule that you just created.
--
-- 'httpStatus', 'createFirewallRuleResponse_httpStatus' - The response's http status code.
newCreateFirewallRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFirewallRuleResponse
newCreateFirewallRuleResponse pHttpStatus_ =
  CreateFirewallRuleResponse'
    { firewallRule =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The firewall rule that you just created.
createFirewallRuleResponse_firewallRule :: Lens.Lens' CreateFirewallRuleResponse (Prelude.Maybe FirewallRule)
createFirewallRuleResponse_firewallRule = Lens.lens (\CreateFirewallRuleResponse' {firewallRule} -> firewallRule) (\s@CreateFirewallRuleResponse' {} a -> s {firewallRule = a} :: CreateFirewallRuleResponse)

-- | The response's http status code.
createFirewallRuleResponse_httpStatus :: Lens.Lens' CreateFirewallRuleResponse Prelude.Int
createFirewallRuleResponse_httpStatus = Lens.lens (\CreateFirewallRuleResponse' {httpStatus} -> httpStatus) (\s@CreateFirewallRuleResponse' {} a -> s {httpStatus = a} :: CreateFirewallRuleResponse)

instance Prelude.NFData CreateFirewallRuleResponse where
  rnf CreateFirewallRuleResponse' {..} =
    Prelude.rnf firewallRule
      `Prelude.seq` Prelude.rnf httpStatus
