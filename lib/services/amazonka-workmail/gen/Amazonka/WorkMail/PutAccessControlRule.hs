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
-- Module      : Amazonka.WorkMail.PutAccessControlRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new access control rule for the specified organization. The rule
-- allows or denies access to the organization for the specified IPv4
-- addresses, access protocol actions, user IDs and impersonation IDs.
-- Adding a new rule with the same name as an existing rule replaces the
-- older rule.
module Amazonka.WorkMail.PutAccessControlRule
  ( -- * Creating a Request
    PutAccessControlRule (..),
    newPutAccessControlRule,

    -- * Request Lenses
    putAccessControlRule_notIpRanges,
    putAccessControlRule_notActions,
    putAccessControlRule_ipRanges,
    putAccessControlRule_notImpersonationRoleIds,
    putAccessControlRule_userIds,
    putAccessControlRule_impersonationRoleIds,
    putAccessControlRule_notUserIds,
    putAccessControlRule_actions,
    putAccessControlRule_name,
    putAccessControlRule_effect,
    putAccessControlRule_description,
    putAccessControlRule_organizationId,

    -- * Destructuring the Response
    PutAccessControlRuleResponse (..),
    newPutAccessControlRuleResponse,

    -- * Response Lenses
    putAccessControlRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newPutAccessControlRule' smart constructor.
data PutAccessControlRule = PutAccessControlRule'
  { -- | IPv4 CIDR ranges to exclude from the rule.
    notIpRanges :: Prelude.Maybe [Prelude.Text],
    -- | Access protocol actions to exclude from the rule. Valid values include
    -- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
    -- and @WebMail@.
    notActions :: Prelude.Maybe [Prelude.Text],
    -- | IPv4 CIDR ranges to include in the rule.
    ipRanges :: Prelude.Maybe [Prelude.Text],
    -- | Impersonation role IDs to exclude from the rule.
    notImpersonationRoleIds :: Prelude.Maybe [Prelude.Text],
    -- | User IDs to include in the rule.
    userIds :: Prelude.Maybe [Prelude.Text],
    -- | Impersonation role IDs to include in the rule.
    impersonationRoleIds :: Prelude.Maybe [Prelude.Text],
    -- | User IDs to exclude from the rule.
    notUserIds :: Prelude.Maybe [Prelude.Text],
    -- | Access protocol actions to include in the rule. Valid values include
    -- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
    -- and @WebMail@.
    actions :: Prelude.Maybe [Prelude.Text],
    -- | The rule name.
    name :: Prelude.Text,
    -- | The rule effect.
    effect :: AccessControlRuleEffect,
    -- | The rule description.
    description :: Prelude.Text,
    -- | The identifier of the organization.
    organizationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAccessControlRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notIpRanges', 'putAccessControlRule_notIpRanges' - IPv4 CIDR ranges to exclude from the rule.
--
-- 'notActions', 'putAccessControlRule_notActions' - Access protocol actions to exclude from the rule. Valid values include
-- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
-- and @WebMail@.
--
-- 'ipRanges', 'putAccessControlRule_ipRanges' - IPv4 CIDR ranges to include in the rule.
--
-- 'notImpersonationRoleIds', 'putAccessControlRule_notImpersonationRoleIds' - Impersonation role IDs to exclude from the rule.
--
-- 'userIds', 'putAccessControlRule_userIds' - User IDs to include in the rule.
--
-- 'impersonationRoleIds', 'putAccessControlRule_impersonationRoleIds' - Impersonation role IDs to include in the rule.
--
-- 'notUserIds', 'putAccessControlRule_notUserIds' - User IDs to exclude from the rule.
--
-- 'actions', 'putAccessControlRule_actions' - Access protocol actions to include in the rule. Valid values include
-- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
-- and @WebMail@.
--
-- 'name', 'putAccessControlRule_name' - The rule name.
--
-- 'effect', 'putAccessControlRule_effect' - The rule effect.
--
-- 'description', 'putAccessControlRule_description' - The rule description.
--
-- 'organizationId', 'putAccessControlRule_organizationId' - The identifier of the organization.
newPutAccessControlRule ::
  -- | 'name'
  Prelude.Text ->
  -- | 'effect'
  AccessControlRuleEffect ->
  -- | 'description'
  Prelude.Text ->
  -- | 'organizationId'
  Prelude.Text ->
  PutAccessControlRule
newPutAccessControlRule
  pName_
  pEffect_
  pDescription_
  pOrganizationId_ =
    PutAccessControlRule'
      { notIpRanges =
          Prelude.Nothing,
        notActions = Prelude.Nothing,
        ipRanges = Prelude.Nothing,
        notImpersonationRoleIds = Prelude.Nothing,
        userIds = Prelude.Nothing,
        impersonationRoleIds = Prelude.Nothing,
        notUserIds = Prelude.Nothing,
        actions = Prelude.Nothing,
        name = pName_,
        effect = pEffect_,
        description = pDescription_,
        organizationId = pOrganizationId_
      }

-- | IPv4 CIDR ranges to exclude from the rule.
putAccessControlRule_notIpRanges :: Lens.Lens' PutAccessControlRule (Prelude.Maybe [Prelude.Text])
putAccessControlRule_notIpRanges = Lens.lens (\PutAccessControlRule' {notIpRanges} -> notIpRanges) (\s@PutAccessControlRule' {} a -> s {notIpRanges = a} :: PutAccessControlRule) Prelude.. Lens.mapping Lens.coerced

-- | Access protocol actions to exclude from the rule. Valid values include
-- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
-- and @WebMail@.
putAccessControlRule_notActions :: Lens.Lens' PutAccessControlRule (Prelude.Maybe [Prelude.Text])
putAccessControlRule_notActions = Lens.lens (\PutAccessControlRule' {notActions} -> notActions) (\s@PutAccessControlRule' {} a -> s {notActions = a} :: PutAccessControlRule) Prelude.. Lens.mapping Lens.coerced

-- | IPv4 CIDR ranges to include in the rule.
putAccessControlRule_ipRanges :: Lens.Lens' PutAccessControlRule (Prelude.Maybe [Prelude.Text])
putAccessControlRule_ipRanges = Lens.lens (\PutAccessControlRule' {ipRanges} -> ipRanges) (\s@PutAccessControlRule' {} a -> s {ipRanges = a} :: PutAccessControlRule) Prelude.. Lens.mapping Lens.coerced

-- | Impersonation role IDs to exclude from the rule.
putAccessControlRule_notImpersonationRoleIds :: Lens.Lens' PutAccessControlRule (Prelude.Maybe [Prelude.Text])
putAccessControlRule_notImpersonationRoleIds = Lens.lens (\PutAccessControlRule' {notImpersonationRoleIds} -> notImpersonationRoleIds) (\s@PutAccessControlRule' {} a -> s {notImpersonationRoleIds = a} :: PutAccessControlRule) Prelude.. Lens.mapping Lens.coerced

-- | User IDs to include in the rule.
putAccessControlRule_userIds :: Lens.Lens' PutAccessControlRule (Prelude.Maybe [Prelude.Text])
putAccessControlRule_userIds = Lens.lens (\PutAccessControlRule' {userIds} -> userIds) (\s@PutAccessControlRule' {} a -> s {userIds = a} :: PutAccessControlRule) Prelude.. Lens.mapping Lens.coerced

-- | Impersonation role IDs to include in the rule.
putAccessControlRule_impersonationRoleIds :: Lens.Lens' PutAccessControlRule (Prelude.Maybe [Prelude.Text])
putAccessControlRule_impersonationRoleIds = Lens.lens (\PutAccessControlRule' {impersonationRoleIds} -> impersonationRoleIds) (\s@PutAccessControlRule' {} a -> s {impersonationRoleIds = a} :: PutAccessControlRule) Prelude.. Lens.mapping Lens.coerced

-- | User IDs to exclude from the rule.
putAccessControlRule_notUserIds :: Lens.Lens' PutAccessControlRule (Prelude.Maybe [Prelude.Text])
putAccessControlRule_notUserIds = Lens.lens (\PutAccessControlRule' {notUserIds} -> notUserIds) (\s@PutAccessControlRule' {} a -> s {notUserIds = a} :: PutAccessControlRule) Prelude.. Lens.mapping Lens.coerced

-- | Access protocol actions to include in the rule. Valid values include
-- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
-- and @WebMail@.
putAccessControlRule_actions :: Lens.Lens' PutAccessControlRule (Prelude.Maybe [Prelude.Text])
putAccessControlRule_actions = Lens.lens (\PutAccessControlRule' {actions} -> actions) (\s@PutAccessControlRule' {} a -> s {actions = a} :: PutAccessControlRule) Prelude.. Lens.mapping Lens.coerced

-- | The rule name.
putAccessControlRule_name :: Lens.Lens' PutAccessControlRule Prelude.Text
putAccessControlRule_name = Lens.lens (\PutAccessControlRule' {name} -> name) (\s@PutAccessControlRule' {} a -> s {name = a} :: PutAccessControlRule)

-- | The rule effect.
putAccessControlRule_effect :: Lens.Lens' PutAccessControlRule AccessControlRuleEffect
putAccessControlRule_effect = Lens.lens (\PutAccessControlRule' {effect} -> effect) (\s@PutAccessControlRule' {} a -> s {effect = a} :: PutAccessControlRule)

-- | The rule description.
putAccessControlRule_description :: Lens.Lens' PutAccessControlRule Prelude.Text
putAccessControlRule_description = Lens.lens (\PutAccessControlRule' {description} -> description) (\s@PutAccessControlRule' {} a -> s {description = a} :: PutAccessControlRule)

-- | The identifier of the organization.
putAccessControlRule_organizationId :: Lens.Lens' PutAccessControlRule Prelude.Text
putAccessControlRule_organizationId = Lens.lens (\PutAccessControlRule' {organizationId} -> organizationId) (\s@PutAccessControlRule' {} a -> s {organizationId = a} :: PutAccessControlRule)

instance Core.AWSRequest PutAccessControlRule where
  type
    AWSResponse PutAccessControlRule =
      PutAccessControlRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutAccessControlRuleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutAccessControlRule where
  hashWithSalt _salt PutAccessControlRule' {..} =
    _salt `Prelude.hashWithSalt` notIpRanges
      `Prelude.hashWithSalt` notActions
      `Prelude.hashWithSalt` ipRanges
      `Prelude.hashWithSalt` notImpersonationRoleIds
      `Prelude.hashWithSalt` userIds
      `Prelude.hashWithSalt` impersonationRoleIds
      `Prelude.hashWithSalt` notUserIds
      `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` effect
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` organizationId

instance Prelude.NFData PutAccessControlRule where
  rnf PutAccessControlRule' {..} =
    Prelude.rnf notIpRanges
      `Prelude.seq` Prelude.rnf notActions
      `Prelude.seq` Prelude.rnf ipRanges
      `Prelude.seq` Prelude.rnf notImpersonationRoleIds
      `Prelude.seq` Prelude.rnf userIds
      `Prelude.seq` Prelude.rnf impersonationRoleIds
      `Prelude.seq` Prelude.rnf notUserIds
      `Prelude.seq` Prelude.rnf actions
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf effect
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf organizationId

instance Data.ToHeaders PutAccessControlRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.PutAccessControlRule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutAccessControlRule where
  toJSON PutAccessControlRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NotIpRanges" Data..=) Prelude.<$> notIpRanges,
            ("NotActions" Data..=) Prelude.<$> notActions,
            ("IpRanges" Data..=) Prelude.<$> ipRanges,
            ("NotImpersonationRoleIds" Data..=)
              Prelude.<$> notImpersonationRoleIds,
            ("UserIds" Data..=) Prelude.<$> userIds,
            ("ImpersonationRoleIds" Data..=)
              Prelude.<$> impersonationRoleIds,
            ("NotUserIds" Data..=) Prelude.<$> notUserIds,
            ("Actions" Data..=) Prelude.<$> actions,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Effect" Data..= effect),
            Prelude.Just ("Description" Data..= description),
            Prelude.Just
              ("OrganizationId" Data..= organizationId)
          ]
      )

instance Data.ToPath PutAccessControlRule where
  toPath = Prelude.const "/"

instance Data.ToQuery PutAccessControlRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutAccessControlRuleResponse' smart constructor.
data PutAccessControlRuleResponse = PutAccessControlRuleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAccessControlRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putAccessControlRuleResponse_httpStatus' - The response's http status code.
newPutAccessControlRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutAccessControlRuleResponse
newPutAccessControlRuleResponse pHttpStatus_ =
  PutAccessControlRuleResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putAccessControlRuleResponse_httpStatus :: Lens.Lens' PutAccessControlRuleResponse Prelude.Int
putAccessControlRuleResponse_httpStatus = Lens.lens (\PutAccessControlRuleResponse' {httpStatus} -> httpStatus) (\s@PutAccessControlRuleResponse' {} a -> s {httpStatus = a} :: PutAccessControlRuleResponse)

instance Prelude.NFData PutAccessControlRuleResponse where
  rnf PutAccessControlRuleResponse' {..} =
    Prelude.rnf httpStatus
