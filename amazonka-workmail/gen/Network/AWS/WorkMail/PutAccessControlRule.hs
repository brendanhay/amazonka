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
-- Module      : Network.AWS.WorkMail.PutAccessControlRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new access control rule for the specified organization. The rule
-- allows or denies access to the organization for the specified IPv4
-- addresses, access protocol actions, and user IDs. Adding a new rule with
-- the same name as an existing rule replaces the older rule.
module Network.AWS.WorkMail.PutAccessControlRule
  ( -- * Creating a Request
    PutAccessControlRule (..),
    newPutAccessControlRule,

    -- * Request Lenses
    putAccessControlRule_notIpRanges,
    putAccessControlRule_ipRanges,
    putAccessControlRule_actions,
    putAccessControlRule_userIds,
    putAccessControlRule_notActions,
    putAccessControlRule_notUserIds,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newPutAccessControlRule' smart constructor.
data PutAccessControlRule = PutAccessControlRule'
  { -- | IPv4 CIDR ranges to exclude from the rule.
    notIpRanges :: Core.Maybe [Core.Text],
    -- | IPv4 CIDR ranges to include in the rule.
    ipRanges :: Core.Maybe [Core.Text],
    -- | Access protocol actions to include in the rule. Valid values include
    -- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
    -- and @WebMail@.
    actions :: Core.Maybe [Core.Text],
    -- | User IDs to include in the rule.
    userIds :: Core.Maybe [Core.Text],
    -- | Access protocol actions to exclude from the rule. Valid values include
    -- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
    -- and @WebMail@.
    notActions :: Core.Maybe [Core.Text],
    -- | User IDs to exclude from the rule.
    notUserIds :: Core.Maybe [Core.Text],
    -- | The rule name.
    name :: Core.Text,
    -- | The rule effect.
    effect :: AccessControlRuleEffect,
    -- | The rule description.
    description :: Core.Text,
    -- | The identifier of the organization.
    organizationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'ipRanges', 'putAccessControlRule_ipRanges' - IPv4 CIDR ranges to include in the rule.
--
-- 'actions', 'putAccessControlRule_actions' - Access protocol actions to include in the rule. Valid values include
-- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
-- and @WebMail@.
--
-- 'userIds', 'putAccessControlRule_userIds' - User IDs to include in the rule.
--
-- 'notActions', 'putAccessControlRule_notActions' - Access protocol actions to exclude from the rule. Valid values include
-- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
-- and @WebMail@.
--
-- 'notUserIds', 'putAccessControlRule_notUserIds' - User IDs to exclude from the rule.
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
  Core.Text ->
  -- | 'effect'
  AccessControlRuleEffect ->
  -- | 'description'
  Core.Text ->
  -- | 'organizationId'
  Core.Text ->
  PutAccessControlRule
newPutAccessControlRule
  pName_
  pEffect_
  pDescription_
  pOrganizationId_ =
    PutAccessControlRule'
      { notIpRanges = Core.Nothing,
        ipRanges = Core.Nothing,
        actions = Core.Nothing,
        userIds = Core.Nothing,
        notActions = Core.Nothing,
        notUserIds = Core.Nothing,
        name = pName_,
        effect = pEffect_,
        description = pDescription_,
        organizationId = pOrganizationId_
      }

-- | IPv4 CIDR ranges to exclude from the rule.
putAccessControlRule_notIpRanges :: Lens.Lens' PutAccessControlRule (Core.Maybe [Core.Text])
putAccessControlRule_notIpRanges = Lens.lens (\PutAccessControlRule' {notIpRanges} -> notIpRanges) (\s@PutAccessControlRule' {} a -> s {notIpRanges = a} :: PutAccessControlRule) Core.. Lens.mapping Lens._Coerce

-- | IPv4 CIDR ranges to include in the rule.
putAccessControlRule_ipRanges :: Lens.Lens' PutAccessControlRule (Core.Maybe [Core.Text])
putAccessControlRule_ipRanges = Lens.lens (\PutAccessControlRule' {ipRanges} -> ipRanges) (\s@PutAccessControlRule' {} a -> s {ipRanges = a} :: PutAccessControlRule) Core.. Lens.mapping Lens._Coerce

-- | Access protocol actions to include in the rule. Valid values include
-- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
-- and @WebMail@.
putAccessControlRule_actions :: Lens.Lens' PutAccessControlRule (Core.Maybe [Core.Text])
putAccessControlRule_actions = Lens.lens (\PutAccessControlRule' {actions} -> actions) (\s@PutAccessControlRule' {} a -> s {actions = a} :: PutAccessControlRule) Core.. Lens.mapping Lens._Coerce

-- | User IDs to include in the rule.
putAccessControlRule_userIds :: Lens.Lens' PutAccessControlRule (Core.Maybe [Core.Text])
putAccessControlRule_userIds = Lens.lens (\PutAccessControlRule' {userIds} -> userIds) (\s@PutAccessControlRule' {} a -> s {userIds = a} :: PutAccessControlRule) Core.. Lens.mapping Lens._Coerce

-- | Access protocol actions to exclude from the rule. Valid values include
-- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
-- and @WebMail@.
putAccessControlRule_notActions :: Lens.Lens' PutAccessControlRule (Core.Maybe [Core.Text])
putAccessControlRule_notActions = Lens.lens (\PutAccessControlRule' {notActions} -> notActions) (\s@PutAccessControlRule' {} a -> s {notActions = a} :: PutAccessControlRule) Core.. Lens.mapping Lens._Coerce

-- | User IDs to exclude from the rule.
putAccessControlRule_notUserIds :: Lens.Lens' PutAccessControlRule (Core.Maybe [Core.Text])
putAccessControlRule_notUserIds = Lens.lens (\PutAccessControlRule' {notUserIds} -> notUserIds) (\s@PutAccessControlRule' {} a -> s {notUserIds = a} :: PutAccessControlRule) Core.. Lens.mapping Lens._Coerce

-- | The rule name.
putAccessControlRule_name :: Lens.Lens' PutAccessControlRule Core.Text
putAccessControlRule_name = Lens.lens (\PutAccessControlRule' {name} -> name) (\s@PutAccessControlRule' {} a -> s {name = a} :: PutAccessControlRule)

-- | The rule effect.
putAccessControlRule_effect :: Lens.Lens' PutAccessControlRule AccessControlRuleEffect
putAccessControlRule_effect = Lens.lens (\PutAccessControlRule' {effect} -> effect) (\s@PutAccessControlRule' {} a -> s {effect = a} :: PutAccessControlRule)

-- | The rule description.
putAccessControlRule_description :: Lens.Lens' PutAccessControlRule Core.Text
putAccessControlRule_description = Lens.lens (\PutAccessControlRule' {description} -> description) (\s@PutAccessControlRule' {} a -> s {description = a} :: PutAccessControlRule)

-- | The identifier of the organization.
putAccessControlRule_organizationId :: Lens.Lens' PutAccessControlRule Core.Text
putAccessControlRule_organizationId = Lens.lens (\PutAccessControlRule' {organizationId} -> organizationId) (\s@PutAccessControlRule' {} a -> s {organizationId = a} :: PutAccessControlRule)

instance Core.AWSRequest PutAccessControlRule where
  type
    AWSResponse PutAccessControlRule =
      PutAccessControlRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutAccessControlRuleResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutAccessControlRule

instance Core.NFData PutAccessControlRule

instance Core.ToHeaders PutAccessControlRule where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.PutAccessControlRule" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutAccessControlRule where
  toJSON PutAccessControlRule' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NotIpRanges" Core..=) Core.<$> notIpRanges,
            ("IpRanges" Core..=) Core.<$> ipRanges,
            ("Actions" Core..=) Core.<$> actions,
            ("UserIds" Core..=) Core.<$> userIds,
            ("NotActions" Core..=) Core.<$> notActions,
            ("NotUserIds" Core..=) Core.<$> notUserIds,
            Core.Just ("Name" Core..= name),
            Core.Just ("Effect" Core..= effect),
            Core.Just ("Description" Core..= description),
            Core.Just ("OrganizationId" Core..= organizationId)
          ]
      )

instance Core.ToPath PutAccessControlRule where
  toPath = Core.const "/"

instance Core.ToQuery PutAccessControlRule where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutAccessControlRuleResponse' smart constructor.
data PutAccessControlRuleResponse = PutAccessControlRuleResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  PutAccessControlRuleResponse
newPutAccessControlRuleResponse pHttpStatus_ =
  PutAccessControlRuleResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putAccessControlRuleResponse_httpStatus :: Lens.Lens' PutAccessControlRuleResponse Core.Int
putAccessControlRuleResponse_httpStatus = Lens.lens (\PutAccessControlRuleResponse' {httpStatus} -> httpStatus) (\s@PutAccessControlRuleResponse' {} a -> s {httpStatus = a} :: PutAccessControlRuleResponse)

instance Core.NFData PutAccessControlRuleResponse
