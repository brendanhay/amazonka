{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newPutAccessControlRule' smart constructor.
data PutAccessControlRule = PutAccessControlRule'
  { -- | IPv4 CIDR ranges to exclude from the rule.
    notIpRanges :: Prelude.Maybe [Prelude.Text],
    -- | IPv4 CIDR ranges to include in the rule.
    ipRanges :: Prelude.Maybe [Prelude.Text],
    -- | Access protocol actions to include in the rule. Valid values include
    -- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
    -- and @WebMail@.
    actions :: Prelude.Maybe [Prelude.Text],
    -- | User IDs to include in the rule.
    userIds :: Prelude.Maybe [Prelude.Text],
    -- | Access protocol actions to exclude from the rule. Valid values include
    -- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
    -- and @WebMail@.
    notActions :: Prelude.Maybe [Prelude.Text],
    -- | User IDs to exclude from the rule.
    notUserIds :: Prelude.Maybe [Prelude.Text],
    -- | The rule name.
    name :: Prelude.Text,
    -- | The rule effect.
    effect :: AccessControlRuleEffect,
    -- | The rule description.
    description :: Prelude.Text,
    -- | The identifier of the organization.
    organizationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        ipRanges = Prelude.Nothing,
        actions = Prelude.Nothing,
        userIds = Prelude.Nothing,
        notActions = Prelude.Nothing,
        notUserIds = Prelude.Nothing,
        name = pName_,
        effect = pEffect_,
        description = pDescription_,
        organizationId = pOrganizationId_
      }

-- | IPv4 CIDR ranges to exclude from the rule.
putAccessControlRule_notIpRanges :: Lens.Lens' PutAccessControlRule (Prelude.Maybe [Prelude.Text])
putAccessControlRule_notIpRanges = Lens.lens (\PutAccessControlRule' {notIpRanges} -> notIpRanges) (\s@PutAccessControlRule' {} a -> s {notIpRanges = a} :: PutAccessControlRule) Prelude.. Lens.mapping Prelude._Coerce

-- | IPv4 CIDR ranges to include in the rule.
putAccessControlRule_ipRanges :: Lens.Lens' PutAccessControlRule (Prelude.Maybe [Prelude.Text])
putAccessControlRule_ipRanges = Lens.lens (\PutAccessControlRule' {ipRanges} -> ipRanges) (\s@PutAccessControlRule' {} a -> s {ipRanges = a} :: PutAccessControlRule) Prelude.. Lens.mapping Prelude._Coerce

-- | Access protocol actions to include in the rule. Valid values include
-- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
-- and @WebMail@.
putAccessControlRule_actions :: Lens.Lens' PutAccessControlRule (Prelude.Maybe [Prelude.Text])
putAccessControlRule_actions = Lens.lens (\PutAccessControlRule' {actions} -> actions) (\s@PutAccessControlRule' {} a -> s {actions = a} :: PutAccessControlRule) Prelude.. Lens.mapping Prelude._Coerce

-- | User IDs to include in the rule.
putAccessControlRule_userIds :: Lens.Lens' PutAccessControlRule (Prelude.Maybe [Prelude.Text])
putAccessControlRule_userIds = Lens.lens (\PutAccessControlRule' {userIds} -> userIds) (\s@PutAccessControlRule' {} a -> s {userIds = a} :: PutAccessControlRule) Prelude.. Lens.mapping Prelude._Coerce

-- | Access protocol actions to exclude from the rule. Valid values include
-- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
-- and @WebMail@.
putAccessControlRule_notActions :: Lens.Lens' PutAccessControlRule (Prelude.Maybe [Prelude.Text])
putAccessControlRule_notActions = Lens.lens (\PutAccessControlRule' {notActions} -> notActions) (\s@PutAccessControlRule' {} a -> s {notActions = a} :: PutAccessControlRule) Prelude.. Lens.mapping Prelude._Coerce

-- | User IDs to exclude from the rule.
putAccessControlRule_notUserIds :: Lens.Lens' PutAccessControlRule (Prelude.Maybe [Prelude.Text])
putAccessControlRule_notUserIds = Lens.lens (\PutAccessControlRule' {notUserIds} -> notUserIds) (\s@PutAccessControlRule' {} a -> s {notUserIds = a} :: PutAccessControlRule) Prelude.. Lens.mapping Prelude._Coerce

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

instance Prelude.AWSRequest PutAccessControlRule where
  type
    Rs PutAccessControlRule =
      PutAccessControlRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutAccessControlRuleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutAccessControlRule

instance Prelude.NFData PutAccessControlRule

instance Prelude.ToHeaders PutAccessControlRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkMailService.PutAccessControlRule" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PutAccessControlRule where
  toJSON PutAccessControlRule' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NotIpRanges" Prelude..=) Prelude.<$> notIpRanges,
            ("IpRanges" Prelude..=) Prelude.<$> ipRanges,
            ("Actions" Prelude..=) Prelude.<$> actions,
            ("UserIds" Prelude..=) Prelude.<$> userIds,
            ("NotActions" Prelude..=) Prelude.<$> notActions,
            ("NotUserIds" Prelude..=) Prelude.<$> notUserIds,
            Prelude.Just ("Name" Prelude..= name),
            Prelude.Just ("Effect" Prelude..= effect),
            Prelude.Just ("Description" Prelude..= description),
            Prelude.Just
              ("OrganizationId" Prelude..= organizationId)
          ]
      )

instance Prelude.ToPath PutAccessControlRule where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutAccessControlRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutAccessControlRuleResponse' smart constructor.
data PutAccessControlRuleResponse = PutAccessControlRuleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData PutAccessControlRuleResponse
