{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WorkMail.Types.AccessControlRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types.AccessControlRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkMail.Types.AccessControlRuleEffect

-- | A rule that controls access to an WorkMail organization.
--
-- /See:/ 'newAccessControlRule' smart constructor.
data AccessControlRule = AccessControlRule'
  { -- | Access protocol actions to include in the rule. Valid values include
    -- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
    -- and @WebMail@.
    actions :: Prelude.Maybe [Prelude.Text],
    -- | The date that the rule was created.
    dateCreated :: Prelude.Maybe Data.POSIX,
    -- | The date that the rule was modified.
    dateModified :: Prelude.Maybe Data.POSIX,
    -- | The rule description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The rule effect.
    effect :: Prelude.Maybe AccessControlRuleEffect,
    -- | Impersonation role IDs to include in the rule.
    impersonationRoleIds :: Prelude.Maybe [Prelude.Text],
    -- | IPv4 CIDR ranges to include in the rule.
    ipRanges :: Prelude.Maybe [Prelude.Text],
    -- | The rule name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Access protocol actions to exclude from the rule. Valid values include
    -- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
    -- and @WebMail@.
    notActions :: Prelude.Maybe [Prelude.Text],
    -- | Impersonation role IDs to exclude from the rule.
    notImpersonationRoleIds :: Prelude.Maybe [Prelude.Text],
    -- | IPv4 CIDR ranges to exclude from the rule.
    notIpRanges :: Prelude.Maybe [Prelude.Text],
    -- | User IDs to exclude from the rule.
    notUserIds :: Prelude.Maybe [Prelude.Text],
    -- | User IDs to include in the rule.
    userIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccessControlRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'accessControlRule_actions' - Access protocol actions to include in the rule. Valid values include
-- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
-- and @WebMail@.
--
-- 'dateCreated', 'accessControlRule_dateCreated' - The date that the rule was created.
--
-- 'dateModified', 'accessControlRule_dateModified' - The date that the rule was modified.
--
-- 'description', 'accessControlRule_description' - The rule description.
--
-- 'effect', 'accessControlRule_effect' - The rule effect.
--
-- 'impersonationRoleIds', 'accessControlRule_impersonationRoleIds' - Impersonation role IDs to include in the rule.
--
-- 'ipRanges', 'accessControlRule_ipRanges' - IPv4 CIDR ranges to include in the rule.
--
-- 'name', 'accessControlRule_name' - The rule name.
--
-- 'notActions', 'accessControlRule_notActions' - Access protocol actions to exclude from the rule. Valid values include
-- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
-- and @WebMail@.
--
-- 'notImpersonationRoleIds', 'accessControlRule_notImpersonationRoleIds' - Impersonation role IDs to exclude from the rule.
--
-- 'notIpRanges', 'accessControlRule_notIpRanges' - IPv4 CIDR ranges to exclude from the rule.
--
-- 'notUserIds', 'accessControlRule_notUserIds' - User IDs to exclude from the rule.
--
-- 'userIds', 'accessControlRule_userIds' - User IDs to include in the rule.
newAccessControlRule ::
  AccessControlRule
newAccessControlRule =
  AccessControlRule'
    { actions = Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      dateModified = Prelude.Nothing,
      description = Prelude.Nothing,
      effect = Prelude.Nothing,
      impersonationRoleIds = Prelude.Nothing,
      ipRanges = Prelude.Nothing,
      name = Prelude.Nothing,
      notActions = Prelude.Nothing,
      notImpersonationRoleIds = Prelude.Nothing,
      notIpRanges = Prelude.Nothing,
      notUserIds = Prelude.Nothing,
      userIds = Prelude.Nothing
    }

-- | Access protocol actions to include in the rule. Valid values include
-- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
-- and @WebMail@.
accessControlRule_actions :: Lens.Lens' AccessControlRule (Prelude.Maybe [Prelude.Text])
accessControlRule_actions = Lens.lens (\AccessControlRule' {actions} -> actions) (\s@AccessControlRule' {} a -> s {actions = a} :: AccessControlRule) Prelude.. Lens.mapping Lens.coerced

-- | The date that the rule was created.
accessControlRule_dateCreated :: Lens.Lens' AccessControlRule (Prelude.Maybe Prelude.UTCTime)
accessControlRule_dateCreated = Lens.lens (\AccessControlRule' {dateCreated} -> dateCreated) (\s@AccessControlRule' {} a -> s {dateCreated = a} :: AccessControlRule) Prelude.. Lens.mapping Data._Time

-- | The date that the rule was modified.
accessControlRule_dateModified :: Lens.Lens' AccessControlRule (Prelude.Maybe Prelude.UTCTime)
accessControlRule_dateModified = Lens.lens (\AccessControlRule' {dateModified} -> dateModified) (\s@AccessControlRule' {} a -> s {dateModified = a} :: AccessControlRule) Prelude.. Lens.mapping Data._Time

-- | The rule description.
accessControlRule_description :: Lens.Lens' AccessControlRule (Prelude.Maybe Prelude.Text)
accessControlRule_description = Lens.lens (\AccessControlRule' {description} -> description) (\s@AccessControlRule' {} a -> s {description = a} :: AccessControlRule)

-- | The rule effect.
accessControlRule_effect :: Lens.Lens' AccessControlRule (Prelude.Maybe AccessControlRuleEffect)
accessControlRule_effect = Lens.lens (\AccessControlRule' {effect} -> effect) (\s@AccessControlRule' {} a -> s {effect = a} :: AccessControlRule)

-- | Impersonation role IDs to include in the rule.
accessControlRule_impersonationRoleIds :: Lens.Lens' AccessControlRule (Prelude.Maybe [Prelude.Text])
accessControlRule_impersonationRoleIds = Lens.lens (\AccessControlRule' {impersonationRoleIds} -> impersonationRoleIds) (\s@AccessControlRule' {} a -> s {impersonationRoleIds = a} :: AccessControlRule) Prelude.. Lens.mapping Lens.coerced

-- | IPv4 CIDR ranges to include in the rule.
accessControlRule_ipRanges :: Lens.Lens' AccessControlRule (Prelude.Maybe [Prelude.Text])
accessControlRule_ipRanges = Lens.lens (\AccessControlRule' {ipRanges} -> ipRanges) (\s@AccessControlRule' {} a -> s {ipRanges = a} :: AccessControlRule) Prelude.. Lens.mapping Lens.coerced

-- | The rule name.
accessControlRule_name :: Lens.Lens' AccessControlRule (Prelude.Maybe Prelude.Text)
accessControlRule_name = Lens.lens (\AccessControlRule' {name} -> name) (\s@AccessControlRule' {} a -> s {name = a} :: AccessControlRule)

-- | Access protocol actions to exclude from the rule. Valid values include
-- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
-- and @WebMail@.
accessControlRule_notActions :: Lens.Lens' AccessControlRule (Prelude.Maybe [Prelude.Text])
accessControlRule_notActions = Lens.lens (\AccessControlRule' {notActions} -> notActions) (\s@AccessControlRule' {} a -> s {notActions = a} :: AccessControlRule) Prelude.. Lens.mapping Lens.coerced

-- | Impersonation role IDs to exclude from the rule.
accessControlRule_notImpersonationRoleIds :: Lens.Lens' AccessControlRule (Prelude.Maybe [Prelude.Text])
accessControlRule_notImpersonationRoleIds = Lens.lens (\AccessControlRule' {notImpersonationRoleIds} -> notImpersonationRoleIds) (\s@AccessControlRule' {} a -> s {notImpersonationRoleIds = a} :: AccessControlRule) Prelude.. Lens.mapping Lens.coerced

-- | IPv4 CIDR ranges to exclude from the rule.
accessControlRule_notIpRanges :: Lens.Lens' AccessControlRule (Prelude.Maybe [Prelude.Text])
accessControlRule_notIpRanges = Lens.lens (\AccessControlRule' {notIpRanges} -> notIpRanges) (\s@AccessControlRule' {} a -> s {notIpRanges = a} :: AccessControlRule) Prelude.. Lens.mapping Lens.coerced

-- | User IDs to exclude from the rule.
accessControlRule_notUserIds :: Lens.Lens' AccessControlRule (Prelude.Maybe [Prelude.Text])
accessControlRule_notUserIds = Lens.lens (\AccessControlRule' {notUserIds} -> notUserIds) (\s@AccessControlRule' {} a -> s {notUserIds = a} :: AccessControlRule) Prelude.. Lens.mapping Lens.coerced

-- | User IDs to include in the rule.
accessControlRule_userIds :: Lens.Lens' AccessControlRule (Prelude.Maybe [Prelude.Text])
accessControlRule_userIds = Lens.lens (\AccessControlRule' {userIds} -> userIds) (\s@AccessControlRule' {} a -> s {userIds = a} :: AccessControlRule) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AccessControlRule where
  parseJSON =
    Data.withObject
      "AccessControlRule"
      ( \x ->
          AccessControlRule'
            Prelude.<$> (x Data..:? "Actions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "DateCreated")
            Prelude.<*> (x Data..:? "DateModified")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Effect")
            Prelude.<*> ( x Data..:? "ImpersonationRoleIds"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "IpRanges" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "NotActions" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "NotImpersonationRoleIds"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "NotIpRanges" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "NotUserIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "UserIds" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AccessControlRule where
  hashWithSalt _salt AccessControlRule' {..} =
    _salt `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` dateCreated
      `Prelude.hashWithSalt` dateModified
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` effect
      `Prelude.hashWithSalt` impersonationRoleIds
      `Prelude.hashWithSalt` ipRanges
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` notActions
      `Prelude.hashWithSalt` notImpersonationRoleIds
      `Prelude.hashWithSalt` notIpRanges
      `Prelude.hashWithSalt` notUserIds
      `Prelude.hashWithSalt` userIds

instance Prelude.NFData AccessControlRule where
  rnf AccessControlRule' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf dateCreated
      `Prelude.seq` Prelude.rnf dateModified
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf effect
      `Prelude.seq` Prelude.rnf impersonationRoleIds
      `Prelude.seq` Prelude.rnf ipRanges
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf notActions
      `Prelude.seq` Prelude.rnf notImpersonationRoleIds
      `Prelude.seq` Prelude.rnf notIpRanges
      `Prelude.seq` Prelude.rnf notUserIds
      `Prelude.seq` Prelude.rnf userIds
