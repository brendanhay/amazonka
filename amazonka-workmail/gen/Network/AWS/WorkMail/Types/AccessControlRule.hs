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
-- Module      : Network.AWS.WorkMail.Types.AccessControlRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.AccessControlRule where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.WorkMail.Types.AccessControlRuleEffect

-- | A rule that controls access to an Amazon WorkMail organization.
--
-- /See:/ 'newAccessControlRule' smart constructor.
data AccessControlRule = AccessControlRule'
  { -- | The rule effect.
    effect :: Core.Maybe AccessControlRuleEffect,
    -- | The date that the rule was created.
    dateCreated :: Core.Maybe Core.POSIX,
    -- | IPv4 CIDR ranges to exclude from the rule.
    notIpRanges :: Core.Maybe [Core.Text],
    -- | IPv4 CIDR ranges to include in the rule.
    ipRanges :: Core.Maybe [Core.Text],
    -- | The date that the rule was modified.
    dateModified :: Core.Maybe Core.POSIX,
    -- | Access protocol actions to include in the rule. Valid values include
    -- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
    -- and @WebMail@.
    actions :: Core.Maybe [Core.Text],
    -- | User IDs to include in the rule.
    userIds :: Core.Maybe [Core.Text],
    -- | The rule name.
    name :: Core.Maybe Core.Text,
    -- | The rule description.
    description :: Core.Maybe Core.Text,
    -- | Access protocol actions to exclude from the rule. Valid values include
    -- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
    -- and @WebMail@.
    notActions :: Core.Maybe [Core.Text],
    -- | User IDs to exclude from the rule.
    notUserIds :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AccessControlRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'effect', 'accessControlRule_effect' - The rule effect.
--
-- 'dateCreated', 'accessControlRule_dateCreated' - The date that the rule was created.
--
-- 'notIpRanges', 'accessControlRule_notIpRanges' - IPv4 CIDR ranges to exclude from the rule.
--
-- 'ipRanges', 'accessControlRule_ipRanges' - IPv4 CIDR ranges to include in the rule.
--
-- 'dateModified', 'accessControlRule_dateModified' - The date that the rule was modified.
--
-- 'actions', 'accessControlRule_actions' - Access protocol actions to include in the rule. Valid values include
-- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
-- and @WebMail@.
--
-- 'userIds', 'accessControlRule_userIds' - User IDs to include in the rule.
--
-- 'name', 'accessControlRule_name' - The rule name.
--
-- 'description', 'accessControlRule_description' - The rule description.
--
-- 'notActions', 'accessControlRule_notActions' - Access protocol actions to exclude from the rule. Valid values include
-- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
-- and @WebMail@.
--
-- 'notUserIds', 'accessControlRule_notUserIds' - User IDs to exclude from the rule.
newAccessControlRule ::
  AccessControlRule
newAccessControlRule =
  AccessControlRule'
    { effect = Core.Nothing,
      dateCreated = Core.Nothing,
      notIpRanges = Core.Nothing,
      ipRanges = Core.Nothing,
      dateModified = Core.Nothing,
      actions = Core.Nothing,
      userIds = Core.Nothing,
      name = Core.Nothing,
      description = Core.Nothing,
      notActions = Core.Nothing,
      notUserIds = Core.Nothing
    }

-- | The rule effect.
accessControlRule_effect :: Lens.Lens' AccessControlRule (Core.Maybe AccessControlRuleEffect)
accessControlRule_effect = Lens.lens (\AccessControlRule' {effect} -> effect) (\s@AccessControlRule' {} a -> s {effect = a} :: AccessControlRule)

-- | The date that the rule was created.
accessControlRule_dateCreated :: Lens.Lens' AccessControlRule (Core.Maybe Core.UTCTime)
accessControlRule_dateCreated = Lens.lens (\AccessControlRule' {dateCreated} -> dateCreated) (\s@AccessControlRule' {} a -> s {dateCreated = a} :: AccessControlRule) Core.. Lens.mapping Core._Time

-- | IPv4 CIDR ranges to exclude from the rule.
accessControlRule_notIpRanges :: Lens.Lens' AccessControlRule (Core.Maybe [Core.Text])
accessControlRule_notIpRanges = Lens.lens (\AccessControlRule' {notIpRanges} -> notIpRanges) (\s@AccessControlRule' {} a -> s {notIpRanges = a} :: AccessControlRule) Core.. Lens.mapping Lens._Coerce

-- | IPv4 CIDR ranges to include in the rule.
accessControlRule_ipRanges :: Lens.Lens' AccessControlRule (Core.Maybe [Core.Text])
accessControlRule_ipRanges = Lens.lens (\AccessControlRule' {ipRanges} -> ipRanges) (\s@AccessControlRule' {} a -> s {ipRanges = a} :: AccessControlRule) Core.. Lens.mapping Lens._Coerce

-- | The date that the rule was modified.
accessControlRule_dateModified :: Lens.Lens' AccessControlRule (Core.Maybe Core.UTCTime)
accessControlRule_dateModified = Lens.lens (\AccessControlRule' {dateModified} -> dateModified) (\s@AccessControlRule' {} a -> s {dateModified = a} :: AccessControlRule) Core.. Lens.mapping Core._Time

-- | Access protocol actions to include in the rule. Valid values include
-- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
-- and @WebMail@.
accessControlRule_actions :: Lens.Lens' AccessControlRule (Core.Maybe [Core.Text])
accessControlRule_actions = Lens.lens (\AccessControlRule' {actions} -> actions) (\s@AccessControlRule' {} a -> s {actions = a} :: AccessControlRule) Core.. Lens.mapping Lens._Coerce

-- | User IDs to include in the rule.
accessControlRule_userIds :: Lens.Lens' AccessControlRule (Core.Maybe [Core.Text])
accessControlRule_userIds = Lens.lens (\AccessControlRule' {userIds} -> userIds) (\s@AccessControlRule' {} a -> s {userIds = a} :: AccessControlRule) Core.. Lens.mapping Lens._Coerce

-- | The rule name.
accessControlRule_name :: Lens.Lens' AccessControlRule (Core.Maybe Core.Text)
accessControlRule_name = Lens.lens (\AccessControlRule' {name} -> name) (\s@AccessControlRule' {} a -> s {name = a} :: AccessControlRule)

-- | The rule description.
accessControlRule_description :: Lens.Lens' AccessControlRule (Core.Maybe Core.Text)
accessControlRule_description = Lens.lens (\AccessControlRule' {description} -> description) (\s@AccessControlRule' {} a -> s {description = a} :: AccessControlRule)

-- | Access protocol actions to exclude from the rule. Valid values include
-- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
-- and @WebMail@.
accessControlRule_notActions :: Lens.Lens' AccessControlRule (Core.Maybe [Core.Text])
accessControlRule_notActions = Lens.lens (\AccessControlRule' {notActions} -> notActions) (\s@AccessControlRule' {} a -> s {notActions = a} :: AccessControlRule) Core.. Lens.mapping Lens._Coerce

-- | User IDs to exclude from the rule.
accessControlRule_notUserIds :: Lens.Lens' AccessControlRule (Core.Maybe [Core.Text])
accessControlRule_notUserIds = Lens.lens (\AccessControlRule' {notUserIds} -> notUserIds) (\s@AccessControlRule' {} a -> s {notUserIds = a} :: AccessControlRule) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON AccessControlRule where
  parseJSON =
    Core.withObject
      "AccessControlRule"
      ( \x ->
          AccessControlRule'
            Core.<$> (x Core..:? "Effect")
            Core.<*> (x Core..:? "DateCreated")
            Core.<*> (x Core..:? "NotIpRanges" Core..!= Core.mempty)
            Core.<*> (x Core..:? "IpRanges" Core..!= Core.mempty)
            Core.<*> (x Core..:? "DateModified")
            Core.<*> (x Core..:? "Actions" Core..!= Core.mempty)
            Core.<*> (x Core..:? "UserIds" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "NotActions" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NotUserIds" Core..!= Core.mempty)
      )

instance Core.Hashable AccessControlRule

instance Core.NFData AccessControlRule
