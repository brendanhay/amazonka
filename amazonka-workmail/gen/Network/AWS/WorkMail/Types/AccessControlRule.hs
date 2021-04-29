{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WorkMail.Types.AccessControlRuleEffect

-- | A rule that controls access to an Amazon WorkMail organization.
--
-- /See:/ 'newAccessControlRule' smart constructor.
data AccessControlRule = AccessControlRule'
  { -- | The rule effect.
    effect :: Prelude.Maybe AccessControlRuleEffect,
    -- | The date that the rule was created.
    dateCreated :: Prelude.Maybe Prelude.POSIX,
    -- | IPv4 CIDR ranges to exclude from the rule.
    notIpRanges :: Prelude.Maybe [Prelude.Text],
    -- | IPv4 CIDR ranges to include in the rule.
    ipRanges :: Prelude.Maybe [Prelude.Text],
    -- | The date that the rule was modified.
    dateModified :: Prelude.Maybe Prelude.POSIX,
    -- | Access protocol actions to include in the rule. Valid values include
    -- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
    -- and @WebMail@.
    actions :: Prelude.Maybe [Prelude.Text],
    -- | User IDs to include in the rule.
    userIds :: Prelude.Maybe [Prelude.Text],
    -- | The rule name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The rule description.
    description :: Prelude.Maybe Prelude.Text,
    -- | Access protocol actions to exclude from the rule. Valid values include
    -- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
    -- and @WebMail@.
    notActions :: Prelude.Maybe [Prelude.Text],
    -- | User IDs to exclude from the rule.
    notUserIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { effect = Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      notIpRanges = Prelude.Nothing,
      ipRanges = Prelude.Nothing,
      dateModified = Prelude.Nothing,
      actions = Prelude.Nothing,
      userIds = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing,
      notActions = Prelude.Nothing,
      notUserIds = Prelude.Nothing
    }

-- | The rule effect.
accessControlRule_effect :: Lens.Lens' AccessControlRule (Prelude.Maybe AccessControlRuleEffect)
accessControlRule_effect = Lens.lens (\AccessControlRule' {effect} -> effect) (\s@AccessControlRule' {} a -> s {effect = a} :: AccessControlRule)

-- | The date that the rule was created.
accessControlRule_dateCreated :: Lens.Lens' AccessControlRule (Prelude.Maybe Prelude.UTCTime)
accessControlRule_dateCreated = Lens.lens (\AccessControlRule' {dateCreated} -> dateCreated) (\s@AccessControlRule' {} a -> s {dateCreated = a} :: AccessControlRule) Prelude.. Lens.mapping Prelude._Time

-- | IPv4 CIDR ranges to exclude from the rule.
accessControlRule_notIpRanges :: Lens.Lens' AccessControlRule (Prelude.Maybe [Prelude.Text])
accessControlRule_notIpRanges = Lens.lens (\AccessControlRule' {notIpRanges} -> notIpRanges) (\s@AccessControlRule' {} a -> s {notIpRanges = a} :: AccessControlRule) Prelude.. Lens.mapping Prelude._Coerce

-- | IPv4 CIDR ranges to include in the rule.
accessControlRule_ipRanges :: Lens.Lens' AccessControlRule (Prelude.Maybe [Prelude.Text])
accessControlRule_ipRanges = Lens.lens (\AccessControlRule' {ipRanges} -> ipRanges) (\s@AccessControlRule' {} a -> s {ipRanges = a} :: AccessControlRule) Prelude.. Lens.mapping Prelude._Coerce

-- | The date that the rule was modified.
accessControlRule_dateModified :: Lens.Lens' AccessControlRule (Prelude.Maybe Prelude.UTCTime)
accessControlRule_dateModified = Lens.lens (\AccessControlRule' {dateModified} -> dateModified) (\s@AccessControlRule' {} a -> s {dateModified = a} :: AccessControlRule) Prelude.. Lens.mapping Prelude._Time

-- | Access protocol actions to include in the rule. Valid values include
-- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
-- and @WebMail@.
accessControlRule_actions :: Lens.Lens' AccessControlRule (Prelude.Maybe [Prelude.Text])
accessControlRule_actions = Lens.lens (\AccessControlRule' {actions} -> actions) (\s@AccessControlRule' {} a -> s {actions = a} :: AccessControlRule) Prelude.. Lens.mapping Prelude._Coerce

-- | User IDs to include in the rule.
accessControlRule_userIds :: Lens.Lens' AccessControlRule (Prelude.Maybe [Prelude.Text])
accessControlRule_userIds = Lens.lens (\AccessControlRule' {userIds} -> userIds) (\s@AccessControlRule' {} a -> s {userIds = a} :: AccessControlRule) Prelude.. Lens.mapping Prelude._Coerce

-- | The rule name.
accessControlRule_name :: Lens.Lens' AccessControlRule (Prelude.Maybe Prelude.Text)
accessControlRule_name = Lens.lens (\AccessControlRule' {name} -> name) (\s@AccessControlRule' {} a -> s {name = a} :: AccessControlRule)

-- | The rule description.
accessControlRule_description :: Lens.Lens' AccessControlRule (Prelude.Maybe Prelude.Text)
accessControlRule_description = Lens.lens (\AccessControlRule' {description} -> description) (\s@AccessControlRule' {} a -> s {description = a} :: AccessControlRule)

-- | Access protocol actions to exclude from the rule. Valid values include
-- @ActiveSync@, @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@,
-- and @WebMail@.
accessControlRule_notActions :: Lens.Lens' AccessControlRule (Prelude.Maybe [Prelude.Text])
accessControlRule_notActions = Lens.lens (\AccessControlRule' {notActions} -> notActions) (\s@AccessControlRule' {} a -> s {notActions = a} :: AccessControlRule) Prelude.. Lens.mapping Prelude._Coerce

-- | User IDs to exclude from the rule.
accessControlRule_notUserIds :: Lens.Lens' AccessControlRule (Prelude.Maybe [Prelude.Text])
accessControlRule_notUserIds = Lens.lens (\AccessControlRule' {notUserIds} -> notUserIds) (\s@AccessControlRule' {} a -> s {notUserIds = a} :: AccessControlRule) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON AccessControlRule where
  parseJSON =
    Prelude.withObject
      "AccessControlRule"
      ( \x ->
          AccessControlRule'
            Prelude.<$> (x Prelude..:? "Effect")
            Prelude.<*> (x Prelude..:? "DateCreated")
            Prelude.<*> ( x Prelude..:? "NotIpRanges"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "IpRanges" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "DateModified")
            Prelude.<*> (x Prelude..:? "Actions" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "UserIds" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> ( x Prelude..:? "NotActions"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "NotUserIds"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AccessControlRule

instance Prelude.NFData AccessControlRule
