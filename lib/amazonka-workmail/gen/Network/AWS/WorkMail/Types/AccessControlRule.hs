{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.AccessControlRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.AccessControlRule
  ( AccessControlRule (..),

    -- * Smart constructor
    mkAccessControlRule,

    -- * Lenses
    acrEffect,
    acrUserIds,
    acrActions,
    acrDateCreated,
    acrName,
    acrNotUserIds,
    acrDateModified,
    acrIPRanges,
    acrNotIPRanges,
    acrNotActions,
    acrDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkMail.Types.AccessControlRuleEffect

-- | A rule that controls access to an Amazon WorkMail organization.
--
-- /See:/ 'mkAccessControlRule' smart constructor.
data AccessControlRule = AccessControlRule'
  { -- | The rule effect.
    effect :: Lude.Maybe AccessControlRuleEffect,
    -- | User IDs to include in the rule.
    userIds :: Lude.Maybe [Lude.Text],
    -- | Access protocol actions to include in the rule. Valid values include @ActiveSync@ , @AutoDiscover@ , @EWS@ , @IMAP@ , @SMTP@ , @WindowsOutlook@ , and @WebMail@ .
    actions :: Lude.Maybe [Lude.Text],
    -- | The date that the rule was created.
    dateCreated :: Lude.Maybe Lude.Timestamp,
    -- | The rule name.
    name :: Lude.Maybe Lude.Text,
    -- | User IDs to exclude from the rule.
    notUserIds :: Lude.Maybe [Lude.Text],
    -- | The date that the rule was modified.
    dateModified :: Lude.Maybe Lude.Timestamp,
    -- | IPv4 CIDR ranges to include in the rule.
    ipRanges :: Lude.Maybe [Lude.Text],
    -- | IPv4 CIDR ranges to exclude from the rule.
    notIPRanges :: Lude.Maybe [Lude.Text],
    -- | Access protocol actions to exclude from the rule. Valid values include @ActiveSync@ , @AutoDiscover@ , @EWS@ , @IMAP@ , @SMTP@ , @WindowsOutlook@ , and @WebMail@ .
    notActions :: Lude.Maybe [Lude.Text],
    -- | The rule description.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccessControlRule' with the minimum fields required to make a request.
--
-- * 'effect' - The rule effect.
-- * 'userIds' - User IDs to include in the rule.
-- * 'actions' - Access protocol actions to include in the rule. Valid values include @ActiveSync@ , @AutoDiscover@ , @EWS@ , @IMAP@ , @SMTP@ , @WindowsOutlook@ , and @WebMail@ .
-- * 'dateCreated' - The date that the rule was created.
-- * 'name' - The rule name.
-- * 'notUserIds' - User IDs to exclude from the rule.
-- * 'dateModified' - The date that the rule was modified.
-- * 'ipRanges' - IPv4 CIDR ranges to include in the rule.
-- * 'notIPRanges' - IPv4 CIDR ranges to exclude from the rule.
-- * 'notActions' - Access protocol actions to exclude from the rule. Valid values include @ActiveSync@ , @AutoDiscover@ , @EWS@ , @IMAP@ , @SMTP@ , @WindowsOutlook@ , and @WebMail@ .
-- * 'description' - The rule description.
mkAccessControlRule ::
  AccessControlRule
mkAccessControlRule =
  AccessControlRule'
    { effect = Lude.Nothing,
      userIds = Lude.Nothing,
      actions = Lude.Nothing,
      dateCreated = Lude.Nothing,
      name = Lude.Nothing,
      notUserIds = Lude.Nothing,
      dateModified = Lude.Nothing,
      ipRanges = Lude.Nothing,
      notIPRanges = Lude.Nothing,
      notActions = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The rule effect.
--
-- /Note:/ Consider using 'effect' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acrEffect :: Lens.Lens' AccessControlRule (Lude.Maybe AccessControlRuleEffect)
acrEffect = Lens.lens (effect :: AccessControlRule -> Lude.Maybe AccessControlRuleEffect) (\s a -> s {effect = a} :: AccessControlRule)
{-# DEPRECATED acrEffect "Use generic-lens or generic-optics with 'effect' instead." #-}

-- | User IDs to include in the rule.
--
-- /Note:/ Consider using 'userIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acrUserIds :: Lens.Lens' AccessControlRule (Lude.Maybe [Lude.Text])
acrUserIds = Lens.lens (userIds :: AccessControlRule -> Lude.Maybe [Lude.Text]) (\s a -> s {userIds = a} :: AccessControlRule)
{-# DEPRECATED acrUserIds "Use generic-lens or generic-optics with 'userIds' instead." #-}

-- | Access protocol actions to include in the rule. Valid values include @ActiveSync@ , @AutoDiscover@ , @EWS@ , @IMAP@ , @SMTP@ , @WindowsOutlook@ , and @WebMail@ .
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acrActions :: Lens.Lens' AccessControlRule (Lude.Maybe [Lude.Text])
acrActions = Lens.lens (actions :: AccessControlRule -> Lude.Maybe [Lude.Text]) (\s a -> s {actions = a} :: AccessControlRule)
{-# DEPRECATED acrActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | The date that the rule was created.
--
-- /Note:/ Consider using 'dateCreated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acrDateCreated :: Lens.Lens' AccessControlRule (Lude.Maybe Lude.Timestamp)
acrDateCreated = Lens.lens (dateCreated :: AccessControlRule -> Lude.Maybe Lude.Timestamp) (\s a -> s {dateCreated = a} :: AccessControlRule)
{-# DEPRECATED acrDateCreated "Use generic-lens or generic-optics with 'dateCreated' instead." #-}

-- | The rule name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acrName :: Lens.Lens' AccessControlRule (Lude.Maybe Lude.Text)
acrName = Lens.lens (name :: AccessControlRule -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: AccessControlRule)
{-# DEPRECATED acrName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | User IDs to exclude from the rule.
--
-- /Note:/ Consider using 'notUserIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acrNotUserIds :: Lens.Lens' AccessControlRule (Lude.Maybe [Lude.Text])
acrNotUserIds = Lens.lens (notUserIds :: AccessControlRule -> Lude.Maybe [Lude.Text]) (\s a -> s {notUserIds = a} :: AccessControlRule)
{-# DEPRECATED acrNotUserIds "Use generic-lens or generic-optics with 'notUserIds' instead." #-}

-- | The date that the rule was modified.
--
-- /Note:/ Consider using 'dateModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acrDateModified :: Lens.Lens' AccessControlRule (Lude.Maybe Lude.Timestamp)
acrDateModified = Lens.lens (dateModified :: AccessControlRule -> Lude.Maybe Lude.Timestamp) (\s a -> s {dateModified = a} :: AccessControlRule)
{-# DEPRECATED acrDateModified "Use generic-lens or generic-optics with 'dateModified' instead." #-}

-- | IPv4 CIDR ranges to include in the rule.
--
-- /Note:/ Consider using 'ipRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acrIPRanges :: Lens.Lens' AccessControlRule (Lude.Maybe [Lude.Text])
acrIPRanges = Lens.lens (ipRanges :: AccessControlRule -> Lude.Maybe [Lude.Text]) (\s a -> s {ipRanges = a} :: AccessControlRule)
{-# DEPRECATED acrIPRanges "Use generic-lens or generic-optics with 'ipRanges' instead." #-}

-- | IPv4 CIDR ranges to exclude from the rule.
--
-- /Note:/ Consider using 'notIPRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acrNotIPRanges :: Lens.Lens' AccessControlRule (Lude.Maybe [Lude.Text])
acrNotIPRanges = Lens.lens (notIPRanges :: AccessControlRule -> Lude.Maybe [Lude.Text]) (\s a -> s {notIPRanges = a} :: AccessControlRule)
{-# DEPRECATED acrNotIPRanges "Use generic-lens or generic-optics with 'notIPRanges' instead." #-}

-- | Access protocol actions to exclude from the rule. Valid values include @ActiveSync@ , @AutoDiscover@ , @EWS@ , @IMAP@ , @SMTP@ , @WindowsOutlook@ , and @WebMail@ .
--
-- /Note:/ Consider using 'notActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acrNotActions :: Lens.Lens' AccessControlRule (Lude.Maybe [Lude.Text])
acrNotActions = Lens.lens (notActions :: AccessControlRule -> Lude.Maybe [Lude.Text]) (\s a -> s {notActions = a} :: AccessControlRule)
{-# DEPRECATED acrNotActions "Use generic-lens or generic-optics with 'notActions' instead." #-}

-- | The rule description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acrDescription :: Lens.Lens' AccessControlRule (Lude.Maybe Lude.Text)
acrDescription = Lens.lens (description :: AccessControlRule -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: AccessControlRule)
{-# DEPRECATED acrDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON AccessControlRule where
  parseJSON =
    Lude.withObject
      "AccessControlRule"
      ( \x ->
          AccessControlRule'
            Lude.<$> (x Lude..:? "Effect")
            Lude.<*> (x Lude..:? "UserIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Actions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "DateCreated")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "NotUserIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "DateModified")
            Lude.<*> (x Lude..:? "IpRanges" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "NotIpRanges" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "NotActions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Description")
      )
