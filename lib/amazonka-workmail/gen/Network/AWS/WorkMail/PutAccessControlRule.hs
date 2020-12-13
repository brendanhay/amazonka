{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.PutAccessControlRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new access control rule for the specified organization. The rule allows or denies access to the organization for the specified IPv4 addresses, access protocol actions, and user IDs. Adding a new rule with the same name as an existing rule replaces the older rule.
module Network.AWS.WorkMail.PutAccessControlRule
  ( -- * Creating a request
    PutAccessControlRule (..),
    mkPutAccessControlRule,

    -- ** Request lenses
    pacrEffect,
    pacrUserIds,
    pacrActions,
    pacrName,
    pacrNotUserIds,
    pacrIPRanges,
    pacrNotIPRanges,
    pacrNotActions,
    pacrDescription,
    pacrOrganizationId,

    -- * Destructuring the response
    PutAccessControlRuleResponse (..),
    mkPutAccessControlRuleResponse,

    -- ** Response lenses
    pacrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkPutAccessControlRule' smart constructor.
data PutAccessControlRule = PutAccessControlRule'
  { -- | The rule effect.
    effect :: AccessControlRuleEffect,
    -- | User IDs to include in the rule.
    userIds :: Lude.Maybe [Lude.Text],
    -- | Access protocol actions to include in the rule. Valid values include @ActiveSync@ , @AutoDiscover@ , @EWS@ , @IMAP@ , @SMTP@ , @WindowsOutlook@ , and @WebMail@ .
    actions :: Lude.Maybe [Lude.Text],
    -- | The rule name.
    name :: Lude.Text,
    -- | User IDs to exclude from the rule.
    notUserIds :: Lude.Maybe [Lude.Text],
    -- | IPv4 CIDR ranges to include in the rule.
    ipRanges :: Lude.Maybe [Lude.Text],
    -- | IPv4 CIDR ranges to exclude from the rule.
    notIPRanges :: Lude.Maybe [Lude.Text],
    -- | Access protocol actions to exclude from the rule. Valid values include @ActiveSync@ , @AutoDiscover@ , @EWS@ , @IMAP@ , @SMTP@ , @WindowsOutlook@ , and @WebMail@ .
    notActions :: Lude.Maybe [Lude.Text],
    -- | The rule description.
    description :: Lude.Text,
    -- | The identifier of the organization.
    organizationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutAccessControlRule' with the minimum fields required to make a request.
--
-- * 'effect' - The rule effect.
-- * 'userIds' - User IDs to include in the rule.
-- * 'actions' - Access protocol actions to include in the rule. Valid values include @ActiveSync@ , @AutoDiscover@ , @EWS@ , @IMAP@ , @SMTP@ , @WindowsOutlook@ , and @WebMail@ .
-- * 'name' - The rule name.
-- * 'notUserIds' - User IDs to exclude from the rule.
-- * 'ipRanges' - IPv4 CIDR ranges to include in the rule.
-- * 'notIPRanges' - IPv4 CIDR ranges to exclude from the rule.
-- * 'notActions' - Access protocol actions to exclude from the rule. Valid values include @ActiveSync@ , @AutoDiscover@ , @EWS@ , @IMAP@ , @SMTP@ , @WindowsOutlook@ , and @WebMail@ .
-- * 'description' - The rule description.
-- * 'organizationId' - The identifier of the organization.
mkPutAccessControlRule ::
  -- | 'effect'
  AccessControlRuleEffect ->
  -- | 'name'
  Lude.Text ->
  -- | 'description'
  Lude.Text ->
  -- | 'organizationId'
  Lude.Text ->
  PutAccessControlRule
mkPutAccessControlRule
  pEffect_
  pName_
  pDescription_
  pOrganizationId_ =
    PutAccessControlRule'
      { effect = pEffect_,
        userIds = Lude.Nothing,
        actions = Lude.Nothing,
        name = pName_,
        notUserIds = Lude.Nothing,
        ipRanges = Lude.Nothing,
        notIPRanges = Lude.Nothing,
        notActions = Lude.Nothing,
        description = pDescription_,
        organizationId = pOrganizationId_
      }

-- | The rule effect.
--
-- /Note:/ Consider using 'effect' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrEffect :: Lens.Lens' PutAccessControlRule AccessControlRuleEffect
pacrEffect = Lens.lens (effect :: PutAccessControlRule -> AccessControlRuleEffect) (\s a -> s {effect = a} :: PutAccessControlRule)
{-# DEPRECATED pacrEffect "Use generic-lens or generic-optics with 'effect' instead." #-}

-- | User IDs to include in the rule.
--
-- /Note:/ Consider using 'userIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrUserIds :: Lens.Lens' PutAccessControlRule (Lude.Maybe [Lude.Text])
pacrUserIds = Lens.lens (userIds :: PutAccessControlRule -> Lude.Maybe [Lude.Text]) (\s a -> s {userIds = a} :: PutAccessControlRule)
{-# DEPRECATED pacrUserIds "Use generic-lens or generic-optics with 'userIds' instead." #-}

-- | Access protocol actions to include in the rule. Valid values include @ActiveSync@ , @AutoDiscover@ , @EWS@ , @IMAP@ , @SMTP@ , @WindowsOutlook@ , and @WebMail@ .
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrActions :: Lens.Lens' PutAccessControlRule (Lude.Maybe [Lude.Text])
pacrActions = Lens.lens (actions :: PutAccessControlRule -> Lude.Maybe [Lude.Text]) (\s a -> s {actions = a} :: PutAccessControlRule)
{-# DEPRECATED pacrActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | The rule name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrName :: Lens.Lens' PutAccessControlRule Lude.Text
pacrName = Lens.lens (name :: PutAccessControlRule -> Lude.Text) (\s a -> s {name = a} :: PutAccessControlRule)
{-# DEPRECATED pacrName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | User IDs to exclude from the rule.
--
-- /Note:/ Consider using 'notUserIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrNotUserIds :: Lens.Lens' PutAccessControlRule (Lude.Maybe [Lude.Text])
pacrNotUserIds = Lens.lens (notUserIds :: PutAccessControlRule -> Lude.Maybe [Lude.Text]) (\s a -> s {notUserIds = a} :: PutAccessControlRule)
{-# DEPRECATED pacrNotUserIds "Use generic-lens or generic-optics with 'notUserIds' instead." #-}

-- | IPv4 CIDR ranges to include in the rule.
--
-- /Note:/ Consider using 'ipRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrIPRanges :: Lens.Lens' PutAccessControlRule (Lude.Maybe [Lude.Text])
pacrIPRanges = Lens.lens (ipRanges :: PutAccessControlRule -> Lude.Maybe [Lude.Text]) (\s a -> s {ipRanges = a} :: PutAccessControlRule)
{-# DEPRECATED pacrIPRanges "Use generic-lens or generic-optics with 'ipRanges' instead." #-}

-- | IPv4 CIDR ranges to exclude from the rule.
--
-- /Note:/ Consider using 'notIPRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrNotIPRanges :: Lens.Lens' PutAccessControlRule (Lude.Maybe [Lude.Text])
pacrNotIPRanges = Lens.lens (notIPRanges :: PutAccessControlRule -> Lude.Maybe [Lude.Text]) (\s a -> s {notIPRanges = a} :: PutAccessControlRule)
{-# DEPRECATED pacrNotIPRanges "Use generic-lens or generic-optics with 'notIPRanges' instead." #-}

-- | Access protocol actions to exclude from the rule. Valid values include @ActiveSync@ , @AutoDiscover@ , @EWS@ , @IMAP@ , @SMTP@ , @WindowsOutlook@ , and @WebMail@ .
--
-- /Note:/ Consider using 'notActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrNotActions :: Lens.Lens' PutAccessControlRule (Lude.Maybe [Lude.Text])
pacrNotActions = Lens.lens (notActions :: PutAccessControlRule -> Lude.Maybe [Lude.Text]) (\s a -> s {notActions = a} :: PutAccessControlRule)
{-# DEPRECATED pacrNotActions "Use generic-lens or generic-optics with 'notActions' instead." #-}

-- | The rule description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrDescription :: Lens.Lens' PutAccessControlRule Lude.Text
pacrDescription = Lens.lens (description :: PutAccessControlRule -> Lude.Text) (\s a -> s {description = a} :: PutAccessControlRule)
{-# DEPRECATED pacrDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The identifier of the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrOrganizationId :: Lens.Lens' PutAccessControlRule Lude.Text
pacrOrganizationId = Lens.lens (organizationId :: PutAccessControlRule -> Lude.Text) (\s a -> s {organizationId = a} :: PutAccessControlRule)
{-# DEPRECATED pacrOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Lude.AWSRequest PutAccessControlRule where
  type Rs PutAccessControlRule = PutAccessControlRuleResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveEmpty
      ( \s h x ->
          PutAccessControlRuleResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutAccessControlRule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.PutAccessControlRule" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutAccessControlRule where
  toJSON PutAccessControlRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Effect" Lude..= effect),
            ("UserIds" Lude..=) Lude.<$> userIds,
            ("Actions" Lude..=) Lude.<$> actions,
            Lude.Just ("Name" Lude..= name),
            ("NotUserIds" Lude..=) Lude.<$> notUserIds,
            ("IpRanges" Lude..=) Lude.<$> ipRanges,
            ("NotIpRanges" Lude..=) Lude.<$> notIPRanges,
            ("NotActions" Lude..=) Lude.<$> notActions,
            Lude.Just ("Description" Lude..= description),
            Lude.Just ("OrganizationId" Lude..= organizationId)
          ]
      )

instance Lude.ToPath PutAccessControlRule where
  toPath = Lude.const "/"

instance Lude.ToQuery PutAccessControlRule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutAccessControlRuleResponse' smart constructor.
newtype PutAccessControlRuleResponse = PutAccessControlRuleResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutAccessControlRuleResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkPutAccessControlRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutAccessControlRuleResponse
mkPutAccessControlRuleResponse pResponseStatus_ =
  PutAccessControlRuleResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrrsResponseStatus :: Lens.Lens' PutAccessControlRuleResponse Lude.Int
pacrrsResponseStatus = Lens.lens (responseStatus :: PutAccessControlRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutAccessControlRuleResponse)
{-# DEPRECATED pacrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
