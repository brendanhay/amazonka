{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.GetAccessControlEffect
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the effects of an organization's access control rules as they apply to a specified IPv4 address, access protocol action, or user ID.
module Network.AWS.WorkMail.GetAccessControlEffect
  ( -- * Creating a request
    GetAccessControlEffect (..),
    mkGetAccessControlEffect,

    -- ** Request lenses
    gaceOrganizationId,
    gaceIPAddress,
    gaceAction,
    gaceUserId,

    -- * Destructuring the response
    GetAccessControlEffectResponse (..),
    mkGetAccessControlEffectResponse,

    -- ** Response lenses
    gacersEffect,
    gacersMatchedRules,
    gacersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkGetAccessControlEffect' smart constructor.
data GetAccessControlEffect = GetAccessControlEffect'
  { organizationId ::
      Lude.Text,
    ipAddress :: Lude.Text,
    action :: Lude.Text,
    userId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAccessControlEffect' with the minimum fields required to make a request.
--
-- * 'action' - The access protocol action. Valid values include @ActiveSync@ , @AutoDiscover@ , @EWS@ , @IMAP@ , @SMTP@ , @WindowsOutlook@ , and @WebMail@ .
-- * 'ipAddress' - The IPv4 address.
-- * 'organizationId' - The identifier for the organization.
-- * 'userId' - The user ID.
mkGetAccessControlEffect ::
  -- | 'organizationId'
  Lude.Text ->
  -- | 'ipAddress'
  Lude.Text ->
  -- | 'action'
  Lude.Text ->
  -- | 'userId'
  Lude.Text ->
  GetAccessControlEffect
mkGetAccessControlEffect
  pOrganizationId_
  pIPAddress_
  pAction_
  pUserId_ =
    GetAccessControlEffect'
      { organizationId = pOrganizationId_,
        ipAddress = pIPAddress_,
        action = pAction_,
        userId = pUserId_
      }

-- | The identifier for the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaceOrganizationId :: Lens.Lens' GetAccessControlEffect Lude.Text
gaceOrganizationId = Lens.lens (organizationId :: GetAccessControlEffect -> Lude.Text) (\s a -> s {organizationId = a} :: GetAccessControlEffect)
{-# DEPRECATED gaceOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The IPv4 address.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaceIPAddress :: Lens.Lens' GetAccessControlEffect Lude.Text
gaceIPAddress = Lens.lens (ipAddress :: GetAccessControlEffect -> Lude.Text) (\s a -> s {ipAddress = a} :: GetAccessControlEffect)
{-# DEPRECATED gaceIPAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | The access protocol action. Valid values include @ActiveSync@ , @AutoDiscover@ , @EWS@ , @IMAP@ , @SMTP@ , @WindowsOutlook@ , and @WebMail@ .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaceAction :: Lens.Lens' GetAccessControlEffect Lude.Text
gaceAction = Lens.lens (action :: GetAccessControlEffect -> Lude.Text) (\s a -> s {action = a} :: GetAccessControlEffect)
{-# DEPRECATED gaceAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The user ID.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaceUserId :: Lens.Lens' GetAccessControlEffect Lude.Text
gaceUserId = Lens.lens (userId :: GetAccessControlEffect -> Lude.Text) (\s a -> s {userId = a} :: GetAccessControlEffect)
{-# DEPRECATED gaceUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

instance Lude.AWSRequest GetAccessControlEffect where
  type Rs GetAccessControlEffect = GetAccessControlEffectResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAccessControlEffectResponse'
            Lude.<$> (x Lude..?> "Effect")
            Lude.<*> (x Lude..?> "MatchedRules" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAccessControlEffect where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.GetAccessControlEffect" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetAccessControlEffect where
  toJSON GetAccessControlEffect' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("OrganizationId" Lude..= organizationId),
            Lude.Just ("IpAddress" Lude..= ipAddress),
            Lude.Just ("Action" Lude..= action),
            Lude.Just ("UserId" Lude..= userId)
          ]
      )

instance Lude.ToPath GetAccessControlEffect where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAccessControlEffect where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAccessControlEffectResponse' smart constructor.
data GetAccessControlEffectResponse = GetAccessControlEffectResponse'
  { effect ::
      Lude.Maybe
        AccessControlRuleEffect,
    matchedRules ::
      Lude.Maybe [Lude.Text],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAccessControlEffectResponse' with the minimum fields required to make a request.
--
-- * 'effect' - The rule effect.
-- * 'matchedRules' - The rules that match the given parameters, resulting in an effect.
-- * 'responseStatus' - The response status code.
mkGetAccessControlEffectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAccessControlEffectResponse
mkGetAccessControlEffectResponse pResponseStatus_ =
  GetAccessControlEffectResponse'
    { effect = Lude.Nothing,
      matchedRules = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The rule effect.
--
-- /Note:/ Consider using 'effect' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacersEffect :: Lens.Lens' GetAccessControlEffectResponse (Lude.Maybe AccessControlRuleEffect)
gacersEffect = Lens.lens (effect :: GetAccessControlEffectResponse -> Lude.Maybe AccessControlRuleEffect) (\s a -> s {effect = a} :: GetAccessControlEffectResponse)
{-# DEPRECATED gacersEffect "Use generic-lens or generic-optics with 'effect' instead." #-}

-- | The rules that match the given parameters, resulting in an effect.
--
-- /Note:/ Consider using 'matchedRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacersMatchedRules :: Lens.Lens' GetAccessControlEffectResponse (Lude.Maybe [Lude.Text])
gacersMatchedRules = Lens.lens (matchedRules :: GetAccessControlEffectResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {matchedRules = a} :: GetAccessControlEffectResponse)
{-# DEPRECATED gacersMatchedRules "Use generic-lens or generic-optics with 'matchedRules' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacersResponseStatus :: Lens.Lens' GetAccessControlEffectResponse Lude.Int
gacersResponseStatus = Lens.lens (responseStatus :: GetAccessControlEffectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAccessControlEffectResponse)
{-# DEPRECATED gacersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
