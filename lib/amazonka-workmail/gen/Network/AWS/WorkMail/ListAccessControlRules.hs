{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.ListAccessControlRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the access control rules for the specified organization.
module Network.AWS.WorkMail.ListAccessControlRules
  ( -- * Creating a request
    ListAccessControlRules (..),
    mkListAccessControlRules,

    -- ** Request lenses
    lacrOrganizationId,

    -- * Destructuring the response
    ListAccessControlRulesResponse (..),
    mkListAccessControlRulesResponse,

    -- ** Response lenses
    lacrrsRules,
    lacrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkListAccessControlRules' smart constructor.
newtype ListAccessControlRules = ListAccessControlRules'
  { organizationId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAccessControlRules' with the minimum fields required to make a request.
--
-- * 'organizationId' - The identifier for the organization.
mkListAccessControlRules ::
  -- | 'organizationId'
  Lude.Text ->
  ListAccessControlRules
mkListAccessControlRules pOrganizationId_ =
  ListAccessControlRules' {organizationId = pOrganizationId_}

-- | The identifier for the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lacrOrganizationId :: Lens.Lens' ListAccessControlRules Lude.Text
lacrOrganizationId = Lens.lens (organizationId :: ListAccessControlRules -> Lude.Text) (\s a -> s {organizationId = a} :: ListAccessControlRules)
{-# DEPRECATED lacrOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Lude.AWSRequest ListAccessControlRules where
  type Rs ListAccessControlRules = ListAccessControlRulesResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAccessControlRulesResponse'
            Lude.<$> (x Lude..?> "Rules" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAccessControlRules where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.ListAccessControlRules" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListAccessControlRules where
  toJSON ListAccessControlRules' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("OrganizationId" Lude..= organizationId)]
      )

instance Lude.ToPath ListAccessControlRules where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAccessControlRules where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAccessControlRulesResponse' smart constructor.
data ListAccessControlRulesResponse = ListAccessControlRulesResponse'
  { rules ::
      Lude.Maybe
        [AccessControlRule],
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

-- | Creates a value of 'ListAccessControlRulesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'rules' - The access control rules.
mkListAccessControlRulesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAccessControlRulesResponse
mkListAccessControlRulesResponse pResponseStatus_ =
  ListAccessControlRulesResponse'
    { rules = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The access control rules.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lacrrsRules :: Lens.Lens' ListAccessControlRulesResponse (Lude.Maybe [AccessControlRule])
lacrrsRules = Lens.lens (rules :: ListAccessControlRulesResponse -> Lude.Maybe [AccessControlRule]) (\s a -> s {rules = a} :: ListAccessControlRulesResponse)
{-# DEPRECATED lacrrsRules "Use generic-lens or generic-optics with 'rules' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lacrrsResponseStatus :: Lens.Lens' ListAccessControlRulesResponse Lude.Int
lacrrsResponseStatus = Lens.lens (responseStatus :: ListAccessControlRulesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAccessControlRulesResponse)
{-# DEPRECATED lacrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
