{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.AuthorizeIPRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more rules to the specified IP access control group.
--
-- This action gives users permission to access their WorkSpaces from the CIDR address ranges specified in the rules.
module Network.AWS.WorkSpaces.AuthorizeIPRules
  ( -- * Creating a request
    AuthorizeIPRules (..),
    mkAuthorizeIPRules,

    -- ** Request lenses
    airGroupId,
    airUserRules,

    -- * Destructuring the response
    AuthorizeIPRulesResponse (..),
    mkAuthorizeIPRulesResponse,

    -- ** Response lenses
    airrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkAuthorizeIPRules' smart constructor.
data AuthorizeIPRules = AuthorizeIPRules'
  { groupId :: Lude.Text,
    userRules :: [IPRuleItem]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuthorizeIPRules' with the minimum fields required to make a request.
--
-- * 'groupId' - The identifier of the group.
-- * 'userRules' - The rules to add to the group.
mkAuthorizeIPRules ::
  -- | 'groupId'
  Lude.Text ->
  AuthorizeIPRules
mkAuthorizeIPRules pGroupId_ =
  AuthorizeIPRules' {groupId = pGroupId_, userRules = Lude.mempty}

-- | The identifier of the group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
airGroupId :: Lens.Lens' AuthorizeIPRules Lude.Text
airGroupId = Lens.lens (groupId :: AuthorizeIPRules -> Lude.Text) (\s a -> s {groupId = a} :: AuthorizeIPRules)
{-# DEPRECATED airGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The rules to add to the group.
--
-- /Note:/ Consider using 'userRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
airUserRules :: Lens.Lens' AuthorizeIPRules [IPRuleItem]
airUserRules = Lens.lens (userRules :: AuthorizeIPRules -> [IPRuleItem]) (\s a -> s {userRules = a} :: AuthorizeIPRules)
{-# DEPRECATED airUserRules "Use generic-lens or generic-optics with 'userRules' instead." #-}

instance Lude.AWSRequest AuthorizeIPRules where
  type Rs AuthorizeIPRules = AuthorizeIPRulesResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AuthorizeIPRulesResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AuthorizeIPRules where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.AuthorizeIpRules" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AuthorizeIPRules where
  toJSON AuthorizeIPRules' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GroupId" Lude..= groupId),
            Lude.Just ("UserRules" Lude..= userRules)
          ]
      )

instance Lude.ToPath AuthorizeIPRules where
  toPath = Lude.const "/"

instance Lude.ToQuery AuthorizeIPRules where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAuthorizeIPRulesResponse' smart constructor.
newtype AuthorizeIPRulesResponse = AuthorizeIPRulesResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuthorizeIPRulesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAuthorizeIPRulesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AuthorizeIPRulesResponse
mkAuthorizeIPRulesResponse pResponseStatus_ =
  AuthorizeIPRulesResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
airrsResponseStatus :: Lens.Lens' AuthorizeIPRulesResponse Lude.Int
airrsResponseStatus = Lens.lens (responseStatus :: AuthorizeIPRulesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AuthorizeIPRulesResponse)
{-# DEPRECATED airrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
