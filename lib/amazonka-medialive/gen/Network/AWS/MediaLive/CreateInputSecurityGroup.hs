{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.CreateInputSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Input Security Group
module Network.AWS.MediaLive.CreateInputSecurityGroup
  ( -- * Creating a request
    CreateInputSecurityGroup (..),
    mkCreateInputSecurityGroup,

    -- ** Request lenses
    cisgWhitelistRules,
    cisgTags,

    -- * Destructuring the response
    CreateInputSecurityGroupResponse (..),
    mkCreateInputSecurityGroupResponse,

    -- ** Response lenses
    cisgrsSecurityGroup,
    cisgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The IPv4 CIDRs to whitelist for this Input Security Group
--
-- /See:/ 'mkCreateInputSecurityGroup' smart constructor.
data CreateInputSecurityGroup = CreateInputSecurityGroup'
  { -- | List of IPv4 CIDR addresses to whitelist
    whitelistRules :: Lude.Maybe [InputWhitelistRuleCidr],
    -- | A collection of key-value pairs.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateInputSecurityGroup' with the minimum fields required to make a request.
--
-- * 'whitelistRules' - List of IPv4 CIDR addresses to whitelist
-- * 'tags' - A collection of key-value pairs.
mkCreateInputSecurityGroup ::
  CreateInputSecurityGroup
mkCreateInputSecurityGroup =
  CreateInputSecurityGroup'
    { whitelistRules = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | List of IPv4 CIDR addresses to whitelist
--
-- /Note:/ Consider using 'whitelistRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisgWhitelistRules :: Lens.Lens' CreateInputSecurityGroup (Lude.Maybe [InputWhitelistRuleCidr])
cisgWhitelistRules = Lens.lens (whitelistRules :: CreateInputSecurityGroup -> Lude.Maybe [InputWhitelistRuleCidr]) (\s a -> s {whitelistRules = a} :: CreateInputSecurityGroup)
{-# DEPRECATED cisgWhitelistRules "Use generic-lens or generic-optics with 'whitelistRules' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisgTags :: Lens.Lens' CreateInputSecurityGroup (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cisgTags = Lens.lens (tags :: CreateInputSecurityGroup -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateInputSecurityGroup)
{-# DEPRECATED cisgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateInputSecurityGroup where
  type Rs CreateInputSecurityGroup = CreateInputSecurityGroupResponse
  request = Req.postJSON mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateInputSecurityGroupResponse'
            Lude.<$> (x Lude..?> "securityGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateInputSecurityGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateInputSecurityGroup where
  toJSON CreateInputSecurityGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("whitelistRules" Lude..=) Lude.<$> whitelistRules,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateInputSecurityGroup where
  toPath = Lude.const "/prod/inputSecurityGroups"

instance Lude.ToQuery CreateInputSecurityGroup where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for CreateInputSecurityGroupResponse
--
-- /See:/ 'mkCreateInputSecurityGroupResponse' smart constructor.
data CreateInputSecurityGroupResponse = CreateInputSecurityGroupResponse'
  { securityGroup :: Lude.Maybe InputSecurityGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateInputSecurityGroupResponse' with the minimum fields required to make a request.
--
-- * 'securityGroup' -
-- * 'responseStatus' - The response status code.
mkCreateInputSecurityGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateInputSecurityGroupResponse
mkCreateInputSecurityGroupResponse pResponseStatus_ =
  CreateInputSecurityGroupResponse'
    { securityGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'securityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisgrsSecurityGroup :: Lens.Lens' CreateInputSecurityGroupResponse (Lude.Maybe InputSecurityGroup)
cisgrsSecurityGroup = Lens.lens (securityGroup :: CreateInputSecurityGroupResponse -> Lude.Maybe InputSecurityGroup) (\s a -> s {securityGroup = a} :: CreateInputSecurityGroupResponse)
{-# DEPRECATED cisgrsSecurityGroup "Use generic-lens or generic-optics with 'securityGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisgrsResponseStatus :: Lens.Lens' CreateInputSecurityGroupResponse Lude.Int
cisgrsResponseStatus = Lens.lens (responseStatus :: CreateInputSecurityGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateInputSecurityGroupResponse)
{-# DEPRECATED cisgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
