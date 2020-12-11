{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.UpdateInputSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an Input Security Group's Whilelists.
module Network.AWS.MediaLive.UpdateInputSecurityGroup
  ( -- * Creating a request
    UpdateInputSecurityGroup (..),
    mkUpdateInputSecurityGroup,

    -- ** Request lenses
    uisgWhitelistRules,
    uisgTags,
    uisgInputSecurityGroupId,

    -- * Destructuring the response
    UpdateInputSecurityGroupResponse (..),
    mkUpdateInputSecurityGroupResponse,

    -- ** Response lenses
    uisgrsSecurityGroup,
    uisgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request to update some combination of the Input Security Group name and the IPv4 CIDRs the Input Security Group should allow.
--
-- /See:/ 'mkUpdateInputSecurityGroup' smart constructor.
data UpdateInputSecurityGroup = UpdateInputSecurityGroup'
  { whitelistRules ::
      Lude.Maybe [InputWhitelistRuleCidr],
    tags ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text)),
    inputSecurityGroupId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateInputSecurityGroup' with the minimum fields required to make a request.
--
-- * 'inputSecurityGroupId' - The id of the Input Security Group to update.
-- * 'tags' - A collection of key-value pairs.
-- * 'whitelistRules' - List of IPv4 CIDR addresses to whitelist
mkUpdateInputSecurityGroup ::
  -- | 'inputSecurityGroupId'
  Lude.Text ->
  UpdateInputSecurityGroup
mkUpdateInputSecurityGroup pInputSecurityGroupId_ =
  UpdateInputSecurityGroup'
    { whitelistRules = Lude.Nothing,
      tags = Lude.Nothing,
      inputSecurityGroupId = pInputSecurityGroupId_
    }

-- | List of IPv4 CIDR addresses to whitelist
--
-- /Note:/ Consider using 'whitelistRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uisgWhitelistRules :: Lens.Lens' UpdateInputSecurityGroup (Lude.Maybe [InputWhitelistRuleCidr])
uisgWhitelistRules = Lens.lens (whitelistRules :: UpdateInputSecurityGroup -> Lude.Maybe [InputWhitelistRuleCidr]) (\s a -> s {whitelistRules = a} :: UpdateInputSecurityGroup)
{-# DEPRECATED uisgWhitelistRules "Use generic-lens or generic-optics with 'whitelistRules' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uisgTags :: Lens.Lens' UpdateInputSecurityGroup (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
uisgTags = Lens.lens (tags :: UpdateInputSecurityGroup -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: UpdateInputSecurityGroup)
{-# DEPRECATED uisgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The id of the Input Security Group to update.
--
-- /Note:/ Consider using 'inputSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uisgInputSecurityGroupId :: Lens.Lens' UpdateInputSecurityGroup Lude.Text
uisgInputSecurityGroupId = Lens.lens (inputSecurityGroupId :: UpdateInputSecurityGroup -> Lude.Text) (\s a -> s {inputSecurityGroupId = a} :: UpdateInputSecurityGroup)
{-# DEPRECATED uisgInputSecurityGroupId "Use generic-lens or generic-optics with 'inputSecurityGroupId' instead." #-}

instance Lude.AWSRequest UpdateInputSecurityGroup where
  type Rs UpdateInputSecurityGroup = UpdateInputSecurityGroupResponse
  request = Req.putJSON mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateInputSecurityGroupResponse'
            Lude.<$> (x Lude..?> "securityGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateInputSecurityGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateInputSecurityGroup where
  toJSON UpdateInputSecurityGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("whitelistRules" Lude..=) Lude.<$> whitelistRules,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath UpdateInputSecurityGroup where
  toPath UpdateInputSecurityGroup' {..} =
    Lude.mconcat
      ["/prod/inputSecurityGroups/", Lude.toBS inputSecurityGroupId]

instance Lude.ToQuery UpdateInputSecurityGroup where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for UpdateInputSecurityGroupResponse
--
-- /See:/ 'mkUpdateInputSecurityGroupResponse' smart constructor.
data UpdateInputSecurityGroupResponse = UpdateInputSecurityGroupResponse'
  { securityGroup ::
      Lude.Maybe
        InputSecurityGroup,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateInputSecurityGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'securityGroup' - Undocumented field.
mkUpdateInputSecurityGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateInputSecurityGroupResponse
mkUpdateInputSecurityGroupResponse pResponseStatus_ =
  UpdateInputSecurityGroupResponse'
    { securityGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'securityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uisgrsSecurityGroup :: Lens.Lens' UpdateInputSecurityGroupResponse (Lude.Maybe InputSecurityGroup)
uisgrsSecurityGroup = Lens.lens (securityGroup :: UpdateInputSecurityGroupResponse -> Lude.Maybe InputSecurityGroup) (\s a -> s {securityGroup = a} :: UpdateInputSecurityGroupResponse)
{-# DEPRECATED uisgrsSecurityGroup "Use generic-lens or generic-optics with 'securityGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uisgrsResponseStatus :: Lens.Lens' UpdateInputSecurityGroupResponse Lude.Int
uisgrsResponseStatus = Lens.lens (responseStatus :: UpdateInputSecurityGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateInputSecurityGroupResponse)
{-# DEPRECATED uisgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
