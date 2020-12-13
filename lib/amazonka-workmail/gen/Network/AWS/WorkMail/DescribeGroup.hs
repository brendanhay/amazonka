{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.DescribeGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the data available for the group.
module Network.AWS.WorkMail.DescribeGroup
  ( -- * Creating a request
    DescribeGroup (..),
    mkDescribeGroup,

    -- ** Request lenses
    dgfGroupId,
    dgfOrganizationId,

    -- * Destructuring the response
    DescribeGroupResponse (..),
    mkDescribeGroupResponse,

    -- ** Response lenses
    dgfrsEmail,
    dgfrsState,
    dgfrsDisabledDate,
    dgfrsName,
    dgfrsGroupId,
    dgfrsEnabledDate,
    dgfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkDescribeGroup' smart constructor.
data DescribeGroup = DescribeGroup'
  { -- | The identifier for the group to be described.
    groupId :: Lude.Text,
    -- | The identifier for the organization under which the group exists.
    organizationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeGroup' with the minimum fields required to make a request.
--
-- * 'groupId' - The identifier for the group to be described.
-- * 'organizationId' - The identifier for the organization under which the group exists.
mkDescribeGroup ::
  -- | 'groupId'
  Lude.Text ->
  -- | 'organizationId'
  Lude.Text ->
  DescribeGroup
mkDescribeGroup pGroupId_ pOrganizationId_ =
  DescribeGroup'
    { groupId = pGroupId_,
      organizationId = pOrganizationId_
    }

-- | The identifier for the group to be described.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgfGroupId :: Lens.Lens' DescribeGroup Lude.Text
dgfGroupId = Lens.lens (groupId :: DescribeGroup -> Lude.Text) (\s a -> s {groupId = a} :: DescribeGroup)
{-# DEPRECATED dgfGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The identifier for the organization under which the group exists.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgfOrganizationId :: Lens.Lens' DescribeGroup Lude.Text
dgfOrganizationId = Lens.lens (organizationId :: DescribeGroup -> Lude.Text) (\s a -> s {organizationId = a} :: DescribeGroup)
{-# DEPRECATED dgfOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Lude.AWSRequest DescribeGroup where
  type Rs DescribeGroup = DescribeGroupResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeGroupResponse'
            Lude.<$> (x Lude..?> "Email")
            Lude.<*> (x Lude..?> "State")
            Lude.<*> (x Lude..?> "DisabledDate")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "GroupId")
            Lude.<*> (x Lude..?> "EnabledDate")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.DescribeGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeGroup where
  toJSON DescribeGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GroupId" Lude..= groupId),
            Lude.Just ("OrganizationId" Lude..= organizationId)
          ]
      )

instance Lude.ToPath DescribeGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeGroupResponse' smart constructor.
data DescribeGroupResponse = DescribeGroupResponse'
  { -- | The email of the described group.
    email :: Lude.Maybe Lude.Text,
    -- | The state of the user: enabled (registered to Amazon WorkMail) or disabled (deregistered or never registered to WorkMail).
    state :: Lude.Maybe EntityState,
    -- | The date and time when a user was deregistered from WorkMail, in UNIX epoch time format.
    disabledDate :: Lude.Maybe Lude.Timestamp,
    -- | The name of the described group.
    name :: Lude.Maybe Lude.Text,
    -- | The identifier of the described group.
    groupId :: Lude.Maybe Lude.Text,
    -- | The date and time when a user was registered to WorkMail, in UNIX epoch time format.
    enabledDate :: Lude.Maybe Lude.Timestamp,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeGroupResponse' with the minimum fields required to make a request.
--
-- * 'email' - The email of the described group.
-- * 'state' - The state of the user: enabled (registered to Amazon WorkMail) or disabled (deregistered or never registered to WorkMail).
-- * 'disabledDate' - The date and time when a user was deregistered from WorkMail, in UNIX epoch time format.
-- * 'name' - The name of the described group.
-- * 'groupId' - The identifier of the described group.
-- * 'enabledDate' - The date and time when a user was registered to WorkMail, in UNIX epoch time format.
-- * 'responseStatus' - The response status code.
mkDescribeGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeGroupResponse
mkDescribeGroupResponse pResponseStatus_ =
  DescribeGroupResponse'
    { email = Lude.Nothing,
      state = Lude.Nothing,
      disabledDate = Lude.Nothing,
      name = Lude.Nothing,
      groupId = Lude.Nothing,
      enabledDate = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The email of the described group.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgfrsEmail :: Lens.Lens' DescribeGroupResponse (Lude.Maybe Lude.Text)
dgfrsEmail = Lens.lens (email :: DescribeGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {email = a} :: DescribeGroupResponse)
{-# DEPRECATED dgfrsEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The state of the user: enabled (registered to Amazon WorkMail) or disabled (deregistered or never registered to WorkMail).
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgfrsState :: Lens.Lens' DescribeGroupResponse (Lude.Maybe EntityState)
dgfrsState = Lens.lens (state :: DescribeGroupResponse -> Lude.Maybe EntityState) (\s a -> s {state = a} :: DescribeGroupResponse)
{-# DEPRECATED dgfrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The date and time when a user was deregistered from WorkMail, in UNIX epoch time format.
--
-- /Note:/ Consider using 'disabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgfrsDisabledDate :: Lens.Lens' DescribeGroupResponse (Lude.Maybe Lude.Timestamp)
dgfrsDisabledDate = Lens.lens (disabledDate :: DescribeGroupResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {disabledDate = a} :: DescribeGroupResponse)
{-# DEPRECATED dgfrsDisabledDate "Use generic-lens or generic-optics with 'disabledDate' instead." #-}

-- | The name of the described group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgfrsName :: Lens.Lens' DescribeGroupResponse (Lude.Maybe Lude.Text)
dgfrsName = Lens.lens (name :: DescribeGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DescribeGroupResponse)
{-# DEPRECATED dgfrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the described group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgfrsGroupId :: Lens.Lens' DescribeGroupResponse (Lude.Maybe Lude.Text)
dgfrsGroupId = Lens.lens (groupId :: DescribeGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {groupId = a} :: DescribeGroupResponse)
{-# DEPRECATED dgfrsGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The date and time when a user was registered to WorkMail, in UNIX epoch time format.
--
-- /Note:/ Consider using 'enabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgfrsEnabledDate :: Lens.Lens' DescribeGroupResponse (Lude.Maybe Lude.Timestamp)
dgfrsEnabledDate = Lens.lens (enabledDate :: DescribeGroupResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {enabledDate = a} :: DescribeGroupResponse)
{-# DEPRECATED dgfrsEnabledDate "Use generic-lens or generic-optics with 'enabledDate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgfrsResponseStatus :: Lens.Lens' DescribeGroupResponse Lude.Int
dgfrsResponseStatus = Lens.lens (responseStatus :: DescribeGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeGroupResponse)
{-# DEPRECATED dgfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
