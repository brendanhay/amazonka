{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dgOrganizationId,
    dgGroupId,

    -- * Destructuring the response
    DescribeGroupResponse (..),
    mkDescribeGroupResponse,

    -- ** Response lenses
    desrsEmail,
    desrsState,
    desrsDisabledDate,
    desrsName,
    desrsGroupId,
    desrsEnabledDate,
    desrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkDescribeGroup' smart constructor.
data DescribeGroup = DescribeGroup'
  { organizationId :: Lude.Text,
    groupId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeGroup' with the minimum fields required to make a request.
--
-- * 'groupId' - The identifier for the group to be described.
-- * 'organizationId' - The identifier for the organization under which the group exists.
mkDescribeGroup ::
  -- | 'organizationId'
  Lude.Text ->
  -- | 'groupId'
  Lude.Text ->
  DescribeGroup
mkDescribeGroup pOrganizationId_ pGroupId_ =
  DescribeGroup'
    { organizationId = pOrganizationId_,
      groupId = pGroupId_
    }

-- | The identifier for the organization under which the group exists.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgOrganizationId :: Lens.Lens' DescribeGroup Lude.Text
dgOrganizationId = Lens.lens (organizationId :: DescribeGroup -> Lude.Text) (\s a -> s {organizationId = a} :: DescribeGroup)
{-# DEPRECATED dgOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The identifier for the group to be described.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgGroupId :: Lens.Lens' DescribeGroup Lude.Text
dgGroupId = Lens.lens (groupId :: DescribeGroup -> Lude.Text) (\s a -> s {groupId = a} :: DescribeGroup)
{-# DEPRECATED dgGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

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
          [ Lude.Just ("OrganizationId" Lude..= organizationId),
            Lude.Just ("GroupId" Lude..= groupId)
          ]
      )

instance Lude.ToPath DescribeGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeGroupResponse' smart constructor.
data DescribeGroupResponse = DescribeGroupResponse'
  { email ::
      Lude.Maybe Lude.Text,
    state :: Lude.Maybe EntityState,
    disabledDate :: Lude.Maybe Lude.Timestamp,
    name :: Lude.Maybe Lude.Text,
    groupId :: Lude.Maybe Lude.Text,
    enabledDate :: Lude.Maybe Lude.Timestamp,
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

-- | Creates a value of 'DescribeGroupResponse' with the minimum fields required to make a request.
--
-- * 'disabledDate' - The date and time when a user was deregistered from WorkMail, in UNIX epoch time format.
-- * 'email' - The email of the described group.
-- * 'enabledDate' - The date and time when a user was registered to WorkMail, in UNIX epoch time format.
-- * 'groupId' - The identifier of the described group.
-- * 'name' - The name of the described group.
-- * 'responseStatus' - The response status code.
-- * 'state' - The state of the user: enabled (registered to Amazon WorkMail) or disabled (deregistered or never registered to WorkMail).
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
desrsEmail :: Lens.Lens' DescribeGroupResponse (Lude.Maybe Lude.Text)
desrsEmail = Lens.lens (email :: DescribeGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {email = a} :: DescribeGroupResponse)
{-# DEPRECATED desrsEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The state of the user: enabled (registered to Amazon WorkMail) or disabled (deregistered or never registered to WorkMail).
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsState :: Lens.Lens' DescribeGroupResponse (Lude.Maybe EntityState)
desrsState = Lens.lens (state :: DescribeGroupResponse -> Lude.Maybe EntityState) (\s a -> s {state = a} :: DescribeGroupResponse)
{-# DEPRECATED desrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The date and time when a user was deregistered from WorkMail, in UNIX epoch time format.
--
-- /Note:/ Consider using 'disabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsDisabledDate :: Lens.Lens' DescribeGroupResponse (Lude.Maybe Lude.Timestamp)
desrsDisabledDate = Lens.lens (disabledDate :: DescribeGroupResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {disabledDate = a} :: DescribeGroupResponse)
{-# DEPRECATED desrsDisabledDate "Use generic-lens or generic-optics with 'disabledDate' instead." #-}

-- | The name of the described group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsName :: Lens.Lens' DescribeGroupResponse (Lude.Maybe Lude.Text)
desrsName = Lens.lens (name :: DescribeGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DescribeGroupResponse)
{-# DEPRECATED desrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the described group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsGroupId :: Lens.Lens' DescribeGroupResponse (Lude.Maybe Lude.Text)
desrsGroupId = Lens.lens (groupId :: DescribeGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {groupId = a} :: DescribeGroupResponse)
{-# DEPRECATED desrsGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The date and time when a user was registered to WorkMail, in UNIX epoch time format.
--
-- /Note:/ Consider using 'enabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsEnabledDate :: Lens.Lens' DescribeGroupResponse (Lude.Maybe Lude.Timestamp)
desrsEnabledDate = Lens.lens (enabledDate :: DescribeGroupResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {enabledDate = a} :: DescribeGroupResponse)
{-# DEPRECATED desrsEnabledDate "Use generic-lens or generic-optics with 'enabledDate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsResponseStatus :: Lens.Lens' DescribeGroupResponse Lude.Int
desrsResponseStatus = Lens.lens (responseStatus :: DescribeGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeGroupResponse)
{-# DEPRECATED desrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
