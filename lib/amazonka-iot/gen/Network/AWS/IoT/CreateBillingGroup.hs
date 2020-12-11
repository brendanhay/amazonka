{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateBillingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a billing group.
module Network.AWS.IoT.CreateBillingGroup
  ( -- * Creating a request
    CreateBillingGroup (..),
    mkCreateBillingGroup,

    -- ** Request lenses
    cbgBillingGroupProperties,
    cbgTags,
    cbgBillingGroupName,

    -- * Destructuring the response
    CreateBillingGroupResponse (..),
    mkCreateBillingGroupResponse,

    -- ** Response lenses
    cbgrsBillingGroupARN,
    cbgrsBillingGroupName,
    cbgrsBillingGroupId,
    cbgrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateBillingGroup' smart constructor.
data CreateBillingGroup = CreateBillingGroup'
  { billingGroupProperties ::
      Lude.Maybe BillingGroupProperties,
    tags :: Lude.Maybe [Tag],
    billingGroupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateBillingGroup' with the minimum fields required to make a request.
--
-- * 'billingGroupName' - The name you wish to give to the billing group.
-- * 'billingGroupProperties' - The properties of the billing group.
-- * 'tags' - Metadata which can be used to manage the billing group.
mkCreateBillingGroup ::
  -- | 'billingGroupName'
  Lude.Text ->
  CreateBillingGroup
mkCreateBillingGroup pBillingGroupName_ =
  CreateBillingGroup'
    { billingGroupProperties = Lude.Nothing,
      tags = Lude.Nothing,
      billingGroupName = pBillingGroupName_
    }

-- | The properties of the billing group.
--
-- /Note:/ Consider using 'billingGroupProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbgBillingGroupProperties :: Lens.Lens' CreateBillingGroup (Lude.Maybe BillingGroupProperties)
cbgBillingGroupProperties = Lens.lens (billingGroupProperties :: CreateBillingGroup -> Lude.Maybe BillingGroupProperties) (\s a -> s {billingGroupProperties = a} :: CreateBillingGroup)
{-# DEPRECATED cbgBillingGroupProperties "Use generic-lens or generic-optics with 'billingGroupProperties' instead." #-}

-- | Metadata which can be used to manage the billing group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbgTags :: Lens.Lens' CreateBillingGroup (Lude.Maybe [Tag])
cbgTags = Lens.lens (tags :: CreateBillingGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateBillingGroup)
{-# DEPRECATED cbgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name you wish to give to the billing group.
--
-- /Note:/ Consider using 'billingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbgBillingGroupName :: Lens.Lens' CreateBillingGroup Lude.Text
cbgBillingGroupName = Lens.lens (billingGroupName :: CreateBillingGroup -> Lude.Text) (\s a -> s {billingGroupName = a} :: CreateBillingGroup)
{-# DEPRECATED cbgBillingGroupName "Use generic-lens or generic-optics with 'billingGroupName' instead." #-}

instance Lude.AWSRequest CreateBillingGroup where
  type Rs CreateBillingGroup = CreateBillingGroupResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateBillingGroupResponse'
            Lude.<$> (x Lude..?> "billingGroupArn")
            Lude.<*> (x Lude..?> "billingGroupName")
            Lude.<*> (x Lude..?> "billingGroupId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateBillingGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateBillingGroup where
  toJSON CreateBillingGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("billingGroupProperties" Lude..=)
              Lude.<$> billingGroupProperties,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateBillingGroup where
  toPath CreateBillingGroup' {..} =
    Lude.mconcat ["/billing-groups/", Lude.toBS billingGroupName]

instance Lude.ToQuery CreateBillingGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateBillingGroupResponse' smart constructor.
data CreateBillingGroupResponse = CreateBillingGroupResponse'
  { billingGroupARN ::
      Lude.Maybe Lude.Text,
    billingGroupName ::
      Lude.Maybe Lude.Text,
    billingGroupId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateBillingGroupResponse' with the minimum fields required to make a request.
--
-- * 'billingGroupARN' - The ARN of the billing group.
-- * 'billingGroupId' - The ID of the billing group.
-- * 'billingGroupName' - The name you gave to the billing group.
-- * 'responseStatus' - The response status code.
mkCreateBillingGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateBillingGroupResponse
mkCreateBillingGroupResponse pResponseStatus_ =
  CreateBillingGroupResponse'
    { billingGroupARN = Lude.Nothing,
      billingGroupName = Lude.Nothing,
      billingGroupId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the billing group.
--
-- /Note:/ Consider using 'billingGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbgrsBillingGroupARN :: Lens.Lens' CreateBillingGroupResponse (Lude.Maybe Lude.Text)
cbgrsBillingGroupARN = Lens.lens (billingGroupARN :: CreateBillingGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {billingGroupARN = a} :: CreateBillingGroupResponse)
{-# DEPRECATED cbgrsBillingGroupARN "Use generic-lens or generic-optics with 'billingGroupARN' instead." #-}

-- | The name you gave to the billing group.
--
-- /Note:/ Consider using 'billingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbgrsBillingGroupName :: Lens.Lens' CreateBillingGroupResponse (Lude.Maybe Lude.Text)
cbgrsBillingGroupName = Lens.lens (billingGroupName :: CreateBillingGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {billingGroupName = a} :: CreateBillingGroupResponse)
{-# DEPRECATED cbgrsBillingGroupName "Use generic-lens or generic-optics with 'billingGroupName' instead." #-}

-- | The ID of the billing group.
--
-- /Note:/ Consider using 'billingGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbgrsBillingGroupId :: Lens.Lens' CreateBillingGroupResponse (Lude.Maybe Lude.Text)
cbgrsBillingGroupId = Lens.lens (billingGroupId :: CreateBillingGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {billingGroupId = a} :: CreateBillingGroupResponse)
{-# DEPRECATED cbgrsBillingGroupId "Use generic-lens or generic-optics with 'billingGroupId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbgrsResponseStatus :: Lens.Lens' CreateBillingGroupResponse Lude.Int
cbgrsResponseStatus = Lens.lens (responseStatus :: CreateBillingGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateBillingGroupResponse)
{-# DEPRECATED cbgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
