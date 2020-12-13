{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeBillingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a billing group.
module Network.AWS.IoT.DescribeBillingGroup
  ( -- * Creating a request
    DescribeBillingGroup (..),
    mkDescribeBillingGroup,

    -- ** Request lenses
    dBillingGroupName,

    -- * Destructuring the response
    DescribeBillingGroupResponse (..),
    mkDescribeBillingGroupResponse,

    -- ** Response lenses
    dbgfrsBillingGroupARN,
    dbgfrsVersion,
    dbgfrsBillingGroupProperties,
    dbgfrsBillingGroupName,
    dbgfrsBillingGroupId,
    dbgfrsBillingGroupMetadata,
    dbgfrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeBillingGroup' smart constructor.
newtype DescribeBillingGroup = DescribeBillingGroup'
  { -- | The name of the billing group.
    billingGroupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeBillingGroup' with the minimum fields required to make a request.
--
-- * 'billingGroupName' - The name of the billing group.
mkDescribeBillingGroup ::
  -- | 'billingGroupName'
  Lude.Text ->
  DescribeBillingGroup
mkDescribeBillingGroup pBillingGroupName_ =
  DescribeBillingGroup' {billingGroupName = pBillingGroupName_}

-- | The name of the billing group.
--
-- /Note:/ Consider using 'billingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dBillingGroupName :: Lens.Lens' DescribeBillingGroup Lude.Text
dBillingGroupName = Lens.lens (billingGroupName :: DescribeBillingGroup -> Lude.Text) (\s a -> s {billingGroupName = a} :: DescribeBillingGroup)
{-# DEPRECATED dBillingGroupName "Use generic-lens or generic-optics with 'billingGroupName' instead." #-}

instance Lude.AWSRequest DescribeBillingGroup where
  type Rs DescribeBillingGroup = DescribeBillingGroupResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeBillingGroupResponse'
            Lude.<$> (x Lude..?> "billingGroupArn")
            Lude.<*> (x Lude..?> "version")
            Lude.<*> (x Lude..?> "billingGroupProperties")
            Lude.<*> (x Lude..?> "billingGroupName")
            Lude.<*> (x Lude..?> "billingGroupId")
            Lude.<*> (x Lude..?> "billingGroupMetadata")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeBillingGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeBillingGroup where
  toPath DescribeBillingGroup' {..} =
    Lude.mconcat ["/billing-groups/", Lude.toBS billingGroupName]

instance Lude.ToQuery DescribeBillingGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeBillingGroupResponse' smart constructor.
data DescribeBillingGroupResponse = DescribeBillingGroupResponse'
  { -- | The ARN of the billing group.
    billingGroupARN :: Lude.Maybe Lude.Text,
    -- | The version of the billing group.
    version :: Lude.Maybe Lude.Integer,
    -- | The properties of the billing group.
    billingGroupProperties :: Lude.Maybe BillingGroupProperties,
    -- | The name of the billing group.
    billingGroupName :: Lude.Maybe Lude.Text,
    -- | The ID of the billing group.
    billingGroupId :: Lude.Maybe Lude.Text,
    -- | Additional information about the billing group.
    billingGroupMetadata :: Lude.Maybe BillingGroupMetadata,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeBillingGroupResponse' with the minimum fields required to make a request.
--
-- * 'billingGroupARN' - The ARN of the billing group.
-- * 'version' - The version of the billing group.
-- * 'billingGroupProperties' - The properties of the billing group.
-- * 'billingGroupName' - The name of the billing group.
-- * 'billingGroupId' - The ID of the billing group.
-- * 'billingGroupMetadata' - Additional information about the billing group.
-- * 'responseStatus' - The response status code.
mkDescribeBillingGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeBillingGroupResponse
mkDescribeBillingGroupResponse pResponseStatus_ =
  DescribeBillingGroupResponse'
    { billingGroupARN = Lude.Nothing,
      version = Lude.Nothing,
      billingGroupProperties = Lude.Nothing,
      billingGroupName = Lude.Nothing,
      billingGroupId = Lude.Nothing,
      billingGroupMetadata = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the billing group.
--
-- /Note:/ Consider using 'billingGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgfrsBillingGroupARN :: Lens.Lens' DescribeBillingGroupResponse (Lude.Maybe Lude.Text)
dbgfrsBillingGroupARN = Lens.lens (billingGroupARN :: DescribeBillingGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {billingGroupARN = a} :: DescribeBillingGroupResponse)
{-# DEPRECATED dbgfrsBillingGroupARN "Use generic-lens or generic-optics with 'billingGroupARN' instead." #-}

-- | The version of the billing group.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgfrsVersion :: Lens.Lens' DescribeBillingGroupResponse (Lude.Maybe Lude.Integer)
dbgfrsVersion = Lens.lens (version :: DescribeBillingGroupResponse -> Lude.Maybe Lude.Integer) (\s a -> s {version = a} :: DescribeBillingGroupResponse)
{-# DEPRECATED dbgfrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The properties of the billing group.
--
-- /Note:/ Consider using 'billingGroupProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgfrsBillingGroupProperties :: Lens.Lens' DescribeBillingGroupResponse (Lude.Maybe BillingGroupProperties)
dbgfrsBillingGroupProperties = Lens.lens (billingGroupProperties :: DescribeBillingGroupResponse -> Lude.Maybe BillingGroupProperties) (\s a -> s {billingGroupProperties = a} :: DescribeBillingGroupResponse)
{-# DEPRECATED dbgfrsBillingGroupProperties "Use generic-lens or generic-optics with 'billingGroupProperties' instead." #-}

-- | The name of the billing group.
--
-- /Note:/ Consider using 'billingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgfrsBillingGroupName :: Lens.Lens' DescribeBillingGroupResponse (Lude.Maybe Lude.Text)
dbgfrsBillingGroupName = Lens.lens (billingGroupName :: DescribeBillingGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {billingGroupName = a} :: DescribeBillingGroupResponse)
{-# DEPRECATED dbgfrsBillingGroupName "Use generic-lens or generic-optics with 'billingGroupName' instead." #-}

-- | The ID of the billing group.
--
-- /Note:/ Consider using 'billingGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgfrsBillingGroupId :: Lens.Lens' DescribeBillingGroupResponse (Lude.Maybe Lude.Text)
dbgfrsBillingGroupId = Lens.lens (billingGroupId :: DescribeBillingGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {billingGroupId = a} :: DescribeBillingGroupResponse)
{-# DEPRECATED dbgfrsBillingGroupId "Use generic-lens or generic-optics with 'billingGroupId' instead." #-}

-- | Additional information about the billing group.
--
-- /Note:/ Consider using 'billingGroupMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgfrsBillingGroupMetadata :: Lens.Lens' DescribeBillingGroupResponse (Lude.Maybe BillingGroupMetadata)
dbgfrsBillingGroupMetadata = Lens.lens (billingGroupMetadata :: DescribeBillingGroupResponse -> Lude.Maybe BillingGroupMetadata) (\s a -> s {billingGroupMetadata = a} :: DescribeBillingGroupResponse)
{-# DEPRECATED dbgfrsBillingGroupMetadata "Use generic-lens or generic-optics with 'billingGroupMetadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgfrsResponseStatus :: Lens.Lens' DescribeBillingGroupResponse Lude.Int
dbgfrsResponseStatus = Lens.lens (responseStatus :: DescribeBillingGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeBillingGroupResponse)
{-# DEPRECATED dbgfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
