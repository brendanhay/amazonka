{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dbgbrsBillingGroupARN,
    dbgbrsVersion,
    dbgbrsBillingGroupProperties,
    dbgbrsBillingGroupName,
    dbgbrsBillingGroupId,
    dbgbrsBillingGroupMetadata,
    dbgbrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeBillingGroup' smart constructor.
newtype DescribeBillingGroup = DescribeBillingGroup'
  { billingGroupName ::
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
  { billingGroupARN ::
      Lude.Maybe Lude.Text,
    version ::
      Lude.Maybe Lude.Integer,
    billingGroupProperties ::
      Lude.Maybe BillingGroupProperties,
    billingGroupName ::
      Lude.Maybe Lude.Text,
    billingGroupId ::
      Lude.Maybe Lude.Text,
    billingGroupMetadata ::
      Lude.Maybe BillingGroupMetadata,
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

-- | Creates a value of 'DescribeBillingGroupResponse' with the minimum fields required to make a request.
--
-- * 'billingGroupARN' - The ARN of the billing group.
-- * 'billingGroupId' - The ID of the billing group.
-- * 'billingGroupMetadata' - Additional information about the billing group.
-- * 'billingGroupName' - The name of the billing group.
-- * 'billingGroupProperties' - The properties of the billing group.
-- * 'responseStatus' - The response status code.
-- * 'version' - The version of the billing group.
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
dbgbrsBillingGroupARN :: Lens.Lens' DescribeBillingGroupResponse (Lude.Maybe Lude.Text)
dbgbrsBillingGroupARN = Lens.lens (billingGroupARN :: DescribeBillingGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {billingGroupARN = a} :: DescribeBillingGroupResponse)
{-# DEPRECATED dbgbrsBillingGroupARN "Use generic-lens or generic-optics with 'billingGroupARN' instead." #-}

-- | The version of the billing group.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgbrsVersion :: Lens.Lens' DescribeBillingGroupResponse (Lude.Maybe Lude.Integer)
dbgbrsVersion = Lens.lens (version :: DescribeBillingGroupResponse -> Lude.Maybe Lude.Integer) (\s a -> s {version = a} :: DescribeBillingGroupResponse)
{-# DEPRECATED dbgbrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The properties of the billing group.
--
-- /Note:/ Consider using 'billingGroupProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgbrsBillingGroupProperties :: Lens.Lens' DescribeBillingGroupResponse (Lude.Maybe BillingGroupProperties)
dbgbrsBillingGroupProperties = Lens.lens (billingGroupProperties :: DescribeBillingGroupResponse -> Lude.Maybe BillingGroupProperties) (\s a -> s {billingGroupProperties = a} :: DescribeBillingGroupResponse)
{-# DEPRECATED dbgbrsBillingGroupProperties "Use generic-lens or generic-optics with 'billingGroupProperties' instead." #-}

-- | The name of the billing group.
--
-- /Note:/ Consider using 'billingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgbrsBillingGroupName :: Lens.Lens' DescribeBillingGroupResponse (Lude.Maybe Lude.Text)
dbgbrsBillingGroupName = Lens.lens (billingGroupName :: DescribeBillingGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {billingGroupName = a} :: DescribeBillingGroupResponse)
{-# DEPRECATED dbgbrsBillingGroupName "Use generic-lens or generic-optics with 'billingGroupName' instead." #-}

-- | The ID of the billing group.
--
-- /Note:/ Consider using 'billingGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgbrsBillingGroupId :: Lens.Lens' DescribeBillingGroupResponse (Lude.Maybe Lude.Text)
dbgbrsBillingGroupId = Lens.lens (billingGroupId :: DescribeBillingGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {billingGroupId = a} :: DescribeBillingGroupResponse)
{-# DEPRECATED dbgbrsBillingGroupId "Use generic-lens or generic-optics with 'billingGroupId' instead." #-}

-- | Additional information about the billing group.
--
-- /Note:/ Consider using 'billingGroupMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgbrsBillingGroupMetadata :: Lens.Lens' DescribeBillingGroupResponse (Lude.Maybe BillingGroupMetadata)
dbgbrsBillingGroupMetadata = Lens.lens (billingGroupMetadata :: DescribeBillingGroupResponse -> Lude.Maybe BillingGroupMetadata) (\s a -> s {billingGroupMetadata = a} :: DescribeBillingGroupResponse)
{-# DEPRECATED dbgbrsBillingGroupMetadata "Use generic-lens or generic-optics with 'billingGroupMetadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgbrsResponseStatus :: Lens.Lens' DescribeBillingGroupResponse Lude.Int
dbgbrsResponseStatus = Lens.lens (responseStatus :: DescribeBillingGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeBillingGroupResponse)
{-# DEPRECATED dbgbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
