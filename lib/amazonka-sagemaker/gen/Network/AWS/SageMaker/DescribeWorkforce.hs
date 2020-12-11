{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeWorkforce
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists private workforce information, including workforce name, Amazon Resource Name (ARN), and, if applicable, allowed IP address ranges (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> ). Allowable IP address ranges are the IP addresses that workers can use to access tasks.
--
-- /Important:/ This operation applies only to private workforces.
module Network.AWS.SageMaker.DescribeWorkforce
  ( -- * Creating a request
    DescribeWorkforce (..),
    mkDescribeWorkforce,

    -- ** Request lenses
    dWorkforceName,

    -- * Destructuring the response
    DescribeWorkforceResponse (..),
    mkDescribeWorkforceResponse,

    -- ** Response lenses
    dwrsResponseStatus,
    dwrsWorkforce,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeWorkforce' smart constructor.
newtype DescribeWorkforce = DescribeWorkforce'
  { workforceName ::
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

-- | Creates a value of 'DescribeWorkforce' with the minimum fields required to make a request.
--
-- * 'workforceName' - The name of the private workforce whose access you want to restrict. @WorkforceName@ is automatically set to @default@ when a workforce is created and cannot be modified.
mkDescribeWorkforce ::
  -- | 'workforceName'
  Lude.Text ->
  DescribeWorkforce
mkDescribeWorkforce pWorkforceName_ =
  DescribeWorkforce' {workforceName = pWorkforceName_}

-- | The name of the private workforce whose access you want to restrict. @WorkforceName@ is automatically set to @default@ when a workforce is created and cannot be modified.
--
-- /Note:/ Consider using 'workforceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dWorkforceName :: Lens.Lens' DescribeWorkforce Lude.Text
dWorkforceName = Lens.lens (workforceName :: DescribeWorkforce -> Lude.Text) (\s a -> s {workforceName = a} :: DescribeWorkforce)
{-# DEPRECATED dWorkforceName "Use generic-lens or generic-optics with 'workforceName' instead." #-}

instance Lude.AWSRequest DescribeWorkforce where
  type Rs DescribeWorkforce = DescribeWorkforceResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeWorkforceResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "Workforce")
      )

instance Lude.ToHeaders DescribeWorkforce where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DescribeWorkforce" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeWorkforce where
  toJSON DescribeWorkforce' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("WorkforceName" Lude..= workforceName)]
      )

instance Lude.ToPath DescribeWorkforce where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeWorkforce where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeWorkforceResponse' smart constructor.
data DescribeWorkforceResponse = DescribeWorkforceResponse'
  { responseStatus ::
      Lude.Int,
    workforce :: Workforce
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeWorkforceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'workforce' - A single private workforce, which is automatically created when you create your first private work team. You can create one private work force in each AWS Region. By default, any workforce-related API operation used in a specific region will apply to the workforce created in that region. To learn how to create a private workforce, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private.html Create a Private Workforce> .
mkDescribeWorkforceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'workforce'
  Workforce ->
  DescribeWorkforceResponse
mkDescribeWorkforceResponse pResponseStatus_ pWorkforce_ =
  DescribeWorkforceResponse'
    { responseStatus = pResponseStatus_,
      workforce = pWorkforce_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwrsResponseStatus :: Lens.Lens' DescribeWorkforceResponse Lude.Int
dwrsResponseStatus = Lens.lens (responseStatus :: DescribeWorkforceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeWorkforceResponse)
{-# DEPRECATED dwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A single private workforce, which is automatically created when you create your first private work team. You can create one private work force in each AWS Region. By default, any workforce-related API operation used in a specific region will apply to the workforce created in that region. To learn how to create a private workforce, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private.html Create a Private Workforce> .
--
-- /Note:/ Consider using 'workforce' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwrsWorkforce :: Lens.Lens' DescribeWorkforceResponse Workforce
dwrsWorkforce = Lens.lens (workforce :: DescribeWorkforceResponse -> Workforce) (\s a -> s {workforce = a} :: DescribeWorkforceResponse)
{-# DEPRECATED dwrsWorkforce "Use generic-lens or generic-optics with 'workforce' instead." #-}
