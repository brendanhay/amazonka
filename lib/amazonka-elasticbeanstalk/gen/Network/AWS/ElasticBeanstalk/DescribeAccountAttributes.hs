{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribeAccountAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns attributes related to AWS Elastic Beanstalk that are associated with the calling AWS account.
--
-- The result currently has one set of attributes—resource quotas.
module Network.AWS.ElasticBeanstalk.DescribeAccountAttributes
  ( -- * Creating a request
    DescribeAccountAttributes (..),
    mkDescribeAccountAttributes,

    -- * Destructuring the response
    DescribeAccountAttributesResponse (..),
    mkDescribeAccountAttributesResponse,

    -- ** Response lenses
    daarsResourceQuotas,
    daarsResponseStatus,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAccountAttributes' smart constructor.
data DescribeAccountAttributes = DescribeAccountAttributes'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAccountAttributes' with the minimum fields required to make a request.
mkDescribeAccountAttributes ::
  DescribeAccountAttributes
mkDescribeAccountAttributes = DescribeAccountAttributes'

instance Lude.AWSRequest DescribeAccountAttributes where
  type
    Rs DescribeAccountAttributes =
      DescribeAccountAttributesResponse
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "DescribeAccountAttributesResult"
      ( \s h x ->
          DescribeAccountAttributesResponse'
            Lude.<$> (x Lude..@? "ResourceQuotas")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAccountAttributes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeAccountAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAccountAttributes where
  toQuery =
    Lude.const
      ( Lude.mconcat
          [ "Action" Lude.=: ("DescribeAccountAttributes" :: Lude.ByteString),
            "Version" Lude.=: ("2010-12-01" :: Lude.ByteString)
          ]
      )

-- | /See:/ 'mkDescribeAccountAttributesResponse' smart constructor.
data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse'
  { resourceQuotas ::
      Lude.Maybe
        ResourceQuotas,
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

-- | Creates a value of 'DescribeAccountAttributesResponse' with the minimum fields required to make a request.
--
-- * 'resourceQuotas' - The Elastic Beanstalk resource quotas associated with the calling AWS account.
-- * 'responseStatus' - The response status code.
mkDescribeAccountAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAccountAttributesResponse
mkDescribeAccountAttributesResponse pResponseStatus_ =
  DescribeAccountAttributesResponse'
    { resourceQuotas = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Elastic Beanstalk resource quotas associated with the calling AWS account.
--
-- /Note:/ Consider using 'resourceQuotas' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarsResourceQuotas :: Lens.Lens' DescribeAccountAttributesResponse (Lude.Maybe ResourceQuotas)
daarsResourceQuotas = Lens.lens (resourceQuotas :: DescribeAccountAttributesResponse -> Lude.Maybe ResourceQuotas) (\s a -> s {resourceQuotas = a} :: DescribeAccountAttributesResponse)
{-# DEPRECATED daarsResourceQuotas "Use generic-lens or generic-optics with 'resourceQuotas' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarsResponseStatus :: Lens.Lens' DescribeAccountAttributesResponse Lude.Int
daarsResponseStatus = Lens.lens (responseStatus :: DescribeAccountAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAccountAttributesResponse)
{-# DEPRECATED daarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
