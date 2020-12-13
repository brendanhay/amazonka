{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.ListAvailableSolutionStacks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the available solution stack names, with the public version first and then in reverse chronological order.
module Network.AWS.ElasticBeanstalk.ListAvailableSolutionStacks
  ( -- * Creating a request
    ListAvailableSolutionStacks (..),
    mkListAvailableSolutionStacks,

    -- * Destructuring the response
    ListAvailableSolutionStacksResponse (..),
    mkListAvailableSolutionStacksResponse,

    -- ** Response lenses
    lassrsSolutionStacks,
    lassrsSolutionStackDetails,
    lassrsResponseStatus,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAvailableSolutionStacks' smart constructor.
data ListAvailableSolutionStacks = ListAvailableSolutionStacks'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAvailableSolutionStacks' with the minimum fields required to make a request.
mkListAvailableSolutionStacks ::
  ListAvailableSolutionStacks
mkListAvailableSolutionStacks = ListAvailableSolutionStacks'

instance Lude.AWSRequest ListAvailableSolutionStacks where
  type
    Rs ListAvailableSolutionStacks =
      ListAvailableSolutionStacksResponse
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "ListAvailableSolutionStacksResult"
      ( \s h x ->
          ListAvailableSolutionStacksResponse'
            Lude.<$> ( x Lude..@? "SolutionStacks" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> ( x Lude..@? "SolutionStackDetails" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAvailableSolutionStacks where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListAvailableSolutionStacks where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAvailableSolutionStacks where
  toQuery =
    Lude.const
      ( Lude.mconcat
          [ "Action"
              Lude.=: ("ListAvailableSolutionStacks" :: Lude.ByteString),
            "Version" Lude.=: ("2010-12-01" :: Lude.ByteString)
          ]
      )

-- | A list of available AWS Elastic Beanstalk solution stacks.
--
-- /See:/ 'mkListAvailableSolutionStacksResponse' smart constructor.
data ListAvailableSolutionStacksResponse = ListAvailableSolutionStacksResponse'
  { -- | A list of available solution stacks.
    solutionStacks :: Lude.Maybe [Lude.Text],
    -- | A list of available solution stacks and their 'SolutionStackDescription' .
    solutionStackDetails :: Lude.Maybe [SolutionStackDescription],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAvailableSolutionStacksResponse' with the minimum fields required to make a request.
--
-- * 'solutionStacks' - A list of available solution stacks.
-- * 'solutionStackDetails' - A list of available solution stacks and their 'SolutionStackDescription' .
-- * 'responseStatus' - The response status code.
mkListAvailableSolutionStacksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAvailableSolutionStacksResponse
mkListAvailableSolutionStacksResponse pResponseStatus_ =
  ListAvailableSolutionStacksResponse'
    { solutionStacks =
        Lude.Nothing,
      solutionStackDetails = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of available solution stacks.
--
-- /Note:/ Consider using 'solutionStacks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lassrsSolutionStacks :: Lens.Lens' ListAvailableSolutionStacksResponse (Lude.Maybe [Lude.Text])
lassrsSolutionStacks = Lens.lens (solutionStacks :: ListAvailableSolutionStacksResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {solutionStacks = a} :: ListAvailableSolutionStacksResponse)
{-# DEPRECATED lassrsSolutionStacks "Use generic-lens or generic-optics with 'solutionStacks' instead." #-}

-- | A list of available solution stacks and their 'SolutionStackDescription' .
--
-- /Note:/ Consider using 'solutionStackDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lassrsSolutionStackDetails :: Lens.Lens' ListAvailableSolutionStacksResponse (Lude.Maybe [SolutionStackDescription])
lassrsSolutionStackDetails = Lens.lens (solutionStackDetails :: ListAvailableSolutionStacksResponse -> Lude.Maybe [SolutionStackDescription]) (\s a -> s {solutionStackDetails = a} :: ListAvailableSolutionStacksResponse)
{-# DEPRECATED lassrsSolutionStackDetails "Use generic-lens or generic-optics with 'solutionStackDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lassrsResponseStatus :: Lens.Lens' ListAvailableSolutionStacksResponse Lude.Int
lassrsResponseStatus = Lens.lens (responseStatus :: ListAvailableSolutionStacksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAvailableSolutionStacksResponse)
{-# DEPRECATED lassrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
