{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DescribeStackSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of the specified stack set.
module Network.AWS.CloudFormation.DescribeStackSet
  ( -- * Creating a request
    DescribeStackSet (..),
    mkDescribeStackSet,

    -- ** Request lenses
    dssStackSetName,

    -- * Destructuring the response
    DescribeStackSetResponse (..),
    mkDescribeStackSetResponse,

    -- ** Response lenses
    dssfrsStackSet,
    dssfrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeStackSet' smart constructor.
newtype DescribeStackSet = DescribeStackSet'
  { -- | The name or unique ID of the stack set whose description you want.
    stackSetName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStackSet' with the minimum fields required to make a request.
--
-- * 'stackSetName' - The name or unique ID of the stack set whose description you want.
mkDescribeStackSet ::
  -- | 'stackSetName'
  Lude.Text ->
  DescribeStackSet
mkDescribeStackSet pStackSetName_ =
  DescribeStackSet' {stackSetName = pStackSetName_}

-- | The name or unique ID of the stack set whose description you want.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssStackSetName :: Lens.Lens' DescribeStackSet Lude.Text
dssStackSetName = Lens.lens (stackSetName :: DescribeStackSet -> Lude.Text) (\s a -> s {stackSetName = a} :: DescribeStackSet)
{-# DEPRECATED dssStackSetName "Use generic-lens or generic-optics with 'stackSetName' instead." #-}

instance Lude.AWSRequest DescribeStackSet where
  type Rs DescribeStackSet = DescribeStackSetResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "DescribeStackSetResult"
      ( \s h x ->
          DescribeStackSetResponse'
            Lude.<$> (x Lude..@? "StackSet") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeStackSet where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeStackSet where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeStackSet where
  toQuery DescribeStackSet' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeStackSet" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "StackSetName" Lude.=: stackSetName
      ]

-- | /See:/ 'mkDescribeStackSetResponse' smart constructor.
data DescribeStackSetResponse = DescribeStackSetResponse'
  { -- | The specified stack set.
    stackSet :: Lude.Maybe StackSet,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStackSetResponse' with the minimum fields required to make a request.
--
-- * 'stackSet' - The specified stack set.
-- * 'responseStatus' - The response status code.
mkDescribeStackSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeStackSetResponse
mkDescribeStackSetResponse pResponseStatus_ =
  DescribeStackSetResponse'
    { stackSet = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The specified stack set.
--
-- /Note:/ Consider using 'stackSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssfrsStackSet :: Lens.Lens' DescribeStackSetResponse (Lude.Maybe StackSet)
dssfrsStackSet = Lens.lens (stackSet :: DescribeStackSetResponse -> Lude.Maybe StackSet) (\s a -> s {stackSet = a} :: DescribeStackSetResponse)
{-# DEPRECATED dssfrsStackSet "Use generic-lens or generic-optics with 'stackSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssfrsResponseStatus :: Lens.Lens' DescribeStackSetResponse Lude.Int
dssfrsResponseStatus = Lens.lens (responseStatus :: DescribeStackSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeStackSetResponse)
{-# DEPRECATED dssfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
