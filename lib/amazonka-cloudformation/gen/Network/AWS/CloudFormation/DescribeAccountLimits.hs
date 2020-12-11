{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DescribeAccountLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves your account's AWS CloudFormation limits, such as the maximum number of stacks that you can create in your account. For more information about account limits, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/cloudformation-limits.html AWS CloudFormation Limits> in the /AWS CloudFormation User Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.DescribeAccountLimits
  ( -- * Creating a request
    DescribeAccountLimits (..),
    mkDescribeAccountLimits,

    -- ** Request lenses
    dalNextToken,

    -- * Destructuring the response
    DescribeAccountLimitsResponse (..),
    mkDescribeAccountLimitsResponse,

    -- ** Response lenses
    dalrsNextToken,
    dalrsAccountLimits,
    dalrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the 'DescribeAccountLimits' action.
--
-- /See:/ 'mkDescribeAccountLimits' smart constructor.
newtype DescribeAccountLimits = DescribeAccountLimits'
  { nextToken ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAccountLimits' with the minimum fields required to make a request.
--
-- * 'nextToken' - A string that identifies the next page of limits that you want to retrieve.
mkDescribeAccountLimits ::
  DescribeAccountLimits
mkDescribeAccountLimits =
  DescribeAccountLimits' {nextToken = Lude.Nothing}

-- | A string that identifies the next page of limits that you want to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dalNextToken :: Lens.Lens' DescribeAccountLimits (Lude.Maybe Lude.Text)
dalNextToken = Lens.lens (nextToken :: DescribeAccountLimits -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAccountLimits)
{-# DEPRECATED dalNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager DescribeAccountLimits where
  page rq rs
    | Page.stop (rs Lens.^. dalrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dalrsAccountLimits) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dalNextToken Lens..~ rs Lens.^. dalrsNextToken

instance Lude.AWSRequest DescribeAccountLimits where
  type Rs DescribeAccountLimits = DescribeAccountLimitsResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "DescribeAccountLimitsResult"
      ( \s h x ->
          DescribeAccountLimitsResponse'
            Lude.<$> (x Lude..@? "NextToken")
            Lude.<*> ( x Lude..@? "AccountLimits" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAccountLimits where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeAccountLimits where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAccountLimits where
  toQuery DescribeAccountLimits' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeAccountLimits" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken
      ]

-- | The output for the 'DescribeAccountLimits' action.
--
-- /See:/ 'mkDescribeAccountLimitsResponse' smart constructor.
data DescribeAccountLimitsResponse = DescribeAccountLimitsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    accountLimits ::
      Lude.Maybe [AccountLimit],
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

-- | Creates a value of 'DescribeAccountLimitsResponse' with the minimum fields required to make a request.
--
-- * 'accountLimits' - An account limit structure that contain a list of AWS CloudFormation account limits and their values.
-- * 'nextToken' - If the output exceeds 1 MB in size, a string that identifies the next page of limits. If no additional page exists, this value is null.
-- * 'responseStatus' - The response status code.
mkDescribeAccountLimitsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAccountLimitsResponse
mkDescribeAccountLimitsResponse pResponseStatus_ =
  DescribeAccountLimitsResponse'
    { nextToken = Lude.Nothing,
      accountLimits = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the output exceeds 1 MB in size, a string that identifies the next page of limits. If no additional page exists, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dalrsNextToken :: Lens.Lens' DescribeAccountLimitsResponse (Lude.Maybe Lude.Text)
dalrsNextToken = Lens.lens (nextToken :: DescribeAccountLimitsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAccountLimitsResponse)
{-# DEPRECATED dalrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An account limit structure that contain a list of AWS CloudFormation account limits and their values.
--
-- /Note:/ Consider using 'accountLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dalrsAccountLimits :: Lens.Lens' DescribeAccountLimitsResponse (Lude.Maybe [AccountLimit])
dalrsAccountLimits = Lens.lens (accountLimits :: DescribeAccountLimitsResponse -> Lude.Maybe [AccountLimit]) (\s a -> s {accountLimits = a} :: DescribeAccountLimitsResponse)
{-# DEPRECATED dalrsAccountLimits "Use generic-lens or generic-optics with 'accountLimits' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dalrsResponseStatus :: Lens.Lens' DescribeAccountLimitsResponse Lude.Int
dalrsResponseStatus = Lens.lens (responseStatus :: DescribeAccountLimitsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAccountLimitsResponse)
{-# DEPRECATED dalrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
