{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.ListExports
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all exported output values in the account and Region in which you call this action. Use this action to see the exported output values that you can import into other stacks. To import values, use the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/intrinsic-function-reference-importvalue.html @Fn::ImportValue@ > function.
--
-- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-exports.html AWS CloudFormation Export Stack Output Values> .
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.ListExports
  ( -- * Creating a request
    ListExports (..),
    mkListExports,

    -- ** Request lenses
    leNextToken,

    -- * Destructuring the response
    ListExportsResponse (..),
    mkListExportsResponse,

    -- ** Response lenses
    lersNextToken,
    lersExports,
    lersResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListExports' smart constructor.
newtype ListExports = ListExports'
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

-- | Creates a value of 'ListExports' with the minimum fields required to make a request.
--
-- * 'nextToken' - A string (provided by the 'ListExports' response output) that identifies the next page of exported output values that you asked to retrieve.
mkListExports ::
  ListExports
mkListExports = ListExports' {nextToken = Lude.Nothing}

-- | A string (provided by the 'ListExports' response output) that identifies the next page of exported output values that you asked to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leNextToken :: Lens.Lens' ListExports (Lude.Maybe Lude.Text)
leNextToken = Lens.lens (nextToken :: ListExports -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListExports)
{-# DEPRECATED leNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager ListExports where
  page rq rs
    | Page.stop (rs Lens.^. lersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lersExports) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& leNextToken Lens..~ rs Lens.^. lersNextToken

instance Lude.AWSRequest ListExports where
  type Rs ListExports = ListExportsResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "ListExportsResult"
      ( \s h x ->
          ListExportsResponse'
            Lude.<$> (x Lude..@? "NextToken")
            Lude.<*> ( x Lude..@? "Exports" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListExports where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListExports where
  toPath = Lude.const "/"

instance Lude.ToQuery ListExports where
  toQuery ListExports' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListExports" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken
      ]

-- | /See:/ 'mkListExportsResponse' smart constructor.
data ListExportsResponse = ListExportsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    exports :: Lude.Maybe [Export],
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

-- | Creates a value of 'ListExportsResponse' with the minimum fields required to make a request.
--
-- * 'exports' - The output for the 'ListExports' action.
-- * 'nextToken' - If the output exceeds 100 exported output values, a string that identifies the next page of exports. If there is no additional page, this value is null.
-- * 'responseStatus' - The response status code.
mkListExportsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListExportsResponse
mkListExportsResponse pResponseStatus_ =
  ListExportsResponse'
    { nextToken = Lude.Nothing,
      exports = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the output exceeds 100 exported output values, a string that identifies the next page of exports. If there is no additional page, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lersNextToken :: Lens.Lens' ListExportsResponse (Lude.Maybe Lude.Text)
lersNextToken = Lens.lens (nextToken :: ListExportsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListExportsResponse)
{-# DEPRECATED lersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The output for the 'ListExports' action.
--
-- /Note:/ Consider using 'exports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lersExports :: Lens.Lens' ListExportsResponse (Lude.Maybe [Export])
lersExports = Lens.lens (exports :: ListExportsResponse -> Lude.Maybe [Export]) (\s a -> s {exports = a} :: ListExportsResponse)
{-# DEPRECATED lersExports "Use generic-lens or generic-optics with 'exports' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lersResponseStatus :: Lens.Lens' ListExportsResponse Lude.Int
lersResponseStatus = Lens.lens (responseStatus :: ListExportsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListExportsResponse)
{-# DEPRECATED lersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
