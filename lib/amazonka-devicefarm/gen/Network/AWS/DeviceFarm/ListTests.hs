{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListTests
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about tests in a given test suite.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListTests
  ( -- * Creating a request
    ListTests (..),
    mkListTests,

    -- ** Request lenses
    ltNextToken,
    ltArn,

    -- * Destructuring the response
    ListTestsResponse (..),
    mkListTestsResponse,

    -- ** Response lenses
    ltrsTests,
    ltrsNextToken,
    ltrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a request to the list tests operation.
--
-- /See:/ 'mkListTests' smart constructor.
data ListTests = ListTests'
  { nextToken :: Lude.Maybe Lude.Text,
    arn :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTests' with the minimum fields required to make a request.
--
-- * 'arn' - The test suite's Amazon Resource Name (ARN).
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
mkListTests ::
  -- | 'arn'
  Lude.Text ->
  ListTests
mkListTests pArn_ =
  ListTests' {nextToken = Lude.Nothing, arn = pArn_}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltNextToken :: Lens.Lens' ListTests (Lude.Maybe Lude.Text)
ltNextToken = Lens.lens (nextToken :: ListTests -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTests)
{-# DEPRECATED ltNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The test suite's Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltArn :: Lens.Lens' ListTests Lude.Text
ltArn = Lens.lens (arn :: ListTests -> Lude.Text) (\s a -> s {arn = a} :: ListTests)
{-# DEPRECATED ltArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Page.AWSPager ListTests where
  page rq rs
    | Page.stop (rs Lens.^. ltrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltrsTests) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltNextToken Lens..~ rs Lens.^. ltrsNextToken

instance Lude.AWSRequest ListTests where
  type Rs ListTests = ListTestsResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTestsResponse'
            Lude.<$> (x Lude..?> "tests" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTests where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.ListTests" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTests where
  toJSON ListTests' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("arn" Lude..= arn)
          ]
      )

instance Lude.ToPath ListTests where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTests where
  toQuery = Lude.const Lude.mempty

-- | Represents the result of a list tests request.
--
-- /See:/ 'mkListTestsResponse' smart constructor.
data ListTestsResponse = ListTestsResponse'
  { tests ::
      Lude.Maybe [Test],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListTestsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
-- * 'responseStatus' - The response status code.
-- * 'tests' - Information about the tests.
mkListTestsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTestsResponse
mkListTestsResponse pResponseStatus_ =
  ListTestsResponse'
    { tests = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the tests.
--
-- /Note:/ Consider using 'tests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsTests :: Lens.Lens' ListTestsResponse (Lude.Maybe [Test])
ltrsTests = Lens.lens (tests :: ListTestsResponse -> Lude.Maybe [Test]) (\s a -> s {tests = a} :: ListTestsResponse)
{-# DEPRECATED ltrsTests "Use generic-lens or generic-optics with 'tests' instead." #-}

-- | If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsNextToken :: Lens.Lens' ListTestsResponse (Lude.Maybe Lude.Text)
ltrsNextToken = Lens.lens (nextToken :: ListTestsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTestsResponse)
{-# DEPRECATED ltrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsResponseStatus :: Lens.Lens' ListTestsResponse Lude.Int
ltrsResponseStatus = Lens.lens (responseStatus :: ListTestsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTestsResponse)
{-# DEPRECATED ltrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
