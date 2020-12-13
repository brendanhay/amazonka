{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.ListApplications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the applications registered with the IAM user or AWS account.
--
-- This operation returns paginated results.
module Network.AWS.CodeDeploy.ListApplications
  ( -- * Creating a request
    ListApplications (..),
    mkListApplications,

    -- ** Request lenses
    laNextToken,

    -- * Destructuring the response
    ListApplicationsResponse (..),
    mkListApplicationsResponse,

    -- ** Response lenses
    larsNextToken,
    larsApplications,
    larsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @ListApplications@ operation.
--
-- /See:/ 'mkListApplications' smart constructor.
newtype ListApplications = ListApplications'
  { -- | An identifier returned from the previous list applications call. It can be used to return the next set of applications in the list.
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListApplications' with the minimum fields required to make a request.
--
-- * 'nextToken' - An identifier returned from the previous list applications call. It can be used to return the next set of applications in the list.
mkListApplications ::
  ListApplications
mkListApplications = ListApplications' {nextToken = Lude.Nothing}

-- | An identifier returned from the previous list applications call. It can be used to return the next set of applications in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laNextToken :: Lens.Lens' ListApplications (Lude.Maybe Lude.Text)
laNextToken = Lens.lens (nextToken :: ListApplications -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListApplications)
{-# DEPRECATED laNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager ListApplications where
  page rq rs
    | Page.stop (rs Lens.^. larsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. larsApplications) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& laNextToken Lens..~ rs Lens.^. larsNextToken

instance Lude.AWSRequest ListApplications where
  type Rs ListApplications = ListApplicationsResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListApplicationsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "applications" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListApplications where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeDeploy_20141006.ListApplications" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListApplications where
  toJSON ListApplications' {..} =
    Lude.object
      (Lude.catMaybes [("nextToken" Lude..=) Lude.<$> nextToken])

instance Lude.ToPath ListApplications where
  toPath = Lude.const "/"

instance Lude.ToQuery ListApplications where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a ListApplications operation.
--
-- /See:/ 'mkListApplicationsResponse' smart constructor.
data ListApplicationsResponse = ListApplicationsResponse'
  { -- | If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list applications call to return the next set of applications in the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of application names.
    applications :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListApplicationsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list applications call to return the next set of applications in the list.
-- * 'applications' - A list of application names.
-- * 'responseStatus' - The response status code.
mkListApplicationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListApplicationsResponse
mkListApplicationsResponse pResponseStatus_ =
  ListApplicationsResponse'
    { nextToken = Lude.Nothing,
      applications = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list applications call to return the next set of applications in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsNextToken :: Lens.Lens' ListApplicationsResponse (Lude.Maybe Lude.Text)
larsNextToken = Lens.lens (nextToken :: ListApplicationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListApplicationsResponse)
{-# DEPRECATED larsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of application names.
--
-- /Note:/ Consider using 'applications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsApplications :: Lens.Lens' ListApplicationsResponse (Lude.Maybe [Lude.Text])
larsApplications = Lens.lens (applications :: ListApplicationsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {applications = a} :: ListApplicationsResponse)
{-# DEPRECATED larsApplications "Use generic-lens or generic-optics with 'applications' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsResponseStatus :: Lens.Lens' ListApplicationsResponse Lude.Int
larsResponseStatus = Lens.lens (responseStatus :: ListApplicationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListApplicationsResponse)
{-# DEPRECATED larsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
