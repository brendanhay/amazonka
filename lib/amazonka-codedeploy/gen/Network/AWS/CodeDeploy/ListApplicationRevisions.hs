{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.ListApplicationRevisions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about revisions for an application.
--
-- This operation returns paginated results.
module Network.AWS.CodeDeploy.ListApplicationRevisions
  ( -- * Creating a request
    ListApplicationRevisions (..),
    mkListApplicationRevisions,

    -- ** Request lenses
    larS3KeyPrefix,
    larDeployed,
    larSortOrder,
    larNextToken,
    larS3Bucket,
    larSortBy,
    larApplicationName,

    -- * Destructuring the response
    ListApplicationRevisionsResponse (..),
    mkListApplicationRevisionsResponse,

    -- ** Response lenses
    larrsNextToken,
    larrsRevisions,
    larrsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @ListApplicationRevisions@ operation.
--
-- /See:/ 'mkListApplicationRevisions' smart constructor.
data ListApplicationRevisions = ListApplicationRevisions'
  { s3KeyPrefix ::
      Lude.Maybe Lude.Text,
    deployed ::
      Lude.Maybe ListStateFilterAction,
    sortOrder :: Lude.Maybe SortOrder,
    nextToken :: Lude.Maybe Lude.Text,
    s3Bucket :: Lude.Maybe Lude.Text,
    sortBy ::
      Lude.Maybe ApplicationRevisionSortBy,
    applicationName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListApplicationRevisions' with the minimum fields required to make a request.
--
-- * 'applicationName' - The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
-- * 'deployed' - Whether to list revisions based on whether the revision is the target revision of a deployment group:
--
--
--     * @include@ : List revisions that are target revisions of a deployment group.
--
--
--     * @exclude@ : Do not list revisions that are target revisions of a deployment group.
--
--
--     * @ignore@ : List all revisions.
--
--
-- * 'nextToken' - An identifier returned from the previous @ListApplicationRevisions@ call. It can be used to return the next set of applications in the list.
-- * 's3Bucket' - An Amazon S3 bucket name to limit the search for revisions.
--
-- If set to null, all of the user's buckets are searched.
-- * 's3KeyPrefix' - A key prefix for the set of Amazon S3 objects to limit the search for revisions.
-- * 'sortBy' - The column name to use to sort the list results:
--
--
--     * @registerTime@ : Sort by the time the revisions were registered with AWS CodeDeploy.
--
--
--     * @firstUsedTime@ : Sort by the time the revisions were first used in a deployment.
--
--
--     * @lastUsedTime@ : Sort by the time the revisions were last used in a deployment.
--
--
-- If not specified or set to null, the results are returned in an arbitrary order.
-- * 'sortOrder' - The order in which to sort the list results:
--
--
--     * @ascending@ : ascending order.
--
--
--     * @descending@ : descending order.
--
--
-- If not specified, the results are sorted in ascending order.
-- If set to null, the results are sorted in an arbitrary order.
mkListApplicationRevisions ::
  -- | 'applicationName'
  Lude.Text ->
  ListApplicationRevisions
mkListApplicationRevisions pApplicationName_ =
  ListApplicationRevisions'
    { s3KeyPrefix = Lude.Nothing,
      deployed = Lude.Nothing,
      sortOrder = Lude.Nothing,
      nextToken = Lude.Nothing,
      s3Bucket = Lude.Nothing,
      sortBy = Lude.Nothing,
      applicationName = pApplicationName_
    }

-- | A key prefix for the set of Amazon S3 objects to limit the search for revisions.
--
-- /Note:/ Consider using 's3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larS3KeyPrefix :: Lens.Lens' ListApplicationRevisions (Lude.Maybe Lude.Text)
larS3KeyPrefix = Lens.lens (s3KeyPrefix :: ListApplicationRevisions -> Lude.Maybe Lude.Text) (\s a -> s {s3KeyPrefix = a} :: ListApplicationRevisions)
{-# DEPRECATED larS3KeyPrefix "Use generic-lens or generic-optics with 's3KeyPrefix' instead." #-}

-- | Whether to list revisions based on whether the revision is the target revision of a deployment group:
--
--
--     * @include@ : List revisions that are target revisions of a deployment group.
--
--
--     * @exclude@ : Do not list revisions that are target revisions of a deployment group.
--
--
--     * @ignore@ : List all revisions.
--
--
--
-- /Note:/ Consider using 'deployed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larDeployed :: Lens.Lens' ListApplicationRevisions (Lude.Maybe ListStateFilterAction)
larDeployed = Lens.lens (deployed :: ListApplicationRevisions -> Lude.Maybe ListStateFilterAction) (\s a -> s {deployed = a} :: ListApplicationRevisions)
{-# DEPRECATED larDeployed "Use generic-lens or generic-optics with 'deployed' instead." #-}

-- | The order in which to sort the list results:
--
--
--     * @ascending@ : ascending order.
--
--
--     * @descending@ : descending order.
--
--
-- If not specified, the results are sorted in ascending order.
-- If set to null, the results are sorted in an arbitrary order.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larSortOrder :: Lens.Lens' ListApplicationRevisions (Lude.Maybe SortOrder)
larSortOrder = Lens.lens (sortOrder :: ListApplicationRevisions -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: ListApplicationRevisions)
{-# DEPRECATED larSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | An identifier returned from the previous @ListApplicationRevisions@ call. It can be used to return the next set of applications in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larNextToken :: Lens.Lens' ListApplicationRevisions (Lude.Maybe Lude.Text)
larNextToken = Lens.lens (nextToken :: ListApplicationRevisions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListApplicationRevisions)
{-# DEPRECATED larNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An Amazon S3 bucket name to limit the search for revisions.
--
-- If set to null, all of the user's buckets are searched.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larS3Bucket :: Lens.Lens' ListApplicationRevisions (Lude.Maybe Lude.Text)
larS3Bucket = Lens.lens (s3Bucket :: ListApplicationRevisions -> Lude.Maybe Lude.Text) (\s a -> s {s3Bucket = a} :: ListApplicationRevisions)
{-# DEPRECATED larS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

-- | The column name to use to sort the list results:
--
--
--     * @registerTime@ : Sort by the time the revisions were registered with AWS CodeDeploy.
--
--
--     * @firstUsedTime@ : Sort by the time the revisions were first used in a deployment.
--
--
--     * @lastUsedTime@ : Sort by the time the revisions were last used in a deployment.
--
--
-- If not specified or set to null, the results are returned in an arbitrary order.
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larSortBy :: Lens.Lens' ListApplicationRevisions (Lude.Maybe ApplicationRevisionSortBy)
larSortBy = Lens.lens (sortBy :: ListApplicationRevisions -> Lude.Maybe ApplicationRevisionSortBy) (\s a -> s {sortBy = a} :: ListApplicationRevisions)
{-# DEPRECATED larSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larApplicationName :: Lens.Lens' ListApplicationRevisions Lude.Text
larApplicationName = Lens.lens (applicationName :: ListApplicationRevisions -> Lude.Text) (\s a -> s {applicationName = a} :: ListApplicationRevisions)
{-# DEPRECATED larApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

instance Page.AWSPager ListApplicationRevisions where
  page rq rs
    | Page.stop (rs Lens.^. larrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. larrsRevisions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& larNextToken Lens..~ rs Lens.^. larrsNextToken

instance Lude.AWSRequest ListApplicationRevisions where
  type Rs ListApplicationRevisions = ListApplicationRevisionsResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListApplicationRevisionsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "revisions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListApplicationRevisions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeDeploy_20141006.ListApplicationRevisions" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListApplicationRevisions where
  toJSON ListApplicationRevisions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("s3KeyPrefix" Lude..=) Lude.<$> s3KeyPrefix,
            ("deployed" Lude..=) Lude.<$> deployed,
            ("sortOrder" Lude..=) Lude.<$> sortOrder,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("s3Bucket" Lude..=) Lude.<$> s3Bucket,
            ("sortBy" Lude..=) Lude.<$> sortBy,
            Lude.Just ("applicationName" Lude..= applicationName)
          ]
      )

instance Lude.ToPath ListApplicationRevisions where
  toPath = Lude.const "/"

instance Lude.ToQuery ListApplicationRevisions where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @ListApplicationRevisions@ operation.
--
-- /See:/ 'mkListApplicationRevisionsResponse' smart constructor.
data ListApplicationRevisionsResponse = ListApplicationRevisionsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    revisions ::
      Lude.Maybe
        [RevisionLocation],
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

-- | Creates a value of 'ListApplicationRevisionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list application revisions call to return the next set of application revisions in the list.
-- * 'responseStatus' - The response status code.
-- * 'revisions' - A list of locations that contain the matching revisions.
mkListApplicationRevisionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListApplicationRevisionsResponse
mkListApplicationRevisionsResponse pResponseStatus_ =
  ListApplicationRevisionsResponse'
    { nextToken = Lude.Nothing,
      revisions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list application revisions call to return the next set of application revisions in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsNextToken :: Lens.Lens' ListApplicationRevisionsResponse (Lude.Maybe Lude.Text)
larrsNextToken = Lens.lens (nextToken :: ListApplicationRevisionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListApplicationRevisionsResponse)
{-# DEPRECATED larrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of locations that contain the matching revisions.
--
-- /Note:/ Consider using 'revisions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsRevisions :: Lens.Lens' ListApplicationRevisionsResponse (Lude.Maybe [RevisionLocation])
larrsRevisions = Lens.lens (revisions :: ListApplicationRevisionsResponse -> Lude.Maybe [RevisionLocation]) (\s a -> s {revisions = a} :: ListApplicationRevisionsResponse)
{-# DEPRECATED larrsRevisions "Use generic-lens or generic-optics with 'revisions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsResponseStatus :: Lens.Lens' ListApplicationRevisionsResponse Lude.Int
larrsResponseStatus = Lens.lens (responseStatus :: ListApplicationRevisionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListApplicationRevisionsResponse)
{-# DEPRECATED larrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
