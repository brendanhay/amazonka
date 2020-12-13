{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DescribeProjectVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists and describes the models in an Amazon Rekognition Custom Labels project. You can specify up to 10 model versions in @ProjectVersionArns@ . If you don't specify a value, descriptions for all models are returned.
--
-- This operation requires permissions to perform the @rekognition:DescribeProjectVersions@ action.
--
-- This operation returns paginated results.
module Network.AWS.Rekognition.DescribeProjectVersions
  ( -- * Creating a request
    DescribeProjectVersions (..),
    mkDescribeProjectVersions,

    -- ** Request lenses
    dpvNextToken,
    dpvProjectARN,
    dpvVersionNames,
    dpvMaxResults,

    -- * Destructuring the response
    DescribeProjectVersionsResponse (..),
    mkDescribeProjectVersionsResponse,

    -- ** Response lenses
    dpvrsNextToken,
    dpvrsProjectVersionDescriptions,
    dpvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeProjectVersions' smart constructor.
data DescribeProjectVersions = DescribeProjectVersions'
  { -- | If the previous response was incomplete (because there is more results to retrieve), Amazon Rekognition Custom Labels returns a pagination token in the response. You can use this pagination token to retrieve the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the project that contains the models you want to describe.
    projectARN :: Lude.Text,
    -- | A list of model version names that you want to describe. You can add up to 10 model version names to the list. If you don't specify a value, all model descriptions are returned. A version name is part of a model (ProjectVersion) ARN. For example, @my-model.2020-01-21T09.10.15@ is the version name in the following ARN. @arn:aws:rekognition:us-east-1:123456789012:project/getting-started/version//my-model.2020-01-21T09.10.15/ /1234567890123@ .
    versionNames :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | The maximum number of results to return per paginated call. The largest value you can specify is 100. If you specify a value greater than 100, a ValidationException error occurs. The default value is 100.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeProjectVersions' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the previous response was incomplete (because there is more results to retrieve), Amazon Rekognition Custom Labels returns a pagination token in the response. You can use this pagination token to retrieve the next set of results.
-- * 'projectARN' - The Amazon Resource Name (ARN) of the project that contains the models you want to describe.
-- * 'versionNames' - A list of model version names that you want to describe. You can add up to 10 model version names to the list. If you don't specify a value, all model descriptions are returned. A version name is part of a model (ProjectVersion) ARN. For example, @my-model.2020-01-21T09.10.15@ is the version name in the following ARN. @arn:aws:rekognition:us-east-1:123456789012:project/getting-started/version//my-model.2020-01-21T09.10.15/ /1234567890123@ .
-- * 'maxResults' - The maximum number of results to return per paginated call. The largest value you can specify is 100. If you specify a value greater than 100, a ValidationException error occurs. The default value is 100.
mkDescribeProjectVersions ::
  -- | 'projectARN'
  Lude.Text ->
  DescribeProjectVersions
mkDescribeProjectVersions pProjectARN_ =
  DescribeProjectVersions'
    { nextToken = Lude.Nothing,
      projectARN = pProjectARN_,
      versionNames = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | If the previous response was incomplete (because there is more results to retrieve), Amazon Rekognition Custom Labels returns a pagination token in the response. You can use this pagination token to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvNextToken :: Lens.Lens' DescribeProjectVersions (Lude.Maybe Lude.Text)
dpvNextToken = Lens.lens (nextToken :: DescribeProjectVersions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeProjectVersions)
{-# DEPRECATED dpvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The Amazon Resource Name (ARN) of the project that contains the models you want to describe.
--
-- /Note:/ Consider using 'projectARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvProjectARN :: Lens.Lens' DescribeProjectVersions Lude.Text
dpvProjectARN = Lens.lens (projectARN :: DescribeProjectVersions -> Lude.Text) (\s a -> s {projectARN = a} :: DescribeProjectVersions)
{-# DEPRECATED dpvProjectARN "Use generic-lens or generic-optics with 'projectARN' instead." #-}

-- | A list of model version names that you want to describe. You can add up to 10 model version names to the list. If you don't specify a value, all model descriptions are returned. A version name is part of a model (ProjectVersion) ARN. For example, @my-model.2020-01-21T09.10.15@ is the version name in the following ARN. @arn:aws:rekognition:us-east-1:123456789012:project/getting-started/version//my-model.2020-01-21T09.10.15/ /1234567890123@ .
--
-- /Note:/ Consider using 'versionNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvVersionNames :: Lens.Lens' DescribeProjectVersions (Lude.Maybe (Lude.NonEmpty Lude.Text))
dpvVersionNames = Lens.lens (versionNames :: DescribeProjectVersions -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {versionNames = a} :: DescribeProjectVersions)
{-# DEPRECATED dpvVersionNames "Use generic-lens or generic-optics with 'versionNames' instead." #-}

-- | The maximum number of results to return per paginated call. The largest value you can specify is 100. If you specify a value greater than 100, a ValidationException error occurs. The default value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvMaxResults :: Lens.Lens' DescribeProjectVersions (Lude.Maybe Lude.Natural)
dpvMaxResults = Lens.lens (maxResults :: DescribeProjectVersions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeProjectVersions)
{-# DEPRECATED dpvMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeProjectVersions where
  page rq rs
    | Page.stop (rs Lens.^. dpvrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dpvrsProjectVersionDescriptions) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dpvNextToken Lens..~ rs Lens.^. dpvrsNextToken

instance Lude.AWSRequest DescribeProjectVersions where
  type Rs DescribeProjectVersions = DescribeProjectVersionsResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeProjectVersionsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "ProjectVersionDescriptions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeProjectVersions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.DescribeProjectVersions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeProjectVersions where
  toJSON DescribeProjectVersions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("ProjectArn" Lude..= projectARN),
            ("VersionNames" Lude..=) Lude.<$> versionNames,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeProjectVersions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeProjectVersions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeProjectVersionsResponse' smart constructor.
data DescribeProjectVersionsResponse = DescribeProjectVersionsResponse'
  { -- | If the previous response was incomplete (because there is more results to retrieve), Amazon Rekognition Custom Labels returns a pagination token in the response. You can use this pagination token to retrieve the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of model descriptions. The list is sorted by the creation date and time of the model versions, latest to earliest.
    projectVersionDescriptions :: Lude.Maybe [ProjectVersionDescription],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeProjectVersionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the previous response was incomplete (because there is more results to retrieve), Amazon Rekognition Custom Labels returns a pagination token in the response. You can use this pagination token to retrieve the next set of results.
-- * 'projectVersionDescriptions' - A list of model descriptions. The list is sorted by the creation date and time of the model versions, latest to earliest.
-- * 'responseStatus' - The response status code.
mkDescribeProjectVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeProjectVersionsResponse
mkDescribeProjectVersionsResponse pResponseStatus_ =
  DescribeProjectVersionsResponse'
    { nextToken = Lude.Nothing,
      projectVersionDescriptions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the previous response was incomplete (because there is more results to retrieve), Amazon Rekognition Custom Labels returns a pagination token in the response. You can use this pagination token to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvrsNextToken :: Lens.Lens' DescribeProjectVersionsResponse (Lude.Maybe Lude.Text)
dpvrsNextToken = Lens.lens (nextToken :: DescribeProjectVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeProjectVersionsResponse)
{-# DEPRECATED dpvrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of model descriptions. The list is sorted by the creation date and time of the model versions, latest to earliest.
--
-- /Note:/ Consider using 'projectVersionDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvrsProjectVersionDescriptions :: Lens.Lens' DescribeProjectVersionsResponse (Lude.Maybe [ProjectVersionDescription])
dpvrsProjectVersionDescriptions = Lens.lens (projectVersionDescriptions :: DescribeProjectVersionsResponse -> Lude.Maybe [ProjectVersionDescription]) (\s a -> s {projectVersionDescriptions = a} :: DescribeProjectVersionsResponse)
{-# DEPRECATED dpvrsProjectVersionDescriptions "Use generic-lens or generic-optics with 'projectVersionDescriptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvrsResponseStatus :: Lens.Lens' DescribeProjectVersionsResponse Lude.Int
dpvrsResponseStatus = Lens.lens (responseStatus :: DescribeProjectVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeProjectVersionsResponse)
{-# DEPRECATED dpvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
