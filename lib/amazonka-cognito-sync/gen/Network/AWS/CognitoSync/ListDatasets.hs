{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.ListDatasets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists datasets for an identity. With Amazon Cognito Sync, each identity has access only to its own data. Thus, the credentials used to make this API call need to have access to the identity data.
--
-- ListDatasets can be called with temporary user credentials provided by Cognito Identity or with developer credentials. You should use the Cognito Identity credentials to make this API call.
module Network.AWS.CognitoSync.ListDatasets
  ( -- * Creating a request
    ListDatasets (..),
    mkListDatasets,

    -- ** Request lenses
    ldIdentityPoolId,
    ldNextToken,
    ldIdentityId,
    ldMaxResults,

    -- * Destructuring the response
    ListDatasetsResponse (..),
    mkListDatasetsResponse,

    -- ** Response lenses
    ldrsCount,
    ldrsNextToken,
    ldrsDatasets,
    ldrsResponseStatus,
  )
where

import Network.AWS.CognitoSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request for a list of datasets for an identity.
--
-- /See:/ 'mkListDatasets' smart constructor.
data ListDatasets = ListDatasets'
  { -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
    identityPoolId :: Lude.Text,
    -- | A pagination token for obtaining the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
    identityId :: Lude.Text,
    -- | The maximum number of results to be returned.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDatasets' with the minimum fields required to make a request.
--
-- * 'identityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
-- * 'nextToken' - A pagination token for obtaining the next page of results.
-- * 'identityId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
-- * 'maxResults' - The maximum number of results to be returned.
mkListDatasets ::
  -- | 'identityPoolId'
  Lude.Text ->
  -- | 'identityId'
  Lude.Text ->
  ListDatasets
mkListDatasets pIdentityPoolId_ pIdentityId_ =
  ListDatasets'
    { identityPoolId = pIdentityPoolId_,
      nextToken = Lude.Nothing,
      identityId = pIdentityId_,
      maxResults = Lude.Nothing
    }

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldIdentityPoolId :: Lens.Lens' ListDatasets Lude.Text
ldIdentityPoolId = Lens.lens (identityPoolId :: ListDatasets -> Lude.Text) (\s a -> s {identityPoolId = a} :: ListDatasets)
{-# DEPRECATED ldIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | A pagination token for obtaining the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldNextToken :: Lens.Lens' ListDatasets (Lude.Maybe Lude.Text)
ldNextToken = Lens.lens (nextToken :: ListDatasets -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDatasets)
{-# DEPRECATED ldNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldIdentityId :: Lens.Lens' ListDatasets Lude.Text
ldIdentityId = Lens.lens (identityId :: ListDatasets -> Lude.Text) (\s a -> s {identityId = a} :: ListDatasets)
{-# DEPRECATED ldIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | The maximum number of results to be returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldMaxResults :: Lens.Lens' ListDatasets (Lude.Maybe Lude.Int)
ldMaxResults = Lens.lens (maxResults :: ListDatasets -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListDatasets)
{-# DEPRECATED ldMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListDatasets where
  type Rs ListDatasets = ListDatasetsResponse
  request = Req.get cognitoSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDatasetsResponse'
            Lude.<$> (x Lude..?> "Count")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Datasets" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDatasets where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListDatasets where
  toPath ListDatasets' {..} =
    Lude.mconcat
      [ "/identitypools/",
        Lude.toBS identityPoolId,
        "/identities/",
        Lude.toBS identityId,
        "/datasets"
      ]

instance Lude.ToQuery ListDatasets where
  toQuery ListDatasets' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | Returned for a successful ListDatasets request.
--
-- /See:/ 'mkListDatasetsResponse' smart constructor.
data ListDatasetsResponse = ListDatasetsResponse'
  { -- | Number of datasets returned.
    count :: Lude.Maybe Lude.Int,
    -- | A pagination token for obtaining the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A set of datasets.
    datasets :: Lude.Maybe [Dataset],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDatasetsResponse' with the minimum fields required to make a request.
--
-- * 'count' - Number of datasets returned.
-- * 'nextToken' - A pagination token for obtaining the next page of results.
-- * 'datasets' - A set of datasets.
-- * 'responseStatus' - The response status code.
mkListDatasetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDatasetsResponse
mkListDatasetsResponse pResponseStatus_ =
  ListDatasetsResponse'
    { count = Lude.Nothing,
      nextToken = Lude.Nothing,
      datasets = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Number of datasets returned.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsCount :: Lens.Lens' ListDatasetsResponse (Lude.Maybe Lude.Int)
ldrsCount = Lens.lens (count :: ListDatasetsResponse -> Lude.Maybe Lude.Int) (\s a -> s {count = a} :: ListDatasetsResponse)
{-# DEPRECATED ldrsCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | A pagination token for obtaining the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsNextToken :: Lens.Lens' ListDatasetsResponse (Lude.Maybe Lude.Text)
ldrsNextToken = Lens.lens (nextToken :: ListDatasetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDatasetsResponse)
{-# DEPRECATED ldrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A set of datasets.
--
-- /Note:/ Consider using 'datasets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsDatasets :: Lens.Lens' ListDatasetsResponse (Lude.Maybe [Dataset])
ldrsDatasets = Lens.lens (datasets :: ListDatasetsResponse -> Lude.Maybe [Dataset]) (\s a -> s {datasets = a} :: ListDatasetsResponse)
{-# DEPRECATED ldrsDatasets "Use generic-lens or generic-optics with 'datasets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsResponseStatus :: Lens.Lens' ListDatasetsResponse Lude.Int
ldrsResponseStatus = Lens.lens (responseStatus :: ListDatasetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDatasetsResponse)
{-# DEPRECATED ldrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
