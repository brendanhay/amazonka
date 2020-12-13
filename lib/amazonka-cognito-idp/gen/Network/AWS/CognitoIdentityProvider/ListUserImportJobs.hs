{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.ListUserImportJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the user import jobs.
module Network.AWS.CognitoIdentityProvider.ListUserImportJobs
  ( -- * Creating a request
    ListUserImportJobs (..),
    mkListUserImportJobs,

    -- ** Request lenses
    luijPaginationToken,
    luijUserPoolId,
    luijMaxResults,

    -- * Destructuring the response
    ListUserImportJobsResponse (..),
    mkListUserImportJobsResponse,

    -- ** Response lenses
    luijrsPaginationToken,
    luijrsUserImportJobs,
    luijrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to list the user import jobs.
--
-- /See:/ 'mkListUserImportJobs' smart constructor.
data ListUserImportJobs = ListUserImportJobs'
  { -- | An identifier that was returned from the previous call to @ListUserImportJobs@ , which can be used to return the next set of import jobs in the list.
    paginationToken :: Lude.Maybe Lude.Text,
    -- | The user pool ID for the user pool that the users are being imported into.
    userPoolId :: Lude.Text,
    -- | The maximum number of import jobs you want the request to return.
    maxResults :: Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListUserImportJobs' with the minimum fields required to make a request.
--
-- * 'paginationToken' - An identifier that was returned from the previous call to @ListUserImportJobs@ , which can be used to return the next set of import jobs in the list.
-- * 'userPoolId' - The user pool ID for the user pool that the users are being imported into.
-- * 'maxResults' - The maximum number of import jobs you want the request to return.
mkListUserImportJobs ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'maxResults'
  Lude.Natural ->
  ListUserImportJobs
mkListUserImportJobs pUserPoolId_ pMaxResults_ =
  ListUserImportJobs'
    { paginationToken = Lude.Nothing,
      userPoolId = pUserPoolId_,
      maxResults = pMaxResults_
    }

-- | An identifier that was returned from the previous call to @ListUserImportJobs@ , which can be used to return the next set of import jobs in the list.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luijPaginationToken :: Lens.Lens' ListUserImportJobs (Lude.Maybe Lude.Text)
luijPaginationToken = Lens.lens (paginationToken :: ListUserImportJobs -> Lude.Maybe Lude.Text) (\s a -> s {paginationToken = a} :: ListUserImportJobs)
{-# DEPRECATED luijPaginationToken "Use generic-lens or generic-optics with 'paginationToken' instead." #-}

-- | The user pool ID for the user pool that the users are being imported into.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luijUserPoolId :: Lens.Lens' ListUserImportJobs Lude.Text
luijUserPoolId = Lens.lens (userPoolId :: ListUserImportJobs -> Lude.Text) (\s a -> s {userPoolId = a} :: ListUserImportJobs)
{-# DEPRECATED luijUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The maximum number of import jobs you want the request to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luijMaxResults :: Lens.Lens' ListUserImportJobs Lude.Natural
luijMaxResults = Lens.lens (maxResults :: ListUserImportJobs -> Lude.Natural) (\s a -> s {maxResults = a} :: ListUserImportJobs)
{-# DEPRECATED luijMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListUserImportJobs where
  type Rs ListUserImportJobs = ListUserImportJobsResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListUserImportJobsResponse'
            Lude.<$> (x Lude..?> "PaginationToken")
            Lude.<*> (x Lude..?> "UserImportJobs")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListUserImportJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.ListUserImportJobs" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListUserImportJobs where
  toJSON ListUserImportJobs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PaginationToken" Lude..=) Lude.<$> paginationToken,
            Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("MaxResults" Lude..= maxResults)
          ]
      )

instance Lude.ToPath ListUserImportJobs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListUserImportJobs where
  toQuery = Lude.const Lude.mempty

-- | Represents the response from the server to the request to list the user import jobs.
--
-- /See:/ 'mkListUserImportJobsResponse' smart constructor.
data ListUserImportJobsResponse = ListUserImportJobsResponse'
  { -- | An identifier that can be used to return the next set of user import jobs in the list.
    paginationToken :: Lude.Maybe Lude.Text,
    -- | The user import jobs.
    userImportJobs :: Lude.Maybe (Lude.NonEmpty UserImportJobType),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListUserImportJobsResponse' with the minimum fields required to make a request.
--
-- * 'paginationToken' - An identifier that can be used to return the next set of user import jobs in the list.
-- * 'userImportJobs' - The user import jobs.
-- * 'responseStatus' - The response status code.
mkListUserImportJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListUserImportJobsResponse
mkListUserImportJobsResponse pResponseStatus_ =
  ListUserImportJobsResponse'
    { paginationToken = Lude.Nothing,
      userImportJobs = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An identifier that can be used to return the next set of user import jobs in the list.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luijrsPaginationToken :: Lens.Lens' ListUserImportJobsResponse (Lude.Maybe Lude.Text)
luijrsPaginationToken = Lens.lens (paginationToken :: ListUserImportJobsResponse -> Lude.Maybe Lude.Text) (\s a -> s {paginationToken = a} :: ListUserImportJobsResponse)
{-# DEPRECATED luijrsPaginationToken "Use generic-lens or generic-optics with 'paginationToken' instead." #-}

-- | The user import jobs.
--
-- /Note:/ Consider using 'userImportJobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luijrsUserImportJobs :: Lens.Lens' ListUserImportJobsResponse (Lude.Maybe (Lude.NonEmpty UserImportJobType))
luijrsUserImportJobs = Lens.lens (userImportJobs :: ListUserImportJobsResponse -> Lude.Maybe (Lude.NonEmpty UserImportJobType)) (\s a -> s {userImportJobs = a} :: ListUserImportJobsResponse)
{-# DEPRECATED luijrsUserImportJobs "Use generic-lens or generic-optics with 'userImportJobs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luijrsResponseStatus :: Lens.Lens' ListUserImportJobsResponse Lude.Int
luijrsResponseStatus = Lens.lens (responseStatus :: ListUserImportJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListUserImportJobsResponse)
{-# DEPRECATED luijrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
