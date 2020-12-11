{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.ListWorkflowTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about workflow types in the specified domain. The results may be split into multiple pages that can be retrieved by making the call repeatedly.
--
-- __Access Control__
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--
--     * You cannot use an IAM policy to constrain this action's parameters.
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.SWF.ListWorkflowTypes
  ( -- * Creating a request
    ListWorkflowTypes (..),
    mkListWorkflowTypes,

    -- ** Request lenses
    lwtNextPageToken,
    lwtReverseOrder,
    lwtName,
    lwtMaximumPageSize,
    lwtDomain,
    lwtRegistrationStatus,

    -- * Destructuring the response
    ListWorkflowTypesResponse (..),
    mkListWorkflowTypesResponse,

    -- ** Response lenses
    lwtrsNextPageToken,
    lwtrsResponseStatus,
    lwtrsTypeInfos,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SWF.Types

-- | /See:/ 'mkListWorkflowTypes' smart constructor.
data ListWorkflowTypes = ListWorkflowTypes'
  { nextPageToken ::
      Lude.Maybe Lude.Text,
    reverseOrder :: Lude.Maybe Lude.Bool,
    name :: Lude.Maybe Lude.Text,
    maximumPageSize :: Lude.Maybe Lude.Natural,
    domain :: Lude.Text,
    registrationStatus :: RegistrationStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListWorkflowTypes' with the minimum fields required to make a request.
--
-- * 'domain' - The name of the domain in which the workflow types have been registered.
-- * 'maximumPageSize' - The maximum number of results that are returned per call. Use @nextPageToken@ to obtain further pages of results.
-- * 'name' - If specified, lists the workflow type with this name.
-- * 'nextPageToken' - If @NextPageToken@ is returned there are more results available. The value of @NextPageToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 60 seconds. Using an expired pagination token will return a @400@ error: "@Specified token has exceeded its maximum lifetime@ ".
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
-- * 'registrationStatus' - Specifies the registration status of the workflow types to list.
-- * 'reverseOrder' - When set to @true@ , returns the results in reverse order. By default the results are returned in ascending alphabetical order of the @name@ of the workflow types.
mkListWorkflowTypes ::
  -- | 'domain'
  Lude.Text ->
  -- | 'registrationStatus'
  RegistrationStatus ->
  ListWorkflowTypes
mkListWorkflowTypes pDomain_ pRegistrationStatus_ =
  ListWorkflowTypes'
    { nextPageToken = Lude.Nothing,
      reverseOrder = Lude.Nothing,
      name = Lude.Nothing,
      maximumPageSize = Lude.Nothing,
      domain = pDomain_,
      registrationStatus = pRegistrationStatus_
    }

-- | If @NextPageToken@ is returned there are more results available. The value of @NextPageToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 60 seconds. Using an expired pagination token will return a @400@ error: "@Specified token has exceeded its maximum lifetime@ ".
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwtNextPageToken :: Lens.Lens' ListWorkflowTypes (Lude.Maybe Lude.Text)
lwtNextPageToken = Lens.lens (nextPageToken :: ListWorkflowTypes -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: ListWorkflowTypes)
{-# DEPRECATED lwtNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | When set to @true@ , returns the results in reverse order. By default the results are returned in ascending alphabetical order of the @name@ of the workflow types.
--
-- /Note:/ Consider using 'reverseOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwtReverseOrder :: Lens.Lens' ListWorkflowTypes (Lude.Maybe Lude.Bool)
lwtReverseOrder = Lens.lens (reverseOrder :: ListWorkflowTypes -> Lude.Maybe Lude.Bool) (\s a -> s {reverseOrder = a} :: ListWorkflowTypes)
{-# DEPRECATED lwtReverseOrder "Use generic-lens or generic-optics with 'reverseOrder' instead." #-}

-- | If specified, lists the workflow type with this name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwtName :: Lens.Lens' ListWorkflowTypes (Lude.Maybe Lude.Text)
lwtName = Lens.lens (name :: ListWorkflowTypes -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ListWorkflowTypes)
{-# DEPRECATED lwtName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The maximum number of results that are returned per call. Use @nextPageToken@ to obtain further pages of results.
--
-- /Note:/ Consider using 'maximumPageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwtMaximumPageSize :: Lens.Lens' ListWorkflowTypes (Lude.Maybe Lude.Natural)
lwtMaximumPageSize = Lens.lens (maximumPageSize :: ListWorkflowTypes -> Lude.Maybe Lude.Natural) (\s a -> s {maximumPageSize = a} :: ListWorkflowTypes)
{-# DEPRECATED lwtMaximumPageSize "Use generic-lens or generic-optics with 'maximumPageSize' instead." #-}

-- | The name of the domain in which the workflow types have been registered.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwtDomain :: Lens.Lens' ListWorkflowTypes Lude.Text
lwtDomain = Lens.lens (domain :: ListWorkflowTypes -> Lude.Text) (\s a -> s {domain = a} :: ListWorkflowTypes)
{-# DEPRECATED lwtDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | Specifies the registration status of the workflow types to list.
--
-- /Note:/ Consider using 'registrationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwtRegistrationStatus :: Lens.Lens' ListWorkflowTypes RegistrationStatus
lwtRegistrationStatus = Lens.lens (registrationStatus :: ListWorkflowTypes -> RegistrationStatus) (\s a -> s {registrationStatus = a} :: ListWorkflowTypes)
{-# DEPRECATED lwtRegistrationStatus "Use generic-lens or generic-optics with 'registrationStatus' instead." #-}

instance Page.AWSPager ListWorkflowTypes where
  page rq rs
    | Page.stop (rs Lens.^. lwtrsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lwtrsTypeInfos) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lwtNextPageToken Lens..~ rs Lens.^. lwtrsNextPageToken

instance Lude.AWSRequest ListWorkflowTypes where
  type Rs ListWorkflowTypes = ListWorkflowTypesResponse
  request = Req.postJSON swfService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListWorkflowTypesResponse'
            Lude.<$> (x Lude..?> "nextPageToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "typeInfos" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders ListWorkflowTypes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SimpleWorkflowService.ListWorkflowTypes" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListWorkflowTypes where
  toJSON ListWorkflowTypes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextPageToken" Lude..=) Lude.<$> nextPageToken,
            ("reverseOrder" Lude..=) Lude.<$> reverseOrder,
            ("name" Lude..=) Lude.<$> name,
            ("maximumPageSize" Lude..=) Lude.<$> maximumPageSize,
            Lude.Just ("domain" Lude..= domain),
            Lude.Just ("registrationStatus" Lude..= registrationStatus)
          ]
      )

instance Lude.ToPath ListWorkflowTypes where
  toPath = Lude.const "/"

instance Lude.ToQuery ListWorkflowTypes where
  toQuery = Lude.const Lude.mempty

-- | Contains a paginated list of information structures about workflow types.
--
-- /See:/ 'mkListWorkflowTypesResponse' smart constructor.
data ListWorkflowTypesResponse = ListWorkflowTypesResponse'
  { nextPageToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    typeInfos :: [WorkflowTypeInfo]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListWorkflowTypesResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
-- * 'responseStatus' - The response status code.
-- * 'typeInfos' - The list of workflow type information.
mkListWorkflowTypesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListWorkflowTypesResponse
mkListWorkflowTypesResponse pResponseStatus_ =
  ListWorkflowTypesResponse'
    { nextPageToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      typeInfos = Lude.mempty
    }

-- | If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwtrsNextPageToken :: Lens.Lens' ListWorkflowTypesResponse (Lude.Maybe Lude.Text)
lwtrsNextPageToken = Lens.lens (nextPageToken :: ListWorkflowTypesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: ListWorkflowTypesResponse)
{-# DEPRECATED lwtrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwtrsResponseStatus :: Lens.Lens' ListWorkflowTypesResponse Lude.Int
lwtrsResponseStatus = Lens.lens (responseStatus :: ListWorkflowTypesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListWorkflowTypesResponse)
{-# DEPRECATED lwtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The list of workflow type information.
--
-- /Note:/ Consider using 'typeInfos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwtrsTypeInfos :: Lens.Lens' ListWorkflowTypesResponse [WorkflowTypeInfo]
lwtrsTypeInfos = Lens.lens (typeInfos :: ListWorkflowTypesResponse -> [WorkflowTypeInfo]) (\s a -> s {typeInfos = a} :: ListWorkflowTypesResponse)
{-# DEPRECATED lwtrsTypeInfos "Use generic-lens or generic-optics with 'typeInfos' instead." #-}
