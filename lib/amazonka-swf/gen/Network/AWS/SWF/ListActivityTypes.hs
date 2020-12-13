{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.ListActivityTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all activities registered in the specified domain that match the specified name and registration status. The result includes information like creation date, current status of the activity, etc. The results may be split into multiple pages. To retrieve subsequent pages, make the call again using the @nextPageToken@ returned by the initial call.
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
module Network.AWS.SWF.ListActivityTypes
  ( -- * Creating a request
    ListActivityTypes (..),
    mkListActivityTypes,

    -- ** Request lenses
    latNextPageToken,
    latDomain,
    latReverseOrder,
    latName,
    latRegistrationStatus,
    latMaximumPageSize,

    -- * Destructuring the response
    ListActivityTypesResponse (..),
    mkListActivityTypesResponse,

    -- ** Response lenses
    latrsNextPageToken,
    latrsTypeInfos,
    latrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SWF.Types

-- | /See:/ 'mkListActivityTypes' smart constructor.
data ListActivityTypes = ListActivityTypes'
  { -- | If @NextPageToken@ is returned there are more results available. The value of @NextPageToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 60 seconds. Using an expired pagination token will return a @400@ error: "@Specified token has exceeded its maximum lifetime@ ".
    --
    -- The configured @maximumPageSize@ determines how many results can be returned in a single call.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | The name of the domain in which the activity types have been registered.
    domain :: Lude.Text,
    -- | When set to @true@ , returns the results in reverse order. By default, the results are returned in ascending alphabetical order by @name@ of the activity types.
    reverseOrder :: Lude.Maybe Lude.Bool,
    -- | If specified, only lists the activity types that have this name.
    name :: Lude.Maybe Lude.Text,
    -- | Specifies the registration status of the activity types to list.
    registrationStatus :: RegistrationStatus,
    -- | The maximum number of results that are returned per call. Use @nextPageToken@ to obtain further pages of results.
    maximumPageSize :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListActivityTypes' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - If @NextPageToken@ is returned there are more results available. The value of @NextPageToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 60 seconds. Using an expired pagination token will return a @400@ error: "@Specified token has exceeded its maximum lifetime@ ".
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
-- * 'domain' - The name of the domain in which the activity types have been registered.
-- * 'reverseOrder' - When set to @true@ , returns the results in reverse order. By default, the results are returned in ascending alphabetical order by @name@ of the activity types.
-- * 'name' - If specified, only lists the activity types that have this name.
-- * 'registrationStatus' - Specifies the registration status of the activity types to list.
-- * 'maximumPageSize' - The maximum number of results that are returned per call. Use @nextPageToken@ to obtain further pages of results.
mkListActivityTypes ::
  -- | 'domain'
  Lude.Text ->
  -- | 'registrationStatus'
  RegistrationStatus ->
  ListActivityTypes
mkListActivityTypes pDomain_ pRegistrationStatus_ =
  ListActivityTypes'
    { nextPageToken = Lude.Nothing,
      domain = pDomain_,
      reverseOrder = Lude.Nothing,
      name = Lude.Nothing,
      registrationStatus = pRegistrationStatus_,
      maximumPageSize = Lude.Nothing
    }

-- | If @NextPageToken@ is returned there are more results available. The value of @NextPageToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 60 seconds. Using an expired pagination token will return a @400@ error: "@Specified token has exceeded its maximum lifetime@ ".
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latNextPageToken :: Lens.Lens' ListActivityTypes (Lude.Maybe Lude.Text)
latNextPageToken = Lens.lens (nextPageToken :: ListActivityTypes -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: ListActivityTypes)
{-# DEPRECATED latNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The name of the domain in which the activity types have been registered.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latDomain :: Lens.Lens' ListActivityTypes Lude.Text
latDomain = Lens.lens (domain :: ListActivityTypes -> Lude.Text) (\s a -> s {domain = a} :: ListActivityTypes)
{-# DEPRECATED latDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | When set to @true@ , returns the results in reverse order. By default, the results are returned in ascending alphabetical order by @name@ of the activity types.
--
-- /Note:/ Consider using 'reverseOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latReverseOrder :: Lens.Lens' ListActivityTypes (Lude.Maybe Lude.Bool)
latReverseOrder = Lens.lens (reverseOrder :: ListActivityTypes -> Lude.Maybe Lude.Bool) (\s a -> s {reverseOrder = a} :: ListActivityTypes)
{-# DEPRECATED latReverseOrder "Use generic-lens or generic-optics with 'reverseOrder' instead." #-}

-- | If specified, only lists the activity types that have this name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latName :: Lens.Lens' ListActivityTypes (Lude.Maybe Lude.Text)
latName = Lens.lens (name :: ListActivityTypes -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ListActivityTypes)
{-# DEPRECATED latName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specifies the registration status of the activity types to list.
--
-- /Note:/ Consider using 'registrationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latRegistrationStatus :: Lens.Lens' ListActivityTypes RegistrationStatus
latRegistrationStatus = Lens.lens (registrationStatus :: ListActivityTypes -> RegistrationStatus) (\s a -> s {registrationStatus = a} :: ListActivityTypes)
{-# DEPRECATED latRegistrationStatus "Use generic-lens or generic-optics with 'registrationStatus' instead." #-}

-- | The maximum number of results that are returned per call. Use @nextPageToken@ to obtain further pages of results.
--
-- /Note:/ Consider using 'maximumPageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latMaximumPageSize :: Lens.Lens' ListActivityTypes (Lude.Maybe Lude.Natural)
latMaximumPageSize = Lens.lens (maximumPageSize :: ListActivityTypes -> Lude.Maybe Lude.Natural) (\s a -> s {maximumPageSize = a} :: ListActivityTypes)
{-# DEPRECATED latMaximumPageSize "Use generic-lens or generic-optics with 'maximumPageSize' instead." #-}

instance Page.AWSPager ListActivityTypes where
  page rq rs
    | Page.stop (rs Lens.^. latrsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. latrsTypeInfos) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& latNextPageToken Lens..~ rs Lens.^. latrsNextPageToken

instance Lude.AWSRequest ListActivityTypes where
  type Rs ListActivityTypes = ListActivityTypesResponse
  request = Req.postJSON swfService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListActivityTypesResponse'
            Lude.<$> (x Lude..?> "nextPageToken")
            Lude.<*> (x Lude..?> "typeInfos" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListActivityTypes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SimpleWorkflowService.ListActivityTypes" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListActivityTypes where
  toJSON ListActivityTypes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextPageToken" Lude..=) Lude.<$> nextPageToken,
            Lude.Just ("domain" Lude..= domain),
            ("reverseOrder" Lude..=) Lude.<$> reverseOrder,
            ("name" Lude..=) Lude.<$> name,
            Lude.Just ("registrationStatus" Lude..= registrationStatus),
            ("maximumPageSize" Lude..=) Lude.<$> maximumPageSize
          ]
      )

instance Lude.ToPath ListActivityTypes where
  toPath = Lude.const "/"

instance Lude.ToQuery ListActivityTypes where
  toQuery = Lude.const Lude.mempty

-- | Contains a paginated list of activity type information structures.
--
-- /See:/ 'mkListActivityTypesResponse' smart constructor.
data ListActivityTypesResponse = ListActivityTypesResponse'
  { -- | If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged.
    --
    -- The configured @maximumPageSize@ determines how many results can be returned in a single call.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | List of activity type information.
    typeInfos :: [ActivityTypeInfo],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListActivityTypesResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
-- * 'typeInfos' - List of activity type information.
-- * 'responseStatus' - The response status code.
mkListActivityTypesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListActivityTypesResponse
mkListActivityTypesResponse pResponseStatus_ =
  ListActivityTypesResponse'
    { nextPageToken = Lude.Nothing,
      typeInfos = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latrsNextPageToken :: Lens.Lens' ListActivityTypesResponse (Lude.Maybe Lude.Text)
latrsNextPageToken = Lens.lens (nextPageToken :: ListActivityTypesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: ListActivityTypesResponse)
{-# DEPRECATED latrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | List of activity type information.
--
-- /Note:/ Consider using 'typeInfos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latrsTypeInfos :: Lens.Lens' ListActivityTypesResponse [ActivityTypeInfo]
latrsTypeInfos = Lens.lens (typeInfos :: ListActivityTypesResponse -> [ActivityTypeInfo]) (\s a -> s {typeInfos = a} :: ListActivityTypesResponse)
{-# DEPRECATED latrsTypeInfos "Use generic-lens or generic-optics with 'typeInfos' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latrsResponseStatus :: Lens.Lens' ListActivityTypesResponse Lude.Int
latrsResponseStatus = Lens.lens (responseStatus :: ListActivityTypesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListActivityTypesResponse)
{-# DEPRECATED latrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
