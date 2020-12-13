{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.ListTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summary information about types that have been registered with CloudFormation.
module Network.AWS.CloudFormation.ListTypes
  ( -- * Creating a request
    ListTypes (..),
    mkListTypes,

    -- ** Request lenses
    ltVisibility,
    ltNextToken,
    ltDeprecatedStatus,
    ltType,
    ltMaxResults,
    ltProvisioningType,

    -- * Destructuring the response
    ListTypesResponse (..),
    mkListTypesResponse,

    -- ** Response lenses
    ltrsTypeSummaries,
    ltrsNextToken,
    ltrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTypes' smart constructor.
data ListTypes = ListTypes'
  { -- | The scope at which the type is visible and usable in CloudFormation operations.
    --
    -- Valid values include:
    --
    --     * @PRIVATE@ : The type is only visible and usable within the account in which it is registered. Currently, AWS CloudFormation marks any types you create as @PRIVATE@ .
    --
    --
    --     * @PUBLIC@ : The type is publically visible and usable within any Amazon account.
    --
    --
    -- The default is @PRIVATE@ .
    visibility :: Lude.Maybe Visibility,
    -- | If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | The deprecation status of the types that you want to get summary information about.
    --
    -- Valid values include:
    --
    --     * @LIVE@ : The type is registered for use in CloudFormation operations.
    --
    --
    --     * @DEPRECATED@ : The type has been deregistered and can no longer be used in CloudFormation operations.
    deprecatedStatus :: Lude.Maybe DeprecatedStatus,
    -- | The type of extension.
    type' :: Lude.Maybe RegistryType,
    -- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | The provisioning behavior of the type. AWS CloudFormation determines the provisioning type during registration, based on the types of handlers in the schema handler package submitted.
    --
    -- Valid values include:
    --
    --     * @FULLY_MUTABLE@ : The type includes an update handler to process updates to the type during stack update operations.
    --
    --
    --     * @IMMUTABLE@ : The type does not include an update handler, so the type cannot be updated and must instead be replaced during stack update operations.
    --
    --
    --     * @NON_PROVISIONABLE@ : The type does not include create, read, and delete handlers, and therefore cannot actually be provisioned.
    provisioningType :: Lude.Maybe ProvisioningType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTypes' with the minimum fields required to make a request.
--
-- * 'visibility' - The scope at which the type is visible and usable in CloudFormation operations.
--
-- Valid values include:
--
--     * @PRIVATE@ : The type is only visible and usable within the account in which it is registered. Currently, AWS CloudFormation marks any types you create as @PRIVATE@ .
--
--
--     * @PUBLIC@ : The type is publically visible and usable within any Amazon account.
--
--
-- The default is @PRIVATE@ .
-- * 'nextToken' - If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
-- * 'deprecatedStatus' - The deprecation status of the types that you want to get summary information about.
--
-- Valid values include:
--
--     * @LIVE@ : The type is registered for use in CloudFormation operations.
--
--
--     * @DEPRECATED@ : The type has been deregistered and can no longer be used in CloudFormation operations.
--
--
-- * 'type'' - The type of extension.
-- * 'maxResults' - The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
-- * 'provisioningType' - The provisioning behavior of the type. AWS CloudFormation determines the provisioning type during registration, based on the types of handlers in the schema handler package submitted.
--
-- Valid values include:
--
--     * @FULLY_MUTABLE@ : The type includes an update handler to process updates to the type during stack update operations.
--
--
--     * @IMMUTABLE@ : The type does not include an update handler, so the type cannot be updated and must instead be replaced during stack update operations.
--
--
--     * @NON_PROVISIONABLE@ : The type does not include create, read, and delete handlers, and therefore cannot actually be provisioned.
mkListTypes ::
  ListTypes
mkListTypes =
  ListTypes'
    { visibility = Lude.Nothing,
      nextToken = Lude.Nothing,
      deprecatedStatus = Lude.Nothing,
      type' = Lude.Nothing,
      maxResults = Lude.Nothing,
      provisioningType = Lude.Nothing
    }

-- | The scope at which the type is visible and usable in CloudFormation operations.
--
-- Valid values include:
--
--     * @PRIVATE@ : The type is only visible and usable within the account in which it is registered. Currently, AWS CloudFormation marks any types you create as @PRIVATE@ .
--
--
--     * @PUBLIC@ : The type is publically visible and usable within any Amazon account.
--
--
-- The default is @PRIVATE@ .
--
-- /Note:/ Consider using 'visibility' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltVisibility :: Lens.Lens' ListTypes (Lude.Maybe Visibility)
ltVisibility = Lens.lens (visibility :: ListTypes -> Lude.Maybe Visibility) (\s a -> s {visibility = a} :: ListTypes)
{-# DEPRECATED ltVisibility "Use generic-lens or generic-optics with 'visibility' instead." #-}

-- | If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltNextToken :: Lens.Lens' ListTypes (Lude.Maybe Lude.Text)
ltNextToken = Lens.lens (nextToken :: ListTypes -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTypes)
{-# DEPRECATED ltNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The deprecation status of the types that you want to get summary information about.
--
-- Valid values include:
--
--     * @LIVE@ : The type is registered for use in CloudFormation operations.
--
--
--     * @DEPRECATED@ : The type has been deregistered and can no longer be used in CloudFormation operations.
--
--
--
-- /Note:/ Consider using 'deprecatedStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltDeprecatedStatus :: Lens.Lens' ListTypes (Lude.Maybe DeprecatedStatus)
ltDeprecatedStatus = Lens.lens (deprecatedStatus :: ListTypes -> Lude.Maybe DeprecatedStatus) (\s a -> s {deprecatedStatus = a} :: ListTypes)
{-# DEPRECATED ltDeprecatedStatus "Use generic-lens or generic-optics with 'deprecatedStatus' instead." #-}

-- | The type of extension.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltType :: Lens.Lens' ListTypes (Lude.Maybe RegistryType)
ltType = Lens.lens (type' :: ListTypes -> Lude.Maybe RegistryType) (\s a -> s {type' = a} :: ListTypes)
{-# DEPRECATED ltType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltMaxResults :: Lens.Lens' ListTypes (Lude.Maybe Lude.Natural)
ltMaxResults = Lens.lens (maxResults :: ListTypes -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListTypes)
{-# DEPRECATED ltMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The provisioning behavior of the type. AWS CloudFormation determines the provisioning type during registration, based on the types of handlers in the schema handler package submitted.
--
-- Valid values include:
--
--     * @FULLY_MUTABLE@ : The type includes an update handler to process updates to the type during stack update operations.
--
--
--     * @IMMUTABLE@ : The type does not include an update handler, so the type cannot be updated and must instead be replaced during stack update operations.
--
--
--     * @NON_PROVISIONABLE@ : The type does not include create, read, and delete handlers, and therefore cannot actually be provisioned.
--
--
--
-- /Note:/ Consider using 'provisioningType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltProvisioningType :: Lens.Lens' ListTypes (Lude.Maybe ProvisioningType)
ltProvisioningType = Lens.lens (provisioningType :: ListTypes -> Lude.Maybe ProvisioningType) (\s a -> s {provisioningType = a} :: ListTypes)
{-# DEPRECATED ltProvisioningType "Use generic-lens or generic-optics with 'provisioningType' instead." #-}

instance Lude.AWSRequest ListTypes where
  type Rs ListTypes = ListTypesResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "ListTypesResult"
      ( \s h x ->
          ListTypesResponse'
            Lude.<$> ( x Lude..@? "TypeSummaries" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTypes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListTypes where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTypes where
  toQuery ListTypes' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListTypes" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "Visibility" Lude.=: visibility,
        "NextToken" Lude.=: nextToken,
        "DeprecatedStatus" Lude.=: deprecatedStatus,
        "Type" Lude.=: type',
        "MaxResults" Lude.=: maxResults,
        "ProvisioningType" Lude.=: provisioningType
      ]

-- | /See:/ 'mkListTypesResponse' smart constructor.
data ListTypesResponse = ListTypesResponse'
  { -- | A list of @TypeSummary@ structures that contain information about the specified types.
    typeSummaries :: Lude.Maybe [TypeSummary],
    -- | If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTypesResponse' with the minimum fields required to make a request.
--
-- * 'typeSummaries' - A list of @TypeSummary@ structures that contain information about the specified types.
-- * 'nextToken' - If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
-- * 'responseStatus' - The response status code.
mkListTypesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTypesResponse
mkListTypesResponse pResponseStatus_ =
  ListTypesResponse'
    { typeSummaries = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of @TypeSummary@ structures that contain information about the specified types.
--
-- /Note:/ Consider using 'typeSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsTypeSummaries :: Lens.Lens' ListTypesResponse (Lude.Maybe [TypeSummary])
ltrsTypeSummaries = Lens.lens (typeSummaries :: ListTypesResponse -> Lude.Maybe [TypeSummary]) (\s a -> s {typeSummaries = a} :: ListTypesResponse)
{-# DEPRECATED ltrsTypeSummaries "Use generic-lens or generic-optics with 'typeSummaries' instead." #-}

-- | If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsNextToken :: Lens.Lens' ListTypesResponse (Lude.Maybe Lude.Text)
ltrsNextToken = Lens.lens (nextToken :: ListTypesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTypesResponse)
{-# DEPRECATED ltrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsResponseStatus :: Lens.Lens' ListTypesResponse Lude.Int
ltrsResponseStatus = Lens.lens (responseStatus :: ListTypesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTypesResponse)
{-# DEPRECATED ltrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
