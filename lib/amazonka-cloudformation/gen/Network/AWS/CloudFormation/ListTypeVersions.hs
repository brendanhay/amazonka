{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.ListTypeVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summary information about the versions of a type.
module Network.AWS.CloudFormation.ListTypeVersions
  ( -- * Creating a request
    ListTypeVersions (..),
    mkListTypeVersions,

    -- ** Request lenses
    ltvTypeName,
    ltvARN,
    ltvNextToken,
    ltvDeprecatedStatus,
    ltvType,
    ltvMaxResults,

    -- * Destructuring the response
    ListTypeVersionsResponse (..),
    mkListTypeVersionsResponse,

    -- ** Response lenses
    ltvrsNextToken,
    ltvrsTypeVersionSummaries,
    ltvrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTypeVersions' smart constructor.
data ListTypeVersions = ListTypeVersions'
  { typeName ::
      Lude.Maybe Lude.Text,
    arn :: Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    deprecatedStatus :: Lude.Maybe DeprecatedStatus,
    type' :: Lude.Maybe RegistryType,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTypeVersions' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the type for which you want version summary information.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
-- * 'deprecatedStatus' - The deprecation status of the type versions that you want to get summary information about.
--
-- Valid values include:
--
--     * @LIVE@ : The type version is registered and can be used in CloudFormation operations, dependent on its provisioning behavior and visibility scope.
--
--
--     * @DEPRECATED@ : The type version has been deregistered and can no longer be used in CloudFormation operations.
--
--
-- The default is @LIVE@ .
-- * 'maxResults' - The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
-- * 'nextToken' - If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
-- * 'type'' - The kind of the type.
--
-- Currently the only valid value is @RESOURCE@ .
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
-- * 'typeName' - The name of the type for which you want version summary information.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
mkListTypeVersions ::
  ListTypeVersions
mkListTypeVersions =
  ListTypeVersions'
    { typeName = Lude.Nothing,
      arn = Lude.Nothing,
      nextToken = Lude.Nothing,
      deprecatedStatus = Lude.Nothing,
      type' = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The name of the type for which you want version summary information.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvTypeName :: Lens.Lens' ListTypeVersions (Lude.Maybe Lude.Text)
ltvTypeName = Lens.lens (typeName :: ListTypeVersions -> Lude.Maybe Lude.Text) (\s a -> s {typeName = a} :: ListTypeVersions)
{-# DEPRECATED ltvTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The Amazon Resource Name (ARN) of the type for which you want version summary information.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvARN :: Lens.Lens' ListTypeVersions (Lude.Maybe Lude.Text)
ltvARN = Lens.lens (arn :: ListTypeVersions -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: ListTypeVersions)
{-# DEPRECATED ltvARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvNextToken :: Lens.Lens' ListTypeVersions (Lude.Maybe Lude.Text)
ltvNextToken = Lens.lens (nextToken :: ListTypeVersions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTypeVersions)
{-# DEPRECATED ltvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The deprecation status of the type versions that you want to get summary information about.
--
-- Valid values include:
--
--     * @LIVE@ : The type version is registered and can be used in CloudFormation operations, dependent on its provisioning behavior and visibility scope.
--
--
--     * @DEPRECATED@ : The type version has been deregistered and can no longer be used in CloudFormation operations.
--
--
-- The default is @LIVE@ .
--
-- /Note:/ Consider using 'deprecatedStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvDeprecatedStatus :: Lens.Lens' ListTypeVersions (Lude.Maybe DeprecatedStatus)
ltvDeprecatedStatus = Lens.lens (deprecatedStatus :: ListTypeVersions -> Lude.Maybe DeprecatedStatus) (\s a -> s {deprecatedStatus = a} :: ListTypeVersions)
{-# DEPRECATED ltvDeprecatedStatus "Use generic-lens or generic-optics with 'deprecatedStatus' instead." #-}

-- | The kind of the type.
--
-- Currently the only valid value is @RESOURCE@ .
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvType :: Lens.Lens' ListTypeVersions (Lude.Maybe RegistryType)
ltvType = Lens.lens (type' :: ListTypeVersions -> Lude.Maybe RegistryType) (\s a -> s {type' = a} :: ListTypeVersions)
{-# DEPRECATED ltvType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvMaxResults :: Lens.Lens' ListTypeVersions (Lude.Maybe Lude.Natural)
ltvMaxResults = Lens.lens (maxResults :: ListTypeVersions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListTypeVersions)
{-# DEPRECATED ltvMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListTypeVersions where
  type Rs ListTypeVersions = ListTypeVersionsResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "ListTypeVersionsResult"
      ( \s h x ->
          ListTypeVersionsResponse'
            Lude.<$> (x Lude..@? "NextToken")
            Lude.<*> ( x Lude..@? "TypeVersionSummaries" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTypeVersions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListTypeVersions where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTypeVersions where
  toQuery ListTypeVersions' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListTypeVersions" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "TypeName" Lude.=: typeName,
        "Arn" Lude.=: arn,
        "NextToken" Lude.=: nextToken,
        "DeprecatedStatus" Lude.=: deprecatedStatus,
        "Type" Lude.=: type',
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkListTypeVersionsResponse' smart constructor.
data ListTypeVersionsResponse = ListTypeVersionsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    typeVersionSummaries ::
      Lude.Maybe [TypeVersionSummary],
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

-- | Creates a value of 'ListTypeVersionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
-- * 'responseStatus' - The response status code.
-- * 'typeVersionSummaries' - A list of @TypeVersionSummary@ structures that contain information about the specified type's versions.
mkListTypeVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTypeVersionsResponse
mkListTypeVersionsResponse pResponseStatus_ =
  ListTypeVersionsResponse'
    { nextToken = Lude.Nothing,
      typeVersionSummaries = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvrsNextToken :: Lens.Lens' ListTypeVersionsResponse (Lude.Maybe Lude.Text)
ltvrsNextToken = Lens.lens (nextToken :: ListTypeVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTypeVersionsResponse)
{-# DEPRECATED ltvrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of @TypeVersionSummary@ structures that contain information about the specified type's versions.
--
-- /Note:/ Consider using 'typeVersionSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvrsTypeVersionSummaries :: Lens.Lens' ListTypeVersionsResponse (Lude.Maybe [TypeVersionSummary])
ltvrsTypeVersionSummaries = Lens.lens (typeVersionSummaries :: ListTypeVersionsResponse -> Lude.Maybe [TypeVersionSummary]) (\s a -> s {typeVersionSummaries = a} :: ListTypeVersionsResponse)
{-# DEPRECATED ltvrsTypeVersionSummaries "Use generic-lens or generic-optics with 'typeVersionSummaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvrsResponseStatus :: Lens.Lens' ListTypeVersionsResponse Lude.Int
ltvrsResponseStatus = Lens.lens (responseStatus :: ListTypeVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTypeVersionsResponse)
{-# DEPRECATED ltvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
