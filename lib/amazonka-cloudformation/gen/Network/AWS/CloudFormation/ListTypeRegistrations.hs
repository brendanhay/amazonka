{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.ListTypeRegistrations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of registration tokens for the specified type(s).
module Network.AWS.CloudFormation.ListTypeRegistrations
  ( -- * Creating a request
    ListTypeRegistrations (..),
    mkListTypeRegistrations,

    -- ** Request lenses
    ltrTypeName,
    ltrRegistrationStatusFilter,
    ltrNextToken,
    ltrTypeARN,
    ltrType,
    ltrMaxResults,

    -- * Destructuring the response
    ListTypeRegistrationsResponse (..),
    mkListTypeRegistrationsResponse,

    -- ** Response lenses
    ltrrsRegistrationTokenList,
    ltrrsNextToken,
    ltrrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTypeRegistrations' smart constructor.
data ListTypeRegistrations = ListTypeRegistrations'
  { typeName ::
      Lude.Maybe Lude.Text,
    registrationStatusFilter ::
      Lude.Maybe RegistrationStatus,
    nextToken :: Lude.Maybe Lude.Text,
    typeARN :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListTypeRegistrations' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
-- * 'nextToken' - If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
-- * 'registrationStatusFilter' - The current status of the type registration request.
--
-- The default is @IN_PROGRESS@ .
-- * 'type'' - The kind of type.
--
-- Currently the only valid value is @RESOURCE@ .
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
-- * 'typeARN' - The Amazon Resource Name (ARN) of the type.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
-- * 'typeName' - The name of the type.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
mkListTypeRegistrations ::
  ListTypeRegistrations
mkListTypeRegistrations =
  ListTypeRegistrations'
    { typeName = Lude.Nothing,
      registrationStatusFilter = Lude.Nothing,
      nextToken = Lude.Nothing,
      typeARN = Lude.Nothing,
      type' = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The name of the type.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrTypeName :: Lens.Lens' ListTypeRegistrations (Lude.Maybe Lude.Text)
ltrTypeName = Lens.lens (typeName :: ListTypeRegistrations -> Lude.Maybe Lude.Text) (\s a -> s {typeName = a} :: ListTypeRegistrations)
{-# DEPRECATED ltrTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The current status of the type registration request.
--
-- The default is @IN_PROGRESS@ .
--
-- /Note:/ Consider using 'registrationStatusFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrRegistrationStatusFilter :: Lens.Lens' ListTypeRegistrations (Lude.Maybe RegistrationStatus)
ltrRegistrationStatusFilter = Lens.lens (registrationStatusFilter :: ListTypeRegistrations -> Lude.Maybe RegistrationStatus) (\s a -> s {registrationStatusFilter = a} :: ListTypeRegistrations)
{-# DEPRECATED ltrRegistrationStatusFilter "Use generic-lens or generic-optics with 'registrationStatusFilter' instead." #-}

-- | If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrNextToken :: Lens.Lens' ListTypeRegistrations (Lude.Maybe Lude.Text)
ltrNextToken = Lens.lens (nextToken :: ListTypeRegistrations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTypeRegistrations)
{-# DEPRECATED ltrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The Amazon Resource Name (ARN) of the type.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'typeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrTypeARN :: Lens.Lens' ListTypeRegistrations (Lude.Maybe Lude.Text)
ltrTypeARN = Lens.lens (typeARN :: ListTypeRegistrations -> Lude.Maybe Lude.Text) (\s a -> s {typeARN = a} :: ListTypeRegistrations)
{-# DEPRECATED ltrTypeARN "Use generic-lens or generic-optics with 'typeARN' instead." #-}

-- | The kind of type.
--
-- Currently the only valid value is @RESOURCE@ .
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrType :: Lens.Lens' ListTypeRegistrations (Lude.Maybe RegistryType)
ltrType = Lens.lens (type' :: ListTypeRegistrations -> Lude.Maybe RegistryType) (\s a -> s {type' = a} :: ListTypeRegistrations)
{-# DEPRECATED ltrType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrMaxResults :: Lens.Lens' ListTypeRegistrations (Lude.Maybe Lude.Natural)
ltrMaxResults = Lens.lens (maxResults :: ListTypeRegistrations -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListTypeRegistrations)
{-# DEPRECATED ltrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListTypeRegistrations where
  type Rs ListTypeRegistrations = ListTypeRegistrationsResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "ListTypeRegistrationsResult"
      ( \s h x ->
          ListTypeRegistrationsResponse'
            Lude.<$> ( x Lude..@? "RegistrationTokenList" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTypeRegistrations where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListTypeRegistrations where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTypeRegistrations where
  toQuery ListTypeRegistrations' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListTypeRegistrations" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "TypeName" Lude.=: typeName,
        "RegistrationStatusFilter" Lude.=: registrationStatusFilter,
        "NextToken" Lude.=: nextToken,
        "TypeArn" Lude.=: typeARN,
        "Type" Lude.=: type',
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkListTypeRegistrationsResponse' smart constructor.
data ListTypeRegistrationsResponse = ListTypeRegistrationsResponse'
  { registrationTokenList ::
      Lude.Maybe [Lude.Text],
    nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListTypeRegistrationsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
-- * 'registrationTokenList' - A list of type registration tokens.
--
-- Use @'DescribeTypeRegistration' @ to return detailed information about a type registration request.
-- * 'responseStatus' - The response status code.
mkListTypeRegistrationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTypeRegistrationsResponse
mkListTypeRegistrationsResponse pResponseStatus_ =
  ListTypeRegistrationsResponse'
    { registrationTokenList =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of type registration tokens.
--
-- Use @'DescribeTypeRegistration' @ to return detailed information about a type registration request.
--
-- /Note:/ Consider using 'registrationTokenList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsRegistrationTokenList :: Lens.Lens' ListTypeRegistrationsResponse (Lude.Maybe [Lude.Text])
ltrrsRegistrationTokenList = Lens.lens (registrationTokenList :: ListTypeRegistrationsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {registrationTokenList = a} :: ListTypeRegistrationsResponse)
{-# DEPRECATED ltrrsRegistrationTokenList "Use generic-lens or generic-optics with 'registrationTokenList' instead." #-}

-- | If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsNextToken :: Lens.Lens' ListTypeRegistrationsResponse (Lude.Maybe Lude.Text)
ltrrsNextToken = Lens.lens (nextToken :: ListTypeRegistrationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTypeRegistrationsResponse)
{-# DEPRECATED ltrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsResponseStatus :: Lens.Lens' ListTypeRegistrationsResponse Lude.Int
ltrrsResponseStatus = Lens.lens (responseStatus :: ListTypeRegistrationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTypeRegistrationsResponse)
{-# DEPRECATED ltrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
