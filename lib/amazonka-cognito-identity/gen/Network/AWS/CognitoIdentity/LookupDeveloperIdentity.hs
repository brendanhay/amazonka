{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.LookupDeveloperIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the @IdentityID@ associated with a @DeveloperUserIdentifier@ or the list of @DeveloperUserIdentifier@ values associated with an @IdentityId@ for an existing identity. Either @IdentityID@ or @DeveloperUserIdentifier@ must not be null. If you supply only one of these values, the other value will be searched in the database and returned as a part of the response. If you supply both, @DeveloperUserIdentifier@ will be matched against @IdentityID@ . If the values are verified against the database, the response returns both values and is the same as the request. Otherwise a @ResourceConflictException@ is thrown.
--
-- @LookupDeveloperIdentity@ is intended for low-throughput control plane operations: for example, to enable customer service to locate an identity ID by username. If you are using it for higher-volume operations such as user authentication, your requests are likely to be throttled. 'GetOpenIdTokenForDeveloperIdentity' is a better option for higher-volume operations for user authentication.
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.LookupDeveloperIdentity
  ( -- * Creating a request
    LookupDeveloperIdentity (..),
    mkLookupDeveloperIdentity,

    -- ** Request lenses
    ldiDeveloperUserIdentifier,
    ldiIdentityPoolId,
    ldiNextToken,
    ldiIdentityId,
    ldiMaxResults,

    -- * Destructuring the response
    LookupDeveloperIdentityResponse (..),
    mkLookupDeveloperIdentityResponse,

    -- ** Response lenses
    ldirsNextToken,
    ldirsIdentityId,
    ldirsDeveloperUserIdentifierList,
    ldirsResponseStatus,
  )
where

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Input to the @LookupDeveloperIdentityInput@ action.
--
-- /See:/ 'mkLookupDeveloperIdentity' smart constructor.
data LookupDeveloperIdentity = LookupDeveloperIdentity'
  { -- | A unique ID used by your backend authentication process to identify a user. Typically, a developer identity provider would issue many developer user identifiers, in keeping with the number of users.
    developerUserIdentifier :: Lude.Maybe Lude.Text,
    -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Lude.Text,
    -- | A pagination token. The first call you make will have @NextToken@ set to null. After that the service will return @NextToken@ values as needed. For example, let's say you make a request with @MaxResults@ set to 10, and there are 20 matches in the database. The service will return a pagination token as a part of the response. This token can be used to call the API again and get results starting from the 11th match.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A unique identifier in the format REGION:GUID.
    identityId :: Lude.Maybe Lude.Text,
    -- | The maximum number of identities to return.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LookupDeveloperIdentity' with the minimum fields required to make a request.
--
-- * 'developerUserIdentifier' - A unique ID used by your backend authentication process to identify a user. Typically, a developer identity provider would issue many developer user identifiers, in keeping with the number of users.
-- * 'identityPoolId' - An identity pool ID in the format REGION:GUID.
-- * 'nextToken' - A pagination token. The first call you make will have @NextToken@ set to null. After that the service will return @NextToken@ values as needed. For example, let's say you make a request with @MaxResults@ set to 10, and there are 20 matches in the database. The service will return a pagination token as a part of the response. This token can be used to call the API again and get results starting from the 11th match.
-- * 'identityId' - A unique identifier in the format REGION:GUID.
-- * 'maxResults' - The maximum number of identities to return.
mkLookupDeveloperIdentity ::
  -- | 'identityPoolId'
  Lude.Text ->
  LookupDeveloperIdentity
mkLookupDeveloperIdentity pIdentityPoolId_ =
  LookupDeveloperIdentity'
    { developerUserIdentifier = Lude.Nothing,
      identityPoolId = pIdentityPoolId_,
      nextToken = Lude.Nothing,
      identityId = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | A unique ID used by your backend authentication process to identify a user. Typically, a developer identity provider would issue many developer user identifiers, in keeping with the number of users.
--
-- /Note:/ Consider using 'developerUserIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldiDeveloperUserIdentifier :: Lens.Lens' LookupDeveloperIdentity (Lude.Maybe Lude.Text)
ldiDeveloperUserIdentifier = Lens.lens (developerUserIdentifier :: LookupDeveloperIdentity -> Lude.Maybe Lude.Text) (\s a -> s {developerUserIdentifier = a} :: LookupDeveloperIdentity)
{-# DEPRECATED ldiDeveloperUserIdentifier "Use generic-lens or generic-optics with 'developerUserIdentifier' instead." #-}

-- | An identity pool ID in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldiIdentityPoolId :: Lens.Lens' LookupDeveloperIdentity Lude.Text
ldiIdentityPoolId = Lens.lens (identityPoolId :: LookupDeveloperIdentity -> Lude.Text) (\s a -> s {identityPoolId = a} :: LookupDeveloperIdentity)
{-# DEPRECATED ldiIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | A pagination token. The first call you make will have @NextToken@ set to null. After that the service will return @NextToken@ values as needed. For example, let's say you make a request with @MaxResults@ set to 10, and there are 20 matches in the database. The service will return a pagination token as a part of the response. This token can be used to call the API again and get results starting from the 11th match.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldiNextToken :: Lens.Lens' LookupDeveloperIdentity (Lude.Maybe Lude.Text)
ldiNextToken = Lens.lens (nextToken :: LookupDeveloperIdentity -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: LookupDeveloperIdentity)
{-# DEPRECATED ldiNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A unique identifier in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldiIdentityId :: Lens.Lens' LookupDeveloperIdentity (Lude.Maybe Lude.Text)
ldiIdentityId = Lens.lens (identityId :: LookupDeveloperIdentity -> Lude.Maybe Lude.Text) (\s a -> s {identityId = a} :: LookupDeveloperIdentity)
{-# DEPRECATED ldiIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | The maximum number of identities to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldiMaxResults :: Lens.Lens' LookupDeveloperIdentity (Lude.Maybe Lude.Natural)
ldiMaxResults = Lens.lens (maxResults :: LookupDeveloperIdentity -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: LookupDeveloperIdentity)
{-# DEPRECATED ldiMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest LookupDeveloperIdentity where
  type Rs LookupDeveloperIdentity = LookupDeveloperIdentityResponse
  request = Req.postJSON cognitoIdentityService
  response =
    Res.receiveJSON
      ( \s h x ->
          LookupDeveloperIdentityResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "IdentityId")
            Lude.<*> (x Lude..?> "DeveloperUserIdentifierList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders LookupDeveloperIdentity where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityService.LookupDeveloperIdentity" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON LookupDeveloperIdentity where
  toJSON LookupDeveloperIdentity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DeveloperUserIdentifier" Lude..=)
              Lude.<$> developerUserIdentifier,
            Lude.Just ("IdentityPoolId" Lude..= identityPoolId),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("IdentityId" Lude..=) Lude.<$> identityId,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath LookupDeveloperIdentity where
  toPath = Lude.const "/"

instance Lude.ToQuery LookupDeveloperIdentity where
  toQuery = Lude.const Lude.mempty

-- | Returned in response to a successful @LookupDeveloperIdentity@ action.
--
-- /See:/ 'mkLookupDeveloperIdentityResponse' smart constructor.
data LookupDeveloperIdentityResponse = LookupDeveloperIdentityResponse'
  { -- | A pagination token. The first call you make will have @NextToken@ set to null. After that the service will return @NextToken@ values as needed. For example, let's say you make a request with @MaxResults@ set to 10, and there are 20 matches in the database. The service will return a pagination token as a part of the response. This token can be used to call the API again and get results starting from the 11th match.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A unique identifier in the format REGION:GUID.
    identityId :: Lude.Maybe Lude.Text,
    -- | This is the list of developer user identifiers associated with an identity ID. Cognito supports the association of multiple developer user identifiers with an identity ID.
    developerUserIdentifierList :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LookupDeveloperIdentityResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A pagination token. The first call you make will have @NextToken@ set to null. After that the service will return @NextToken@ values as needed. For example, let's say you make a request with @MaxResults@ set to 10, and there are 20 matches in the database. The service will return a pagination token as a part of the response. This token can be used to call the API again and get results starting from the 11th match.
-- * 'identityId' - A unique identifier in the format REGION:GUID.
-- * 'developerUserIdentifierList' - This is the list of developer user identifiers associated with an identity ID. Cognito supports the association of multiple developer user identifiers with an identity ID.
-- * 'responseStatus' - The response status code.
mkLookupDeveloperIdentityResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  LookupDeveloperIdentityResponse
mkLookupDeveloperIdentityResponse pResponseStatus_ =
  LookupDeveloperIdentityResponse'
    { nextToken = Lude.Nothing,
      identityId = Lude.Nothing,
      developerUserIdentifierList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A pagination token. The first call you make will have @NextToken@ set to null. After that the service will return @NextToken@ values as needed. For example, let's say you make a request with @MaxResults@ set to 10, and there are 20 matches in the database. The service will return a pagination token as a part of the response. This token can be used to call the API again and get results starting from the 11th match.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldirsNextToken :: Lens.Lens' LookupDeveloperIdentityResponse (Lude.Maybe Lude.Text)
ldirsNextToken = Lens.lens (nextToken :: LookupDeveloperIdentityResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: LookupDeveloperIdentityResponse)
{-# DEPRECATED ldirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A unique identifier in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldirsIdentityId :: Lens.Lens' LookupDeveloperIdentityResponse (Lude.Maybe Lude.Text)
ldirsIdentityId = Lens.lens (identityId :: LookupDeveloperIdentityResponse -> Lude.Maybe Lude.Text) (\s a -> s {identityId = a} :: LookupDeveloperIdentityResponse)
{-# DEPRECATED ldirsIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | This is the list of developer user identifiers associated with an identity ID. Cognito supports the association of multiple developer user identifiers with an identity ID.
--
-- /Note:/ Consider using 'developerUserIdentifierList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldirsDeveloperUserIdentifierList :: Lens.Lens' LookupDeveloperIdentityResponse (Lude.Maybe [Lude.Text])
ldirsDeveloperUserIdentifierList = Lens.lens (developerUserIdentifierList :: LookupDeveloperIdentityResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {developerUserIdentifierList = a} :: LookupDeveloperIdentityResponse)
{-# DEPRECATED ldirsDeveloperUserIdentifierList "Use generic-lens or generic-optics with 'developerUserIdentifierList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldirsResponseStatus :: Lens.Lens' LookupDeveloperIdentityResponse Lude.Int
ldirsResponseStatus = Lens.lens (responseStatus :: LookupDeveloperIdentityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: LookupDeveloperIdentityResponse)
{-# DEPRECATED ldirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
