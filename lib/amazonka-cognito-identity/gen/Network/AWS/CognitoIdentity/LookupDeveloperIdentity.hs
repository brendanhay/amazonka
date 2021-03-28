{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      LookupDeveloperIdentity (..)
    , mkLookupDeveloperIdentity
    -- ** Request lenses
    , ldiIdentityPoolId
    , ldiDeveloperUserIdentifier
    , ldiIdentityId
    , ldiMaxResults
    , ldiNextToken

    -- * Destructuring the response
    , LookupDeveloperIdentityResponse (..)
    , mkLookupDeveloperIdentityResponse
    -- ** Response lenses
    , ldirrsDeveloperUserIdentifierList
    , ldirrsIdentityId
    , ldirrsNextToken
    , ldirrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentity.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the @LookupDeveloperIdentityInput@ action.
--
-- /See:/ 'mkLookupDeveloperIdentity' smart constructor.
data LookupDeveloperIdentity = LookupDeveloperIdentity'
  { identityPoolId :: Types.IdentityPoolId
    -- ^ An identity pool ID in the format REGION:GUID.
  , developerUserIdentifier :: Core.Maybe Types.DeveloperUserIdentifier
    -- ^ A unique ID used by your backend authentication process to identify a user. Typically, a developer identity provider would issue many developer user identifiers, in keeping with the number of users.
  , identityId :: Core.Maybe Types.IdentityId
    -- ^ A unique identifier in the format REGION:GUID.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of identities to return.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A pagination token. The first call you make will have @NextToken@ set to null. After that the service will return @NextToken@ values as needed. For example, let's say you make a request with @MaxResults@ set to 10, and there are 20 matches in the database. The service will return a pagination token as a part of the response. This token can be used to call the API again and get results starting from the 11th match.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LookupDeveloperIdentity' value with any optional fields omitted.
mkLookupDeveloperIdentity
    :: Types.IdentityPoolId -- ^ 'identityPoolId'
    -> LookupDeveloperIdentity
mkLookupDeveloperIdentity identityPoolId
  = LookupDeveloperIdentity'{identityPoolId,
                             developerUserIdentifier = Core.Nothing, identityId = Core.Nothing,
                             maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | An identity pool ID in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldiIdentityPoolId :: Lens.Lens' LookupDeveloperIdentity Types.IdentityPoolId
ldiIdentityPoolId = Lens.field @"identityPoolId"
{-# INLINEABLE ldiIdentityPoolId #-}
{-# DEPRECATED identityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead"  #-}

-- | A unique ID used by your backend authentication process to identify a user. Typically, a developer identity provider would issue many developer user identifiers, in keeping with the number of users.
--
-- /Note:/ Consider using 'developerUserIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldiDeveloperUserIdentifier :: Lens.Lens' LookupDeveloperIdentity (Core.Maybe Types.DeveloperUserIdentifier)
ldiDeveloperUserIdentifier = Lens.field @"developerUserIdentifier"
{-# INLINEABLE ldiDeveloperUserIdentifier #-}
{-# DEPRECATED developerUserIdentifier "Use generic-lens or generic-optics with 'developerUserIdentifier' instead"  #-}

-- | A unique identifier in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldiIdentityId :: Lens.Lens' LookupDeveloperIdentity (Core.Maybe Types.IdentityId)
ldiIdentityId = Lens.field @"identityId"
{-# INLINEABLE ldiIdentityId #-}
{-# DEPRECATED identityId "Use generic-lens or generic-optics with 'identityId' instead"  #-}

-- | The maximum number of identities to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldiMaxResults :: Lens.Lens' LookupDeveloperIdentity (Core.Maybe Core.Natural)
ldiMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ldiMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A pagination token. The first call you make will have @NextToken@ set to null. After that the service will return @NextToken@ values as needed. For example, let's say you make a request with @MaxResults@ set to 10, and there are 20 matches in the database. The service will return a pagination token as a part of the response. This token can be used to call the API again and get results starting from the 11th match.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldiNextToken :: Lens.Lens' LookupDeveloperIdentity (Core.Maybe Types.NextToken)
ldiNextToken = Lens.field @"nextToken"
{-# INLINEABLE ldiNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery LookupDeveloperIdentity where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders LookupDeveloperIdentity where
        toHeaders LookupDeveloperIdentity{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityService.LookupDeveloperIdentity")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON LookupDeveloperIdentity where
        toJSON LookupDeveloperIdentity{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("IdentityPoolId" Core..= identityPoolId),
                  ("DeveloperUserIdentifier" Core..=) Core.<$>
                    developerUserIdentifier,
                  ("IdentityId" Core..=) Core.<$> identityId,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest LookupDeveloperIdentity where
        type Rs LookupDeveloperIdentity = LookupDeveloperIdentityResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 LookupDeveloperIdentityResponse' Core.<$>
                   (x Core..:? "DeveloperUserIdentifierList") Core.<*>
                     x Core..:? "IdentityId"
                     Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Returned in response to a successful @LookupDeveloperIdentity@ action.
--
-- /See:/ 'mkLookupDeveloperIdentityResponse' smart constructor.
data LookupDeveloperIdentityResponse = LookupDeveloperIdentityResponse'
  { developerUserIdentifierList :: Core.Maybe [Types.DeveloperUserIdentifier]
    -- ^ This is the list of developer user identifiers associated with an identity ID. Cognito supports the association of multiple developer user identifiers with an identity ID.
  , identityId :: Core.Maybe Types.IdentityId
    -- ^ A unique identifier in the format REGION:GUID.
  , nextToken :: Core.Maybe Types.PaginationKey
    -- ^ A pagination token. The first call you make will have @NextToken@ set to null. After that the service will return @NextToken@ values as needed. For example, let's say you make a request with @MaxResults@ set to 10, and there are 20 matches in the database. The service will return a pagination token as a part of the response. This token can be used to call the API again and get results starting from the 11th match.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LookupDeveloperIdentityResponse' value with any optional fields omitted.
mkLookupDeveloperIdentityResponse
    :: Core.Int -- ^ 'responseStatus'
    -> LookupDeveloperIdentityResponse
mkLookupDeveloperIdentityResponse responseStatus
  = LookupDeveloperIdentityResponse'{developerUserIdentifierList =
                                       Core.Nothing,
                                     identityId = Core.Nothing, nextToken = Core.Nothing,
                                     responseStatus}

-- | This is the list of developer user identifiers associated with an identity ID. Cognito supports the association of multiple developer user identifiers with an identity ID.
--
-- /Note:/ Consider using 'developerUserIdentifierList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldirrsDeveloperUserIdentifierList :: Lens.Lens' LookupDeveloperIdentityResponse (Core.Maybe [Types.DeveloperUserIdentifier])
ldirrsDeveloperUserIdentifierList = Lens.field @"developerUserIdentifierList"
{-# INLINEABLE ldirrsDeveloperUserIdentifierList #-}
{-# DEPRECATED developerUserIdentifierList "Use generic-lens or generic-optics with 'developerUserIdentifierList' instead"  #-}

-- | A unique identifier in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldirrsIdentityId :: Lens.Lens' LookupDeveloperIdentityResponse (Core.Maybe Types.IdentityId)
ldirrsIdentityId = Lens.field @"identityId"
{-# INLINEABLE ldirrsIdentityId #-}
{-# DEPRECATED identityId "Use generic-lens or generic-optics with 'identityId' instead"  #-}

-- | A pagination token. The first call you make will have @NextToken@ set to null. After that the service will return @NextToken@ values as needed. For example, let's say you make a request with @MaxResults@ set to 10, and there are 20 matches in the database. The service will return a pagination token as a part of the response. This token can be used to call the API again and get results starting from the 11th match.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldirrsNextToken :: Lens.Lens' LookupDeveloperIdentityResponse (Core.Maybe Types.PaginationKey)
ldirrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ldirrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldirrsResponseStatus :: Lens.Lens' LookupDeveloperIdentityResponse Core.Int
ldirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ldirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
