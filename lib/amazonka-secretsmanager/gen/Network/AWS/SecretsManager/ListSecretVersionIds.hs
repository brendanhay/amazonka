{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.ListSecretVersionIds
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the versions attached to the specified secret. The output does not include the @SecretString@ or @SecretBinary@ fields. By default, the list includes only versions that have at least one staging label in @VersionStage@ attached.
--
-- __Minimum permissions__ 
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:ListSecretVersionIds
--
--
-- __Related operations__ 
--
--     * To list the secrets in an account, use 'ListSecrets' .
--
--
--
-- This operation returns paginated results.
module Network.AWS.SecretsManager.ListSecretVersionIds
    (
    -- * Creating a request
      ListSecretVersionIds (..)
    , mkListSecretVersionIds
    -- ** Request lenses
    , lsviSecretId
    , lsviIncludeDeprecated
    , lsviMaxResults
    , lsviNextToken

    -- * Destructuring the response
    , ListSecretVersionIdsResponse (..)
    , mkListSecretVersionIdsResponse
    -- ** Response lenses
    , lsvirrsARN
    , lsvirrsName
    , lsvirrsNextToken
    , lsvirrsVersions
    , lsvirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SecretsManager.Types as Types

-- | /See:/ 'mkListSecretVersionIds' smart constructor.
data ListSecretVersionIds = ListSecretVersionIds'
  { secretId :: Types.SecretId
    -- ^ The identifier for the secret containing the versions you want to list. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
  , includeDeprecated :: Core.Maybe Core.Bool
    -- ^ (Optional) Specifies that you want the results to include versions that do not have any staging labels attached to them. Such versions are considered deprecated and are subject to deletion by Secrets Manager as needed.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ (Optional) Limits the number of results you want to include in the response. If you don't include this parameter, it defaults to a value that's specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (isn't null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Secrets Manager might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
  , nextToken :: Core.Maybe Types.NextTokenType
    -- ^ (Optional) Use this parameter in a request if you receive a @NextToken@ response in a previous request indicating there's more output available. In a subsequent call, set it to the value of the previous call @NextToken@ response to indicate where the output should continue from.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSecretVersionIds' value with any optional fields omitted.
mkListSecretVersionIds
    :: Types.SecretId -- ^ 'secretId'
    -> ListSecretVersionIds
mkListSecretVersionIds secretId
  = ListSecretVersionIds'{secretId, includeDeprecated = Core.Nothing,
                          maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The identifier for the secret containing the versions you want to list. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- /Note:/ Consider using 'secretId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsviSecretId :: Lens.Lens' ListSecretVersionIds Types.SecretId
lsviSecretId = Lens.field @"secretId"
{-# INLINEABLE lsviSecretId #-}
{-# DEPRECATED secretId "Use generic-lens or generic-optics with 'secretId' instead"  #-}

-- | (Optional) Specifies that you want the results to include versions that do not have any staging labels attached to them. Such versions are considered deprecated and are subject to deletion by Secrets Manager as needed.
--
-- /Note:/ Consider using 'includeDeprecated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsviIncludeDeprecated :: Lens.Lens' ListSecretVersionIds (Core.Maybe Core.Bool)
lsviIncludeDeprecated = Lens.field @"includeDeprecated"
{-# INLINEABLE lsviIncludeDeprecated #-}
{-# DEPRECATED includeDeprecated "Use generic-lens or generic-optics with 'includeDeprecated' instead"  #-}

-- | (Optional) Limits the number of results you want to include in the response. If you don't include this parameter, it defaults to a value that's specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (isn't null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Secrets Manager might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsviMaxResults :: Lens.Lens' ListSecretVersionIds (Core.Maybe Core.Natural)
lsviMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lsviMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | (Optional) Use this parameter in a request if you receive a @NextToken@ response in a previous request indicating there's more output available. In a subsequent call, set it to the value of the previous call @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsviNextToken :: Lens.Lens' ListSecretVersionIds (Core.Maybe Types.NextTokenType)
lsviNextToken = Lens.field @"nextToken"
{-# INLINEABLE lsviNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListSecretVersionIds where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListSecretVersionIds where
        toHeaders ListSecretVersionIds{..}
          = Core.pure ("X-Amz-Target", "secretsmanager.ListSecretVersionIds")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListSecretVersionIds where
        toJSON ListSecretVersionIds{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SecretId" Core..= secretId),
                  ("IncludeDeprecated" Core..=) Core.<$> includeDeprecated,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListSecretVersionIds where
        type Rs ListSecretVersionIds = ListSecretVersionIdsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListSecretVersionIdsResponse' Core.<$>
                   (x Core..:? "ARN") Core.<*> x Core..:? "Name" Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> x Core..:? "Versions"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListSecretVersionIds where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"versions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListSecretVersionIdsResponse' smart constructor.
data ListSecretVersionIdsResponse = ListSecretVersionIdsResponse'
  { arn :: Core.Maybe Types.SecretARNType
    -- ^ The Amazon Resource Name (ARN) for the secret.
  , name :: Core.Maybe Types.Name
    -- ^ The friendly name of the secret.
  , nextToken :: Core.Maybe Types.NextTokenType
    -- ^ If present in the response, this value indicates that there's more output available than included in the current response. This can occur even when the response includes no values at all, such as when you ask for a filtered view of a very long list. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to continue processing and get the next part of the output. You should repeat this until the @NextToken@ response element comes back empty (as @null@ ).
  , versions :: Core.Maybe [Types.SecretVersionsListEntry]
    -- ^ The list of the currently available versions of the specified secret.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListSecretVersionIdsResponse' value with any optional fields omitted.
mkListSecretVersionIdsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListSecretVersionIdsResponse
mkListSecretVersionIdsResponse responseStatus
  = ListSecretVersionIdsResponse'{arn = Core.Nothing,
                                  name = Core.Nothing, nextToken = Core.Nothing,
                                  versions = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) for the secret.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsvirrsARN :: Lens.Lens' ListSecretVersionIdsResponse (Core.Maybe Types.SecretARNType)
lsvirrsARN = Lens.field @"arn"
{-# INLINEABLE lsvirrsARN #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The friendly name of the secret.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsvirrsName :: Lens.Lens' ListSecretVersionIdsResponse (Core.Maybe Types.Name)
lsvirrsName = Lens.field @"name"
{-# INLINEABLE lsvirrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | If present in the response, this value indicates that there's more output available than included in the current response. This can occur even when the response includes no values at all, such as when you ask for a filtered view of a very long list. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to continue processing and get the next part of the output. You should repeat this until the @NextToken@ response element comes back empty (as @null@ ).
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsvirrsNextToken :: Lens.Lens' ListSecretVersionIdsResponse (Core.Maybe Types.NextTokenType)
lsvirrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lsvirrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The list of the currently available versions of the specified secret.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsvirrsVersions :: Lens.Lens' ListSecretVersionIdsResponse (Core.Maybe [Types.SecretVersionsListEntry])
lsvirrsVersions = Lens.field @"versions"
{-# INLINEABLE lsvirrsVersions #-}
{-# DEPRECATED versions "Use generic-lens or generic-optics with 'versions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsvirrsResponseStatus :: Lens.Lens' ListSecretVersionIdsResponse Core.Int
lsvirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lsvirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
