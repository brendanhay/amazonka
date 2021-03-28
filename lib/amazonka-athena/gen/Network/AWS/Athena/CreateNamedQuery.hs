{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.CreateNamedQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a named query in the specified workgroup. Requires that you have access to the workgroup.
--
-- For code samples using the AWS SDK for Java, see <http://docs.aws.amazon.com/athena/latest/ug/code-samples.html Examples and Code Samples> in the /Amazon Athena User Guide/ .
module Network.AWS.Athena.CreateNamedQuery
    (
    -- * Creating a request
      CreateNamedQuery (..)
    , mkCreateNamedQuery
    -- ** Request lenses
    , cnqName
    , cnqDatabase
    , cnqQueryString
    , cnqClientRequestToken
    , cnqDescription
    , cnqWorkGroup

    -- * Destructuring the response
    , CreateNamedQueryResponse (..)
    , mkCreateNamedQueryResponse
    -- ** Response lenses
    , cnqrrsNamedQueryId
    , cnqrrsResponseStatus
    ) where

import qualified Network.AWS.Athena.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateNamedQuery' smart constructor.
data CreateNamedQuery = CreateNamedQuery'
  { name :: Types.Name
    -- ^ The query name.
  , database :: Types.DatabaseString
    -- ^ The database to which the query belongs.
  , queryString :: Types.QueryString
    -- ^ The contents of the query with all query statements.
  , clientRequestToken :: Core.Maybe Types.IdempotencyToken
    -- ^ A unique case-sensitive string used to ensure the request to create the query is idempotent (executes only once). If another @CreateNamedQuery@ request is received, the same response is returned and another query is not created. If a parameter has changed, for example, the @QueryString@ , an error is returned.
--
-- /Important:/ This token is listed as not required because AWS SDKs (for example the AWS SDK for Java) auto-generate the token for users. If you are not using the AWS SDK or the AWS CLI, you must provide this token or the action will fail.
  , description :: Core.Maybe Types.Description
    -- ^ The query description.
  , workGroup :: Core.Maybe Types.WorkGroupName
    -- ^ The name of the workgroup in which the named query is being created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateNamedQuery' value with any optional fields omitted.
mkCreateNamedQuery
    :: Types.Name -- ^ 'name'
    -> Types.DatabaseString -- ^ 'database'
    -> Types.QueryString -- ^ 'queryString'
    -> CreateNamedQuery
mkCreateNamedQuery name database queryString
  = CreateNamedQuery'{name, database, queryString,
                      clientRequestToken = Core.Nothing, description = Core.Nothing,
                      workGroup = Core.Nothing}

-- | The query name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnqName :: Lens.Lens' CreateNamedQuery Types.Name
cnqName = Lens.field @"name"
{-# INLINEABLE cnqName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The database to which the query belongs.
--
-- /Note:/ Consider using 'database' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnqDatabase :: Lens.Lens' CreateNamedQuery Types.DatabaseString
cnqDatabase = Lens.field @"database"
{-# INLINEABLE cnqDatabase #-}
{-# DEPRECATED database "Use generic-lens or generic-optics with 'database' instead"  #-}

-- | The contents of the query with all query statements.
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnqQueryString :: Lens.Lens' CreateNamedQuery Types.QueryString
cnqQueryString = Lens.field @"queryString"
{-# INLINEABLE cnqQueryString #-}
{-# DEPRECATED queryString "Use generic-lens or generic-optics with 'queryString' instead"  #-}

-- | A unique case-sensitive string used to ensure the request to create the query is idempotent (executes only once). If another @CreateNamedQuery@ request is received, the same response is returned and another query is not created. If a parameter has changed, for example, the @QueryString@ , an error is returned.
--
-- /Important:/ This token is listed as not required because AWS SDKs (for example the AWS SDK for Java) auto-generate the token for users. If you are not using the AWS SDK or the AWS CLI, you must provide this token or the action will fail.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnqClientRequestToken :: Lens.Lens' CreateNamedQuery (Core.Maybe Types.IdempotencyToken)
cnqClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE cnqClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | The query description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnqDescription :: Lens.Lens' CreateNamedQuery (Core.Maybe Types.Description)
cnqDescription = Lens.field @"description"
{-# INLINEABLE cnqDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The name of the workgroup in which the named query is being created.
--
-- /Note:/ Consider using 'workGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnqWorkGroup :: Lens.Lens' CreateNamedQuery (Core.Maybe Types.WorkGroupName)
cnqWorkGroup = Lens.field @"workGroup"
{-# INLINEABLE cnqWorkGroup #-}
{-# DEPRECATED workGroup "Use generic-lens or generic-optics with 'workGroup' instead"  #-}

instance Core.ToQuery CreateNamedQuery where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateNamedQuery where
        toHeaders CreateNamedQuery{..}
          = Core.pure ("X-Amz-Target", "AmazonAthena.CreateNamedQuery")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateNamedQuery where
        toJSON CreateNamedQuery{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("Database" Core..= database),
                  Core.Just ("QueryString" Core..= queryString),
                  ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
                  ("Description" Core..=) Core.<$> description,
                  ("WorkGroup" Core..=) Core.<$> workGroup])

instance Core.AWSRequest CreateNamedQuery where
        type Rs CreateNamedQuery = CreateNamedQueryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateNamedQueryResponse' Core.<$>
                   (x Core..:? "NamedQueryId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateNamedQueryResponse' smart constructor.
data CreateNamedQueryResponse = CreateNamedQueryResponse'
  { namedQueryId :: Core.Maybe Types.NamedQueryId
    -- ^ The unique ID of the query.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateNamedQueryResponse' value with any optional fields omitted.
mkCreateNamedQueryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateNamedQueryResponse
mkCreateNamedQueryResponse responseStatus
  = CreateNamedQueryResponse'{namedQueryId = Core.Nothing,
                              responseStatus}

-- | The unique ID of the query.
--
-- /Note:/ Consider using 'namedQueryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnqrrsNamedQueryId :: Lens.Lens' CreateNamedQueryResponse (Core.Maybe Types.NamedQueryId)
cnqrrsNamedQueryId = Lens.field @"namedQueryId"
{-# INLINEABLE cnqrrsNamedQueryId #-}
{-# DEPRECATED namedQueryId "Use generic-lens or generic-optics with 'namedQueryId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnqrrsResponseStatus :: Lens.Lens' CreateNamedQueryResponse Core.Int
cnqrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cnqrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
