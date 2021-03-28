{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.PutApplicationPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the permission policy for an application. For the list of actions supported for this operation, see
--
--  <https://docs.aws.amazon.com/serverlessrepo/latest/devguide/access-control-resource-based.html#application-permissions Application 
--  Permissions> 
--  .
module Network.AWS.ServerlessApplicationRepository.PutApplicationPolicy
    (
    -- * Creating a request
      PutApplicationPolicy (..)
    , mkPutApplicationPolicy
    -- ** Request lenses
    , papApplicationId
    , papStatements

    -- * Destructuring the response
    , PutApplicationPolicyResponse (..)
    , mkPutApplicationPolicyResponse
    -- ** Response lenses
    , paprrsStatements
    , paprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServerlessApplicationRepository.Types as Types

-- | /See:/ 'mkPutApplicationPolicy' smart constructor.
data PutApplicationPolicy = PutApplicationPolicy'
  { applicationId :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the application.
  , statements :: [Types.ApplicationPolicyStatement]
    -- ^ An array of policy statements applied to the application.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutApplicationPolicy' value with any optional fields omitted.
mkPutApplicationPolicy
    :: Core.Text -- ^ 'applicationId'
    -> PutApplicationPolicy
mkPutApplicationPolicy applicationId
  = PutApplicationPolicy'{applicationId, statements = Core.mempty}

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papApplicationId :: Lens.Lens' PutApplicationPolicy Core.Text
papApplicationId = Lens.field @"applicationId"
{-# INLINEABLE papApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | An array of policy statements applied to the application.
--
-- /Note:/ Consider using 'statements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papStatements :: Lens.Lens' PutApplicationPolicy [Types.ApplicationPolicyStatement]
papStatements = Lens.field @"statements"
{-# INLINEABLE papStatements #-}
{-# DEPRECATED statements "Use generic-lens or generic-optics with 'statements' instead"  #-}

instance Core.ToQuery PutApplicationPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutApplicationPolicy where
        toHeaders PutApplicationPolicy{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutApplicationPolicy where
        toJSON PutApplicationPolicy{..}
          = Core.object
              (Core.catMaybes [Core.Just ("statements" Core..= statements)])

instance Core.AWSRequest PutApplicationPolicy where
        type Rs PutApplicationPolicy = PutApplicationPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/applications/" Core.<> Core.toText applicationId Core.<>
                             "/policy",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutApplicationPolicyResponse' Core.<$>
                   (x Core..:? "statements") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutApplicationPolicyResponse' smart constructor.
data PutApplicationPolicyResponse = PutApplicationPolicyResponse'
  { statements :: Core.Maybe [Types.ApplicationPolicyStatement]
    -- ^ An array of policy statements applied to the application.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutApplicationPolicyResponse' value with any optional fields omitted.
mkPutApplicationPolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutApplicationPolicyResponse
mkPutApplicationPolicyResponse responseStatus
  = PutApplicationPolicyResponse'{statements = Core.Nothing,
                                  responseStatus}

-- | An array of policy statements applied to the application.
--
-- /Note:/ Consider using 'statements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paprrsStatements :: Lens.Lens' PutApplicationPolicyResponse (Core.Maybe [Types.ApplicationPolicyStatement])
paprrsStatements = Lens.field @"statements"
{-# INLINEABLE paprrsStatements #-}
{-# DEPRECATED statements "Use generic-lens or generic-optics with 'statements' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paprrsResponseStatus :: Lens.Lens' PutApplicationPolicyResponse Core.Int
paprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE paprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
