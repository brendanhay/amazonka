{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.DeleteSourceCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a set of GitHub, GitHub Enterprise, or Bitbucket source credentials. 
module Network.AWS.CodeBuild.DeleteSourceCredentials
    (
    -- * Creating a request
      DeleteSourceCredentials (..)
    , mkDeleteSourceCredentials
    -- ** Request lenses
    , dscArn

    -- * Destructuring the response
    , DeleteSourceCredentialsResponse (..)
    , mkDeleteSourceCredentialsResponse
    -- ** Response lenses
    , dscrrsArn
    , dscrrsResponseStatus
    ) where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteSourceCredentials' smart constructor.
newtype DeleteSourceCredentials = DeleteSourceCredentials'
  { arn :: Types.NonEmptyString
    -- ^ The Amazon Resource Name (ARN) of the token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSourceCredentials' value with any optional fields omitted.
mkDeleteSourceCredentials
    :: Types.NonEmptyString -- ^ 'arn'
    -> DeleteSourceCredentials
mkDeleteSourceCredentials arn = DeleteSourceCredentials'{arn}

-- | The Amazon Resource Name (ARN) of the token.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscArn :: Lens.Lens' DeleteSourceCredentials Types.NonEmptyString
dscArn = Lens.field @"arn"
{-# INLINEABLE dscArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.ToQuery DeleteSourceCredentials where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteSourceCredentials where
        toHeaders DeleteSourceCredentials{..}
          = Core.pure
              ("X-Amz-Target", "CodeBuild_20161006.DeleteSourceCredentials")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteSourceCredentials where
        toJSON DeleteSourceCredentials{..}
          = Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest DeleteSourceCredentials where
        type Rs DeleteSourceCredentials = DeleteSourceCredentialsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteSourceCredentialsResponse' Core.<$>
                   (x Core..:? "arn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteSourceCredentialsResponse' smart constructor.
data DeleteSourceCredentialsResponse = DeleteSourceCredentialsResponse'
  { arn :: Core.Maybe Types.NonEmptyString
    -- ^ The Amazon Resource Name (ARN) of the token. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSourceCredentialsResponse' value with any optional fields omitted.
mkDeleteSourceCredentialsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteSourceCredentialsResponse
mkDeleteSourceCredentialsResponse responseStatus
  = DeleteSourceCredentialsResponse'{arn = Core.Nothing,
                                     responseStatus}

-- | The Amazon Resource Name (ARN) of the token. 
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrrsArn :: Lens.Lens' DeleteSourceCredentialsResponse (Core.Maybe Types.NonEmptyString)
dscrrsArn = Lens.field @"arn"
{-# INLINEABLE dscrrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrrsResponseStatus :: Lens.Lens' DeleteSourceCredentialsResponse Core.Int
dscrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dscrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
