{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.DeleteApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified application.
module Network.AWS.ServerlessApplicationRepository.DeleteApplication
    (
    -- * Creating a request
      DeleteApplication (..)
    , mkDeleteApplication
    -- ** Request lenses
    , daApplicationId

    -- * Destructuring the response
    , DeleteApplicationResponse (..)
    , mkDeleteApplicationResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServerlessApplicationRepository.Types as Types

-- | /See:/ 'mkDeleteApplication' smart constructor.
newtype DeleteApplication = DeleteApplication'
  { applicationId :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the application.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApplication' value with any optional fields omitted.
mkDeleteApplication
    :: Core.Text -- ^ 'applicationId'
    -> DeleteApplication
mkDeleteApplication applicationId
  = DeleteApplication'{applicationId}

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daApplicationId :: Lens.Lens' DeleteApplication Core.Text
daApplicationId = Lens.field @"applicationId"
{-# INLINEABLE daApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

instance Core.ToQuery DeleteApplication where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteApplication where
        toHeaders DeleteApplication{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteApplication where
        type Rs DeleteApplication = DeleteApplicationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/applications/" Core.<> Core.toText applicationId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteApplicationResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteApplicationResponse' smart constructor.
data DeleteApplicationResponse = DeleteApplicationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApplicationResponse' value with any optional fields omitted.
mkDeleteApplicationResponse
    :: DeleteApplicationResponse
mkDeleteApplicationResponse = DeleteApplicationResponse'
