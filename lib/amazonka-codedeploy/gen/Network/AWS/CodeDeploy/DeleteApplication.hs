{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.DeleteApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an application.
module Network.AWS.CodeDeploy.DeleteApplication
    (
    -- * Creating a request
      DeleteApplication (..)
    , mkDeleteApplication
    -- ** Request lenses
    , daApplicationName

    -- * Destructuring the response
    , DeleteApplicationResponse (..)
    , mkDeleteApplicationResponse
    ) where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeleteApplication@ operation.
--
-- /See:/ 'mkDeleteApplication' smart constructor.
newtype DeleteApplication = DeleteApplication'
  { applicationName :: Types.ApplicationName
    -- ^ The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApplication' value with any optional fields omitted.
mkDeleteApplication
    :: Types.ApplicationName -- ^ 'applicationName'
    -> DeleteApplication
mkDeleteApplication applicationName
  = DeleteApplication'{applicationName}

-- | The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daApplicationName :: Lens.Lens' DeleteApplication Types.ApplicationName
daApplicationName = Lens.field @"applicationName"
{-# INLINEABLE daApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

instance Core.ToQuery DeleteApplication where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteApplication where
        toHeaders DeleteApplication{..}
          = Core.pure
              ("X-Amz-Target", "CodeDeploy_20141006.DeleteApplication")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteApplication where
        toJSON DeleteApplication{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("applicationName" Core..= applicationName)])

instance Core.AWSRequest DeleteApplication where
        type Rs DeleteApplication = DeleteApplicationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
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
