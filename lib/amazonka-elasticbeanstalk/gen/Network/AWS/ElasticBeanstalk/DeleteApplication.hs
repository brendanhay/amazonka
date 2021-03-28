{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DeleteApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified application along with all associated versions and configurations. The application versions will not be deleted from your Amazon S3 bucket.
module Network.AWS.ElasticBeanstalk.DeleteApplication
    (
    -- * Creating a request
      DeleteApplication (..)
    , mkDeleteApplication
    -- ** Request lenses
    , daApplicationName
    , daTerminateEnvByForce

    -- * Destructuring the response
    , DeleteApplicationResponse (..)
    , mkDeleteApplicationResponse
    ) where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to delete an application.
--
-- /See:/ 'mkDeleteApplication' smart constructor.
data DeleteApplication = DeleteApplication'
  { applicationName :: Types.ApplicationName
    -- ^ The name of the application to delete.
  , terminateEnvByForce :: Core.Maybe Core.Bool
    -- ^ When set to true, running environments will be terminated before deleting the application.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApplication' value with any optional fields omitted.
mkDeleteApplication
    :: Types.ApplicationName -- ^ 'applicationName'
    -> DeleteApplication
mkDeleteApplication applicationName
  = DeleteApplication'{applicationName,
                       terminateEnvByForce = Core.Nothing}

-- | The name of the application to delete.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daApplicationName :: Lens.Lens' DeleteApplication Types.ApplicationName
daApplicationName = Lens.field @"applicationName"
{-# INLINEABLE daApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | When set to true, running environments will be terminated before deleting the application.
--
-- /Note:/ Consider using 'terminateEnvByForce' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daTerminateEnvByForce :: Lens.Lens' DeleteApplication (Core.Maybe Core.Bool)
daTerminateEnvByForce = Lens.field @"terminateEnvByForce"
{-# INLINEABLE daTerminateEnvByForce #-}
{-# DEPRECATED terminateEnvByForce "Use generic-lens or generic-optics with 'terminateEnvByForce' instead"  #-}

instance Core.ToQuery DeleteApplication where
        toQuery DeleteApplication{..}
          = Core.toQueryPair "Action" ("DeleteApplication" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ApplicationName" applicationName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TerminateEnvByForce")
                terminateEnvByForce

instance Core.ToHeaders DeleteApplication where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteApplication where
        type Rs DeleteApplication = DeleteApplicationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
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
