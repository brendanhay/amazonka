{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.RestartAppServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Causes the environment to restart the application container server running on each Amazon EC2 instance.
module Network.AWS.ElasticBeanstalk.RestartAppServer
    (
    -- * Creating a request
      RestartAppServer (..)
    , mkRestartAppServer
    -- ** Request lenses
    , rasEnvironmentId
    , rasEnvironmentName

    -- * Destructuring the response
    , RestartAppServerResponse (..)
    , mkRestartAppServerResponse
    ) where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkRestartAppServer' smart constructor.
data RestartAppServer = RestartAppServer'
  { environmentId :: Core.Maybe Types.EnvironmentId
    -- ^ The ID of the environment to restart the server for.
--
-- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error. 
  , environmentName :: Core.Maybe Types.EnvironmentName
    -- ^ The name of the environment to restart the server for.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RestartAppServer' value with any optional fields omitted.
mkRestartAppServer
    :: RestartAppServer
mkRestartAppServer
  = RestartAppServer'{environmentId = Core.Nothing,
                      environmentName = Core.Nothing}

-- | The ID of the environment to restart the server for.
--
-- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error. 
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasEnvironmentId :: Lens.Lens' RestartAppServer (Core.Maybe Types.EnvironmentId)
rasEnvironmentId = Lens.field @"environmentId"
{-# INLINEABLE rasEnvironmentId #-}
{-# DEPRECATED environmentId "Use generic-lens or generic-optics with 'environmentId' instead"  #-}

-- | The name of the environment to restart the server for.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error. 
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasEnvironmentName :: Lens.Lens' RestartAppServer (Core.Maybe Types.EnvironmentName)
rasEnvironmentName = Lens.field @"environmentName"
{-# INLINEABLE rasEnvironmentName #-}
{-# DEPRECATED environmentName "Use generic-lens or generic-optics with 'environmentName' instead"  #-}

instance Core.ToQuery RestartAppServer where
        toQuery RestartAppServer{..}
          = Core.toQueryPair "Action" ("RestartAppServer" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EnvironmentId")
                environmentId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EnvironmentName")
                environmentName

instance Core.ToHeaders RestartAppServer where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RestartAppServer where
        type Rs RestartAppServer = RestartAppServerResponse
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
        parseResponse = Response.receiveNull RestartAppServerResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRestartAppServerResponse' smart constructor.
data RestartAppServerResponse = RestartAppServerResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RestartAppServerResponse' value with any optional fields omitted.
mkRestartAppServerResponse
    :: RestartAppServerResponse
mkRestartAppServerResponse = RestartAppServerResponse'
