{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.RebuildEnvironment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes and recreates all of the AWS resources (for example: the Auto Scaling group, load balancer, etc.) for a specified environment and forces a restart.
module Network.AWS.ElasticBeanstalk.RebuildEnvironment
    (
    -- * Creating a request
      RebuildEnvironment (..)
    , mkRebuildEnvironment
    -- ** Request lenses
    , reEnvironmentId
    , reEnvironmentName

    -- * Destructuring the response
    , RebuildEnvironmentResponse (..)
    , mkRebuildEnvironmentResponse
    ) where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkRebuildEnvironment' smart constructor.
data RebuildEnvironment = RebuildEnvironment'
  { environmentId :: Core.Maybe Types.EnvironmentId
    -- ^ The ID of the environment to rebuild.
--
-- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error. 
  , environmentName :: Core.Maybe Types.EnvironmentName
    -- ^ The name of the environment to rebuild.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RebuildEnvironment' value with any optional fields omitted.
mkRebuildEnvironment
    :: RebuildEnvironment
mkRebuildEnvironment
  = RebuildEnvironment'{environmentId = Core.Nothing,
                        environmentName = Core.Nothing}

-- | The ID of the environment to rebuild.
--
-- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error. 
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reEnvironmentId :: Lens.Lens' RebuildEnvironment (Core.Maybe Types.EnvironmentId)
reEnvironmentId = Lens.field @"environmentId"
{-# INLINEABLE reEnvironmentId #-}
{-# DEPRECATED environmentId "Use generic-lens or generic-optics with 'environmentId' instead"  #-}

-- | The name of the environment to rebuild.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error. 
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reEnvironmentName :: Lens.Lens' RebuildEnvironment (Core.Maybe Types.EnvironmentName)
reEnvironmentName = Lens.field @"environmentName"
{-# INLINEABLE reEnvironmentName #-}
{-# DEPRECATED environmentName "Use generic-lens or generic-optics with 'environmentName' instead"  #-}

instance Core.ToQuery RebuildEnvironment where
        toQuery RebuildEnvironment{..}
          = Core.toQueryPair "Action" ("RebuildEnvironment" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EnvironmentId")
                environmentId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EnvironmentName")
                environmentName

instance Core.ToHeaders RebuildEnvironment where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RebuildEnvironment where
        type Rs RebuildEnvironment = RebuildEnvironmentResponse
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
        parseResponse = Response.receiveNull RebuildEnvironmentResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRebuildEnvironmentResponse' smart constructor.
data RebuildEnvironmentResponse = RebuildEnvironmentResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RebuildEnvironmentResponse' value with any optional fields omitted.
mkRebuildEnvironmentResponse
    :: RebuildEnvironmentResponse
mkRebuildEnvironmentResponse = RebuildEnvironmentResponse'
