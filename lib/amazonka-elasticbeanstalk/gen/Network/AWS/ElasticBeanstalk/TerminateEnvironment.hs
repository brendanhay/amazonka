{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.TerminateEnvironment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates the specified environment.
module Network.AWS.ElasticBeanstalk.TerminateEnvironment
    (
    -- * Creating a request
      TerminateEnvironment (..)
    , mkTerminateEnvironment
    -- ** Request lenses
    , teEnvironmentId
    , teEnvironmentName
    , teForceTerminate
    , teTerminateResources

     -- * Destructuring the response
    , Types.EnvironmentDescription (..)
    , Types.mkEnvironmentDescription
    -- ** Response lenses
    , Types.eAbortableOperationInProgress
    , Types.eApplicationName
    , Types.eCNAME
    , Types.eDateCreated
    , Types.eDateUpdated
    , Types.eDescription
    , Types.eEndpointURL
    , Types.eEnvironmentArn
    , Types.eEnvironmentId
    , Types.eEnvironmentLinks
    , Types.eEnvironmentName
    , Types.eHealth
    , Types.eHealthStatus
    , Types.eOperationsRole
    , Types.ePlatformArn
    , Types.eResources
    , Types.eSolutionStackName
    , Types.eStatus
    , Types.eTemplateName
    , Types.eTier
    , Types.eVersionLabel
    ) where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to terminate an environment.
--
-- /See:/ 'mkTerminateEnvironment' smart constructor.
data TerminateEnvironment = TerminateEnvironment'
  { environmentId :: Core.Maybe Types.EnvironmentId
    -- ^ The ID of the environment to terminate.
--
-- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error. 
  , environmentName :: Core.Maybe Types.EnvironmentName
    -- ^ The name of the environment to terminate.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error. 
  , forceTerminate :: Core.Maybe Core.Bool
    -- ^ Terminates the target environment even if another environment in the same group is dependent on it.
  , terminateResources :: Core.Maybe Core.Bool
    -- ^ Indicates whether the associated AWS resources should shut down when the environment is terminated:
--
--
--     * @true@ : The specified environment as well as the associated AWS resources, such as Auto Scaling group and LoadBalancer, are terminated.
--
--
--     * @false@ : AWS Elastic Beanstalk resource management is removed from the environment, but the AWS resources continue to operate.
--
--
-- For more information, see the <https://docs.aws.amazon.com/elasticbeanstalk/latest/ug/ AWS Elastic Beanstalk User Guide. > 
-- Default: @true@ 
-- Valid Values: @true@ | @false@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TerminateEnvironment' value with any optional fields omitted.
mkTerminateEnvironment
    :: TerminateEnvironment
mkTerminateEnvironment
  = TerminateEnvironment'{environmentId = Core.Nothing,
                          environmentName = Core.Nothing, forceTerminate = Core.Nothing,
                          terminateResources = Core.Nothing}

-- | The ID of the environment to terminate.
--
-- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error. 
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
teEnvironmentId :: Lens.Lens' TerminateEnvironment (Core.Maybe Types.EnvironmentId)
teEnvironmentId = Lens.field @"environmentId"
{-# INLINEABLE teEnvironmentId #-}
{-# DEPRECATED environmentId "Use generic-lens or generic-optics with 'environmentId' instead"  #-}

-- | The name of the environment to terminate.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error. 
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
teEnvironmentName :: Lens.Lens' TerminateEnvironment (Core.Maybe Types.EnvironmentName)
teEnvironmentName = Lens.field @"environmentName"
{-# INLINEABLE teEnvironmentName #-}
{-# DEPRECATED environmentName "Use generic-lens or generic-optics with 'environmentName' instead"  #-}

-- | Terminates the target environment even if another environment in the same group is dependent on it.
--
-- /Note:/ Consider using 'forceTerminate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
teForceTerminate :: Lens.Lens' TerminateEnvironment (Core.Maybe Core.Bool)
teForceTerminate = Lens.field @"forceTerminate"
{-# INLINEABLE teForceTerminate #-}
{-# DEPRECATED forceTerminate "Use generic-lens or generic-optics with 'forceTerminate' instead"  #-}

-- | Indicates whether the associated AWS resources should shut down when the environment is terminated:
--
--
--     * @true@ : The specified environment as well as the associated AWS resources, such as Auto Scaling group and LoadBalancer, are terminated.
--
--
--     * @false@ : AWS Elastic Beanstalk resource management is removed from the environment, but the AWS resources continue to operate.
--
--
-- For more information, see the <https://docs.aws.amazon.com/elasticbeanstalk/latest/ug/ AWS Elastic Beanstalk User Guide. > 
-- Default: @true@ 
-- Valid Values: @true@ | @false@ 
--
-- /Note:/ Consider using 'terminateResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
teTerminateResources :: Lens.Lens' TerminateEnvironment (Core.Maybe Core.Bool)
teTerminateResources = Lens.field @"terminateResources"
{-# INLINEABLE teTerminateResources #-}
{-# DEPRECATED terminateResources "Use generic-lens or generic-optics with 'terminateResources' instead"  #-}

instance Core.ToQuery TerminateEnvironment where
        toQuery TerminateEnvironment{..}
          = Core.toQueryPair "Action" ("TerminateEnvironment" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EnvironmentId")
                environmentId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EnvironmentName")
                environmentName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ForceTerminate")
                forceTerminate
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TerminateResources")
                terminateResources

instance Core.ToHeaders TerminateEnvironment where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest TerminateEnvironment where
        type Rs TerminateEnvironment = Types.EnvironmentDescription
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
        parseResponse
          = Response.receiveXMLWrapper "TerminateEnvironmentResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}
