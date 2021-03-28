{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.RequestEnvironmentInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a request to compile the specified type of information of the deployed environment.
--
-- Setting the @InfoType@ to @tail@ compiles the last lines from the application server log files of every Amazon EC2 instance in your environment. 
-- Setting the @InfoType@ to @bundle@ compresses the application server log files for every Amazon EC2 instance into a @.zip@ file. Legacy and .NET containers do not support bundle logs. 
-- Use 'RetrieveEnvironmentInfo' to obtain the set of logs. 
-- Related Topics
--
--     * 'RetrieveEnvironmentInfo' 
--
--
module Network.AWS.ElasticBeanstalk.RequestEnvironmentInfo
    (
    -- * Creating a request
      RequestEnvironmentInfo (..)
    , mkRequestEnvironmentInfo
    -- ** Request lenses
    , reiInfoType
    , reiEnvironmentId
    , reiEnvironmentName

    -- * Destructuring the response
    , RequestEnvironmentInfoResponse (..)
    , mkRequestEnvironmentInfoResponse
    ) where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to retrieve logs from an environment and store them in your Elastic Beanstalk storage bucket.
--
-- /See:/ 'mkRequestEnvironmentInfo' smart constructor.
data RequestEnvironmentInfo = RequestEnvironmentInfo'
  { infoType :: Types.EnvironmentInfoType
    -- ^ The type of information to request.
  , environmentId :: Core.Maybe Types.EnvironmentId
    -- ^ The ID of the environment of the requested data.
--
-- If no such environment is found, @RequestEnvironmentInfo@ returns an @InvalidParameterValue@ error. 
-- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error. 
  , environmentName :: Core.Maybe Types.EnvironmentName
    -- ^ The name of the environment of the requested data.
--
-- If no such environment is found, @RequestEnvironmentInfo@ returns an @InvalidParameterValue@ error. 
-- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RequestEnvironmentInfo' value with any optional fields omitted.
mkRequestEnvironmentInfo
    :: Types.EnvironmentInfoType -- ^ 'infoType'
    -> RequestEnvironmentInfo
mkRequestEnvironmentInfo infoType
  = RequestEnvironmentInfo'{infoType, environmentId = Core.Nothing,
                            environmentName = Core.Nothing}

-- | The type of information to request.
--
-- /Note:/ Consider using 'infoType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiInfoType :: Lens.Lens' RequestEnvironmentInfo Types.EnvironmentInfoType
reiInfoType = Lens.field @"infoType"
{-# INLINEABLE reiInfoType #-}
{-# DEPRECATED infoType "Use generic-lens or generic-optics with 'infoType' instead"  #-}

-- | The ID of the environment of the requested data.
--
-- If no such environment is found, @RequestEnvironmentInfo@ returns an @InvalidParameterValue@ error. 
-- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error. 
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiEnvironmentId :: Lens.Lens' RequestEnvironmentInfo (Core.Maybe Types.EnvironmentId)
reiEnvironmentId = Lens.field @"environmentId"
{-# INLINEABLE reiEnvironmentId #-}
{-# DEPRECATED environmentId "Use generic-lens or generic-optics with 'environmentId' instead"  #-}

-- | The name of the environment of the requested data.
--
-- If no such environment is found, @RequestEnvironmentInfo@ returns an @InvalidParameterValue@ error. 
-- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error. 
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiEnvironmentName :: Lens.Lens' RequestEnvironmentInfo (Core.Maybe Types.EnvironmentName)
reiEnvironmentName = Lens.field @"environmentName"
{-# INLINEABLE reiEnvironmentName #-}
{-# DEPRECATED environmentName "Use generic-lens or generic-optics with 'environmentName' instead"  #-}

instance Core.ToQuery RequestEnvironmentInfo where
        toQuery RequestEnvironmentInfo{..}
          = Core.toQueryPair "Action" ("RequestEnvironmentInfo" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "InfoType" infoType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EnvironmentId")
                environmentId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EnvironmentName")
                environmentName

instance Core.ToHeaders RequestEnvironmentInfo where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RequestEnvironmentInfo where
        type Rs RequestEnvironmentInfo = RequestEnvironmentInfoResponse
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
          = Response.receiveNull RequestEnvironmentInfoResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRequestEnvironmentInfoResponse' smart constructor.
data RequestEnvironmentInfoResponse = RequestEnvironmentInfoResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RequestEnvironmentInfoResponse' value with any optional fields omitted.
mkRequestEnvironmentInfoResponse
    :: RequestEnvironmentInfoResponse
mkRequestEnvironmentInfoResponse = RequestEnvironmentInfoResponse'
