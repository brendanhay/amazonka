{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.RetrieveEnvironmentInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the compiled information from a 'RequestEnvironmentInfo' request.
--
-- Related Topics
--
--     * 'RequestEnvironmentInfo' 
--
--
module Network.AWS.ElasticBeanstalk.RetrieveEnvironmentInfo
    (
    -- * Creating a request
      RetrieveEnvironmentInfo (..)
    , mkRetrieveEnvironmentInfo
    -- ** Request lenses
    , rInfoType
    , rEnvironmentId
    , rEnvironmentName

    -- * Destructuring the response
    , RetrieveEnvironmentInfoResponse (..)
    , mkRetrieveEnvironmentInfoResponse
    -- ** Response lenses
    , reirrsEnvironmentInfo
    , reirrsResponseStatus
    ) where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to download logs retrieved with 'RequestEnvironmentInfo' .
--
-- /See:/ 'mkRetrieveEnvironmentInfo' smart constructor.
data RetrieveEnvironmentInfo = RetrieveEnvironmentInfo'
  { infoType :: Types.EnvironmentInfoType
    -- ^ The type of information to retrieve.
  , environmentId :: Core.Maybe Types.EnvironmentId
    -- ^ The ID of the data's environment.
--
-- If no such environment is found, returns an @InvalidParameterValue@ error.
-- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
  , environmentName :: Core.Maybe Types.EnvironmentName
    -- ^ The name of the data's environment.
--
-- If no such environment is found, returns an @InvalidParameterValue@ error. 
-- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RetrieveEnvironmentInfo' value with any optional fields omitted.
mkRetrieveEnvironmentInfo
    :: Types.EnvironmentInfoType -- ^ 'infoType'
    -> RetrieveEnvironmentInfo
mkRetrieveEnvironmentInfo infoType
  = RetrieveEnvironmentInfo'{infoType, environmentId = Core.Nothing,
                             environmentName = Core.Nothing}

-- | The type of information to retrieve.
--
-- /Note:/ Consider using 'infoType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rInfoType :: Lens.Lens' RetrieveEnvironmentInfo Types.EnvironmentInfoType
rInfoType = Lens.field @"infoType"
{-# INLINEABLE rInfoType #-}
{-# DEPRECATED infoType "Use generic-lens or generic-optics with 'infoType' instead"  #-}

-- | The ID of the data's environment.
--
-- If no such environment is found, returns an @InvalidParameterValue@ error.
-- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEnvironmentId :: Lens.Lens' RetrieveEnvironmentInfo (Core.Maybe Types.EnvironmentId)
rEnvironmentId = Lens.field @"environmentId"
{-# INLINEABLE rEnvironmentId #-}
{-# DEPRECATED environmentId "Use generic-lens or generic-optics with 'environmentId' instead"  #-}

-- | The name of the data's environment.
--
-- If no such environment is found, returns an @InvalidParameterValue@ error. 
-- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error. 
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEnvironmentName :: Lens.Lens' RetrieveEnvironmentInfo (Core.Maybe Types.EnvironmentName)
rEnvironmentName = Lens.field @"environmentName"
{-# INLINEABLE rEnvironmentName #-}
{-# DEPRECATED environmentName "Use generic-lens or generic-optics with 'environmentName' instead"  #-}

instance Core.ToQuery RetrieveEnvironmentInfo where
        toQuery RetrieveEnvironmentInfo{..}
          = Core.toQueryPair "Action"
              ("RetrieveEnvironmentInfo" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "InfoType" infoType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EnvironmentId")
                environmentId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EnvironmentName")
                environmentName

instance Core.ToHeaders RetrieveEnvironmentInfo where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RetrieveEnvironmentInfo where
        type Rs RetrieveEnvironmentInfo = RetrieveEnvironmentInfoResponse
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
          = Response.receiveXMLWrapper "RetrieveEnvironmentInfoResult"
              (\ s h x ->
                 RetrieveEnvironmentInfoResponse' Core.<$>
                   (x Core..@? "EnvironmentInfo" Core..<@> Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Result message containing a description of the requested environment info.
--
-- /See:/ 'mkRetrieveEnvironmentInfoResponse' smart constructor.
data RetrieveEnvironmentInfoResponse = RetrieveEnvironmentInfoResponse'
  { environmentInfo :: Core.Maybe [Types.EnvironmentInfoDescription]
    -- ^ The 'EnvironmentInfoDescription' of the environment. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RetrieveEnvironmentInfoResponse' value with any optional fields omitted.
mkRetrieveEnvironmentInfoResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RetrieveEnvironmentInfoResponse
mkRetrieveEnvironmentInfoResponse responseStatus
  = RetrieveEnvironmentInfoResponse'{environmentInfo = Core.Nothing,
                                     responseStatus}

-- | The 'EnvironmentInfoDescription' of the environment. 
--
-- /Note:/ Consider using 'environmentInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reirrsEnvironmentInfo :: Lens.Lens' RetrieveEnvironmentInfoResponse (Core.Maybe [Types.EnvironmentInfoDescription])
reirrsEnvironmentInfo = Lens.field @"environmentInfo"
{-# INLINEABLE reirrsEnvironmentInfo #-}
{-# DEPRECATED environmentInfo "Use generic-lens or generic-optics with 'environmentInfo' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reirrsResponseStatus :: Lens.Lens' RetrieveEnvironmentInfoResponse Core.Int
reirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE reirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
