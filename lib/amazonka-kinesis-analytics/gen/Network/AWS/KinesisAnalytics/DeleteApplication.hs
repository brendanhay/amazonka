{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.DeleteApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified application. Amazon Kinesis Analytics halts application execution and deletes the application, including any application artifacts (such as in-application streams, reference table, and application code).
--
-- This operation requires permissions to perform the @kinesisanalytics:DeleteApplication@ action.
module Network.AWS.KinesisAnalytics.DeleteApplication
    (
    -- * Creating a request
      DeleteApplication (..)
    , mkDeleteApplication
    -- ** Request lenses
    , dApplicationName
    , dCreateTimestamp

    -- * Destructuring the response
    , DeleteApplicationResponse (..)
    , mkDeleteApplicationResponse
    -- ** Response lenses
    , drsResponseStatus
    ) where

import qualified Network.AWS.KinesisAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDeleteApplication' smart constructor.
data DeleteApplication = DeleteApplication'
  { applicationName :: Types.ApplicationName
    -- ^ Name of the Amazon Kinesis Analytics application to delete.
  , createTimestamp :: Core.NominalDiffTime
    -- ^ You can use the @DescribeApplication@ operation to get this value. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteApplication' value with any optional fields omitted.
mkDeleteApplication
    :: Types.ApplicationName -- ^ 'applicationName'
    -> Core.NominalDiffTime -- ^ 'createTimestamp'
    -> DeleteApplication
mkDeleteApplication applicationName createTimestamp
  = DeleteApplication'{applicationName, createTimestamp}

-- | Name of the Amazon Kinesis Analytics application to delete.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dApplicationName :: Lens.Lens' DeleteApplication Types.ApplicationName
dApplicationName = Lens.field @"applicationName"
{-# INLINEABLE dApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | You can use the @DescribeApplication@ operation to get this value. 
--
-- /Note:/ Consider using 'createTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCreateTimestamp :: Lens.Lens' DeleteApplication Core.NominalDiffTime
dCreateTimestamp = Lens.field @"createTimestamp"
{-# INLINEABLE dCreateTimestamp #-}
{-# DEPRECATED createTimestamp "Use generic-lens or generic-optics with 'createTimestamp' instead"  #-}

instance Core.ToQuery DeleteApplication where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteApplication where
        toHeaders DeleteApplication{..}
          = Core.pure
              ("X-Amz-Target", "KinesisAnalytics_20150814.DeleteApplication")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteApplication where
        toJSON DeleteApplication{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ApplicationName" Core..= applicationName),
                  Core.Just ("CreateTimestamp" Core..= createTimestamp)])

instance Core.AWSRequest DeleteApplication where
        type Rs DeleteApplication = DeleteApplicationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteApplicationResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | 
--
-- /See:/ 'mkDeleteApplicationResponse' smart constructor.
newtype DeleteApplicationResponse = DeleteApplicationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApplicationResponse' value with any optional fields omitted.
mkDeleteApplicationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteApplicationResponse
mkDeleteApplicationResponse responseStatus
  = DeleteApplicationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteApplicationResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
