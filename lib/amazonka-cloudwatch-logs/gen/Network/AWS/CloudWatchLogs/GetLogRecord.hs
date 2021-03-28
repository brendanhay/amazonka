{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.GetLogRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all of the fields and values of a single log event. All fields are retrieved, even if the original query that produced the @logRecordPointer@ retrieved only a subset of fields. Fields are returned as field name/field value pairs.
--
-- The full unparsed log event is returned within @@message@ .
module Network.AWS.CloudWatchLogs.GetLogRecord
    (
    -- * Creating a request
      GetLogRecord (..)
    , mkGetLogRecord
    -- ** Request lenses
    , glrLogRecordPointer

    -- * Destructuring the response
    , GetLogRecordResponse (..)
    , mkGetLogRecordResponse
    -- ** Response lenses
    , glrrrsLogRecord
    , glrrrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetLogRecord' smart constructor.
newtype GetLogRecord = GetLogRecord'
  { logRecordPointer :: Types.LogRecordPointer
    -- ^ The pointer corresponding to the log event record you want to retrieve. You get this from the response of a @GetQueryResults@ operation. In that response, the value of the @@ptr@ field for a log event is the value to use as @logRecordPointer@ to retrieve that complete log event record.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetLogRecord' value with any optional fields omitted.
mkGetLogRecord
    :: Types.LogRecordPointer -- ^ 'logRecordPointer'
    -> GetLogRecord
mkGetLogRecord logRecordPointer = GetLogRecord'{logRecordPointer}

-- | The pointer corresponding to the log event record you want to retrieve. You get this from the response of a @GetQueryResults@ operation. In that response, the value of the @@ptr@ field for a log event is the value to use as @logRecordPointer@ to retrieve that complete log event record.
--
-- /Note:/ Consider using 'logRecordPointer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glrLogRecordPointer :: Lens.Lens' GetLogRecord Types.LogRecordPointer
glrLogRecordPointer = Lens.field @"logRecordPointer"
{-# INLINEABLE glrLogRecordPointer #-}
{-# DEPRECATED logRecordPointer "Use generic-lens or generic-optics with 'logRecordPointer' instead"  #-}

instance Core.ToQuery GetLogRecord where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetLogRecord where
        toHeaders GetLogRecord{..}
          = Core.pure ("X-Amz-Target", "Logs_20140328.GetLogRecord") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetLogRecord where
        toJSON GetLogRecord{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("logRecordPointer" Core..= logRecordPointer)])

instance Core.AWSRequest GetLogRecord where
        type Rs GetLogRecord = GetLogRecordResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetLogRecordResponse' Core.<$>
                   (x Core..:? "logRecord") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetLogRecordResponse' smart constructor.
data GetLogRecordResponse = GetLogRecordResponse'
  { logRecord :: Core.Maybe (Core.HashMap Types.Field Types.Value)
    -- ^ The requested log event, as a JSON string.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetLogRecordResponse' value with any optional fields omitted.
mkGetLogRecordResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetLogRecordResponse
mkGetLogRecordResponse responseStatus
  = GetLogRecordResponse'{logRecord = Core.Nothing, responseStatus}

-- | The requested log event, as a JSON string.
--
-- /Note:/ Consider using 'logRecord' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glrrrsLogRecord :: Lens.Lens' GetLogRecordResponse (Core.Maybe (Core.HashMap Types.Field Types.Value))
glrrrsLogRecord = Lens.field @"logRecord"
{-# INLINEABLE glrrrsLogRecord #-}
{-# DEPRECATED logRecord "Use generic-lens or generic-optics with 'logRecord' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glrrrsResponseStatus :: Lens.Lens' GetLogRecordResponse Core.Int
glrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE glrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
