{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DeleteRetentionPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified retention policy.
--
-- Log events do not expire if they belong to log groups without a retention policy.
module Network.AWS.CloudWatchLogs.DeleteRetentionPolicy
    (
    -- * Creating a request
      DeleteRetentionPolicy (..)
    , mkDeleteRetentionPolicy
    -- ** Request lenses
    , drpLogGroupName

    -- * Destructuring the response
    , DeleteRetentionPolicyResponse (..)
    , mkDeleteRetentionPolicyResponse
    ) where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteRetentionPolicy' smart constructor.
newtype DeleteRetentionPolicy = DeleteRetentionPolicy'
  { logGroupName :: Types.LogGroupName
    -- ^ The name of the log group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRetentionPolicy' value with any optional fields omitted.
mkDeleteRetentionPolicy
    :: Types.LogGroupName -- ^ 'logGroupName'
    -> DeleteRetentionPolicy
mkDeleteRetentionPolicy logGroupName
  = DeleteRetentionPolicy'{logGroupName}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpLogGroupName :: Lens.Lens' DeleteRetentionPolicy Types.LogGroupName
drpLogGroupName = Lens.field @"logGroupName"
{-# INLINEABLE drpLogGroupName #-}
{-# DEPRECATED logGroupName "Use generic-lens or generic-optics with 'logGroupName' instead"  #-}

instance Core.ToQuery DeleteRetentionPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteRetentionPolicy where
        toHeaders DeleteRetentionPolicy{..}
          = Core.pure ("X-Amz-Target", "Logs_20140328.DeleteRetentionPolicy")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteRetentionPolicy where
        toJSON DeleteRetentionPolicy{..}
          = Core.object
              (Core.catMaybes [Core.Just ("logGroupName" Core..= logGroupName)])

instance Core.AWSRequest DeleteRetentionPolicy where
        type Rs DeleteRetentionPolicy = DeleteRetentionPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteRetentionPolicyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteRetentionPolicyResponse' smart constructor.
data DeleteRetentionPolicyResponse = DeleteRetentionPolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRetentionPolicyResponse' value with any optional fields omitted.
mkDeleteRetentionPolicyResponse
    :: DeleteRetentionPolicyResponse
mkDeleteRetentionPolicyResponse = DeleteRetentionPolicyResponse'
