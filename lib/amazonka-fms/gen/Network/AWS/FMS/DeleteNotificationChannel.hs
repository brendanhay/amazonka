{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.DeleteNotificationChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS Firewall Manager association with the IAM role and the Amazon Simple Notification Service (SNS) topic that is used to record AWS Firewall Manager SNS logs.
module Network.AWS.FMS.DeleteNotificationChannel
    (
    -- * Creating a request
      DeleteNotificationChannel (..)
    , mkDeleteNotificationChannel

    -- * Destructuring the response
    , DeleteNotificationChannelResponse (..)
    , mkDeleteNotificationChannelResponse
    ) where

import qualified Network.AWS.FMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteNotificationChannel' smart constructor.
data DeleteNotificationChannel = DeleteNotificationChannel'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNotificationChannel' value with any optional fields omitted.
mkDeleteNotificationChannel
    :: DeleteNotificationChannel
mkDeleteNotificationChannel = DeleteNotificationChannel'

instance Core.ToQuery DeleteNotificationChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteNotificationChannel where
        toHeaders DeleteNotificationChannel{..}
          = Core.pure
              ("X-Amz-Target", "AWSFMS_20180101.DeleteNotificationChannel")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteNotificationChannel where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DeleteNotificationChannel where
        type Rs DeleteNotificationChannel =
             DeleteNotificationChannelResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeleteNotificationChannelResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteNotificationChannelResponse' smart constructor.
data DeleteNotificationChannelResponse = DeleteNotificationChannelResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNotificationChannelResponse' value with any optional fields omitted.
mkDeleteNotificationChannelResponse
    :: DeleteNotificationChannelResponse
mkDeleteNotificationChannelResponse
  = DeleteNotificationChannelResponse'
