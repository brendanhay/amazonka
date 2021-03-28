{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeChapCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of Challenge-Handshake Authentication Protocol (CHAP) credentials information for a specified iSCSI target, one for each target-initiator pair. This operation is supported in the volume and tape gateway types.
module Network.AWS.StorageGateway.DescribeChapCredentials
    (
    -- * Creating a request
      DescribeChapCredentials (..)
    , mkDescribeChapCredentials
    -- ** Request lenses
    , dccTargetARN

    -- * Destructuring the response
    , DescribeChapCredentialsResponse (..)
    , mkDescribeChapCredentialsResponse
    -- ** Response lenses
    , dccrrsChapCredentials
    , dccrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | A JSON object containing the Amazon Resource Name (ARN) of the iSCSI volume target.
--
-- /See:/ 'mkDescribeChapCredentials' smart constructor.
newtype DescribeChapCredentials = DescribeChapCredentials'
  { targetARN :: Types.TargetARN
    -- ^ The Amazon Resource Name (ARN) of the iSCSI volume target. Use the 'DescribeStorediSCSIVolumes' operation to return to retrieve the TargetARN for specified VolumeARN.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeChapCredentials' value with any optional fields omitted.
mkDescribeChapCredentials
    :: Types.TargetARN -- ^ 'targetARN'
    -> DescribeChapCredentials
mkDescribeChapCredentials targetARN
  = DescribeChapCredentials'{targetARN}

-- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the 'DescribeStorediSCSIVolumes' operation to return to retrieve the TargetARN for specified VolumeARN.
--
-- /Note:/ Consider using 'targetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccTargetARN :: Lens.Lens' DescribeChapCredentials Types.TargetARN
dccTargetARN = Lens.field @"targetARN"
{-# INLINEABLE dccTargetARN #-}
{-# DEPRECATED targetARN "Use generic-lens or generic-optics with 'targetARN' instead"  #-}

instance Core.ToQuery DescribeChapCredentials where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeChapCredentials where
        toHeaders DescribeChapCredentials{..}
          = Core.pure
              ("X-Amz-Target", "StorageGateway_20130630.DescribeChapCredentials")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeChapCredentials where
        toJSON DescribeChapCredentials{..}
          = Core.object
              (Core.catMaybes [Core.Just ("TargetARN" Core..= targetARN)])

instance Core.AWSRequest DescribeChapCredentials where
        type Rs DescribeChapCredentials = DescribeChapCredentialsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeChapCredentialsResponse' Core.<$>
                   (x Core..:? "ChapCredentials") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A JSON object containing the following fields:
--
-- /See:/ 'mkDescribeChapCredentialsResponse' smart constructor.
data DescribeChapCredentialsResponse = DescribeChapCredentialsResponse'
  { chapCredentials :: Core.Maybe [Types.ChapInfo]
    -- ^ An array of 'ChapInfo' objects that represent CHAP credentials. Each object in the array contains CHAP credential information for one target-initiator pair. If no CHAP credentials are set, an empty array is returned. CHAP credential information is provided in a JSON object with the following fields:
--
--
--     * __InitiatorName__ : The iSCSI initiator that connects to the target.
--
--
--     * __SecretToAuthenticateInitiator__ : The secret key that the initiator (for example, the Windows client) must provide to participate in mutual CHAP with the target.
--
--
--     * __SecretToAuthenticateTarget__ : The secret key that the target must provide to participate in mutual CHAP with the initiator (e.g. Windows client).
--
--
--     * __TargetARN__ : The Amazon Resource Name (ARN) of the storage volume.
--
--
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeChapCredentialsResponse' value with any optional fields omitted.
mkDescribeChapCredentialsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeChapCredentialsResponse
mkDescribeChapCredentialsResponse responseStatus
  = DescribeChapCredentialsResponse'{chapCredentials = Core.Nothing,
                                     responseStatus}

-- | An array of 'ChapInfo' objects that represent CHAP credentials. Each object in the array contains CHAP credential information for one target-initiator pair. If no CHAP credentials are set, an empty array is returned. CHAP credential information is provided in a JSON object with the following fields:
--
--
--     * __InitiatorName__ : The iSCSI initiator that connects to the target.
--
--
--     * __SecretToAuthenticateInitiator__ : The secret key that the initiator (for example, the Windows client) must provide to participate in mutual CHAP with the target.
--
--
--     * __SecretToAuthenticateTarget__ : The secret key that the target must provide to participate in mutual CHAP with the initiator (e.g. Windows client).
--
--
--     * __TargetARN__ : The Amazon Resource Name (ARN) of the storage volume.
--
--
--
-- /Note:/ Consider using 'chapCredentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccrrsChapCredentials :: Lens.Lens' DescribeChapCredentialsResponse (Core.Maybe [Types.ChapInfo])
dccrrsChapCredentials = Lens.field @"chapCredentials"
{-# INLINEABLE dccrrsChapCredentials #-}
{-# DEPRECATED chapCredentials "Use generic-lens or generic-optics with 'chapCredentials' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccrrsResponseStatus :: Lens.Lens' DescribeChapCredentialsResponse Core.Int
dccrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dccrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
