{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DisassociateKmsKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the associated AWS Key Management Service (AWS KMS) customer master key (CMK) from the specified log group.
--
-- After the AWS KMS CMK is disassociated from the log group, AWS CloudWatch Logs stops encrypting newly ingested data for the log group. All previously ingested data remains encrypted, and AWS CloudWatch Logs requires permissions for the CMK whenever the encrypted data is requested.
-- Note that it can take up to 5 minutes for this operation to take effect.
module Network.AWS.CloudWatchLogs.DisassociateKmsKey
    (
    -- * Creating a request
      DisassociateKmsKey (..)
    , mkDisassociateKmsKey
    -- ** Request lenses
    , dkkLogGroupName

    -- * Destructuring the response
    , DisassociateKmsKeyResponse (..)
    , mkDisassociateKmsKeyResponse
    ) where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateKmsKey' smart constructor.
newtype DisassociateKmsKey = DisassociateKmsKey'
  { logGroupName :: Types.LogGroupName
    -- ^ The name of the log group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateKmsKey' value with any optional fields omitted.
mkDisassociateKmsKey
    :: Types.LogGroupName -- ^ 'logGroupName'
    -> DisassociateKmsKey
mkDisassociateKmsKey logGroupName
  = DisassociateKmsKey'{logGroupName}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkkLogGroupName :: Lens.Lens' DisassociateKmsKey Types.LogGroupName
dkkLogGroupName = Lens.field @"logGroupName"
{-# INLINEABLE dkkLogGroupName #-}
{-# DEPRECATED logGroupName "Use generic-lens or generic-optics with 'logGroupName' instead"  #-}

instance Core.ToQuery DisassociateKmsKey where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisassociateKmsKey where
        toHeaders DisassociateKmsKey{..}
          = Core.pure ("X-Amz-Target", "Logs_20140328.DisassociateKmsKey")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DisassociateKmsKey where
        toJSON DisassociateKmsKey{..}
          = Core.object
              (Core.catMaybes [Core.Just ("logGroupName" Core..= logGroupName)])

instance Core.AWSRequest DisassociateKmsKey where
        type Rs DisassociateKmsKey = DisassociateKmsKeyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DisassociateKmsKeyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateKmsKeyResponse' smart constructor.
data DisassociateKmsKeyResponse = DisassociateKmsKeyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateKmsKeyResponse' value with any optional fields omitted.
mkDisassociateKmsKeyResponse
    :: DisassociateKmsKeyResponse
mkDisassociateKmsKeyResponse = DisassociateKmsKeyResponse'
