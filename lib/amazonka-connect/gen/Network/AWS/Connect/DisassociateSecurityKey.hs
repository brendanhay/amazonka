{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DisassociateSecurityKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified security key.
module Network.AWS.Connect.DisassociateSecurityKey
    (
    -- * Creating a request
      DisassociateSecurityKey (..)
    , mkDisassociateSecurityKey
    -- ** Request lenses
    , dskInstanceId
    , dskAssociationId

    -- * Destructuring the response
    , DisassociateSecurityKeyResponse (..)
    , mkDisassociateSecurityKeyResponse
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateSecurityKey' smart constructor.
data DisassociateSecurityKey = DisassociateSecurityKey'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , associationId :: Types.AssociationId
    -- ^ The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateSecurityKey' value with any optional fields omitted.
mkDisassociateSecurityKey
    :: Types.InstanceId -- ^ 'instanceId'
    -> Types.AssociationId -- ^ 'associationId'
    -> DisassociateSecurityKey
mkDisassociateSecurityKey instanceId associationId
  = DisassociateSecurityKey'{instanceId, associationId}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dskInstanceId :: Lens.Lens' DisassociateSecurityKey Types.InstanceId
dskInstanceId = Lens.field @"instanceId"
{-# INLINEABLE dskInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dskAssociationId :: Lens.Lens' DisassociateSecurityKey Types.AssociationId
dskAssociationId = Lens.field @"associationId"
{-# INLINEABLE dskAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

instance Core.ToQuery DisassociateSecurityKey where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisassociateSecurityKey where
        toHeaders DisassociateSecurityKey{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DisassociateSecurityKey where
        type Rs DisassociateSecurityKey = DisassociateSecurityKeyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/instance/" Core.<> Core.toText instanceId Core.<>
                             "/security-key/"
                             Core.<> Core.toText associationId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DisassociateSecurityKeyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateSecurityKeyResponse' smart constructor.
data DisassociateSecurityKeyResponse = DisassociateSecurityKeyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateSecurityKeyResponse' value with any optional fields omitted.
mkDisassociateSecurityKeyResponse
    :: DisassociateSecurityKeyResponse
mkDisassociateSecurityKeyResponse
  = DisassociateSecurityKeyResponse'
