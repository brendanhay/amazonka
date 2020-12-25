{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DisassociateSecurityKey (..),
    mkDisassociateSecurityKey,

    -- ** Request lenses
    dskInstanceId,
    dskAssociationId,

    -- * Destructuring the response
    DisassociateSecurityKeyResponse (..),
    mkDisassociateSecurityKeyResponse,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateSecurityKey' smart constructor.
data DisassociateSecurityKey = DisassociateSecurityKey'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
    associationId :: Types.AssociationId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateSecurityKey' value with any optional fields omitted.
mkDisassociateSecurityKey ::
  -- | 'instanceId'
  Types.InstanceId ->
  -- | 'associationId'
  Types.AssociationId ->
  DisassociateSecurityKey
mkDisassociateSecurityKey instanceId associationId =
  DisassociateSecurityKey' {instanceId, associationId}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dskInstanceId :: Lens.Lens' DisassociateSecurityKey Types.InstanceId
dskInstanceId = Lens.field @"instanceId"
{-# DEPRECATED dskInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dskAssociationId :: Lens.Lens' DisassociateSecurityKey Types.AssociationId
dskAssociationId = Lens.field @"associationId"
{-# DEPRECATED dskAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

instance Core.AWSRequest DisassociateSecurityKey where
  type Rs DisassociateSecurityKey = DisassociateSecurityKeyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/instance/" Core.<> (Core.toText instanceId)
                Core.<> ("/security-key/")
                Core.<> (Core.toText associationId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response = Response.receiveNull DisassociateSecurityKeyResponse'

-- | /See:/ 'mkDisassociateSecurityKeyResponse' smart constructor.
data DisassociateSecurityKeyResponse = DisassociateSecurityKeyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateSecurityKeyResponse' value with any optional fields omitted.
mkDisassociateSecurityKeyResponse ::
  DisassociateSecurityKeyResponse
mkDisassociateSecurityKeyResponse =
  DisassociateSecurityKeyResponse'
