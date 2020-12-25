{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteKeyPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified key pair, by removing the public key from Amazon EC2.
module Network.AWS.EC2.DeleteKeyPair
  ( -- * Creating a request
    DeleteKeyPair (..),
    mkDeleteKeyPair,

    -- ** Request lenses
    dkpDryRun,
    dkpKeyName,
    dkpKeyPairId,

    -- * Destructuring the response
    DeleteKeyPairResponse (..),
    mkDeleteKeyPairResponse,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteKeyPair' smart constructor.
data DeleteKeyPair = DeleteKeyPair'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The name of the key pair.
    keyName :: Core.Maybe Types.KeyPairName,
    -- | The ID of the key pair.
    keyPairId :: Core.Maybe Types.KeyPairId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteKeyPair' value with any optional fields omitted.
mkDeleteKeyPair ::
  DeleteKeyPair
mkDeleteKeyPair =
  DeleteKeyPair'
    { dryRun = Core.Nothing,
      keyName = Core.Nothing,
      keyPairId = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkpDryRun :: Lens.Lens' DeleteKeyPair (Core.Maybe Core.Bool)
dkpDryRun = Lens.field @"dryRun"
{-# DEPRECATED dkpDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The name of the key pair.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkpKeyName :: Lens.Lens' DeleteKeyPair (Core.Maybe Types.KeyPairName)
dkpKeyName = Lens.field @"keyName"
{-# DEPRECATED dkpKeyName "Use generic-lens or generic-optics with 'keyName' instead." #-}

-- | The ID of the key pair.
--
-- /Note:/ Consider using 'keyPairId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkpKeyPairId :: Lens.Lens' DeleteKeyPair (Core.Maybe Types.KeyPairId)
dkpKeyPairId = Lens.field @"keyPairId"
{-# DEPRECATED dkpKeyPairId "Use generic-lens or generic-optics with 'keyPairId' instead." #-}

instance Core.AWSRequest DeleteKeyPair where
  type Rs DeleteKeyPair = DeleteKeyPairResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DeleteKeyPair")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "KeyName" Core.<$> keyName)
                Core.<> (Core.toQueryValue "KeyPairId" Core.<$> keyPairId)
            )
      }
  response = Response.receiveNull DeleteKeyPairResponse'

-- | /See:/ 'mkDeleteKeyPairResponse' smart constructor.
data DeleteKeyPairResponse = DeleteKeyPairResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteKeyPairResponse' value with any optional fields omitted.
mkDeleteKeyPairResponse ::
  DeleteKeyPairResponse
mkDeleteKeyPairResponse = DeleteKeyPairResponse'
