{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteKeyPair (..)
    , mkDeleteKeyPair
    -- ** Request lenses
    , dkpDryRun
    , dkpKeyName
    , dkpKeyPairId

    -- * Destructuring the response
    , DeleteKeyPairResponse (..)
    , mkDeleteKeyPairResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteKeyPair' smart constructor.
data DeleteKeyPair = DeleteKeyPair'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , keyName :: Core.Maybe Types.KeyPairName
    -- ^ The name of the key pair.
  , keyPairId :: Core.Maybe Types.KeyPairId
    -- ^ The ID of the key pair.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteKeyPair' value with any optional fields omitted.
mkDeleteKeyPair
    :: DeleteKeyPair
mkDeleteKeyPair
  = DeleteKeyPair'{dryRun = Core.Nothing, keyName = Core.Nothing,
                   keyPairId = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkpDryRun :: Lens.Lens' DeleteKeyPair (Core.Maybe Core.Bool)
dkpDryRun = Lens.field @"dryRun"
{-# INLINEABLE dkpDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The name of the key pair.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkpKeyName :: Lens.Lens' DeleteKeyPair (Core.Maybe Types.KeyPairName)
dkpKeyName = Lens.field @"keyName"
{-# INLINEABLE dkpKeyName #-}
{-# DEPRECATED keyName "Use generic-lens or generic-optics with 'keyName' instead"  #-}

-- | The ID of the key pair.
--
-- /Note:/ Consider using 'keyPairId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkpKeyPairId :: Lens.Lens' DeleteKeyPair (Core.Maybe Types.KeyPairId)
dkpKeyPairId = Lens.field @"keyPairId"
{-# INLINEABLE dkpKeyPairId #-}
{-# DEPRECATED keyPairId "Use generic-lens or generic-optics with 'keyPairId' instead"  #-}

instance Core.ToQuery DeleteKeyPair where
        toQuery DeleteKeyPair{..}
          = Core.toQueryPair "Action" ("DeleteKeyPair" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "KeyName") keyName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "KeyPairId") keyPairId

instance Core.ToHeaders DeleteKeyPair where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteKeyPair where
        type Rs DeleteKeyPair = DeleteKeyPairResponse
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
        parseResponse = Response.receiveNull DeleteKeyPairResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteKeyPairResponse' smart constructor.
data DeleteKeyPairResponse = DeleteKeyPairResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteKeyPairResponse' value with any optional fields omitted.
mkDeleteKeyPairResponse
    :: DeleteKeyPairResponse
mkDeleteKeyPairResponse = DeleteKeyPairResponse'
