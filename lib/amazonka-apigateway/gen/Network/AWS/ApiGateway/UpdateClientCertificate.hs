{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.UpdateClientCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about an 'ClientCertificate' resource.
module Network.AWS.ApiGateway.UpdateClientCertificate
    (
    -- * Creating a request
      UpdateClientCertificate (..)
    , mkUpdateClientCertificate
    -- ** Request lenses
    , uccClientCertificateId
    , uccPatchOperations

     -- * Destructuring the response
    , Types.ClientCertificate (..)
    , Types.mkClientCertificate
    -- ** Response lenses
    , Types.ccClientCertificateId
    , Types.ccCreatedDate
    , Types.ccDescription
    , Types.ccExpirationDate
    , Types.ccPemEncodedCertificate
    , Types.ccTags
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to change information about an 'ClientCertificate' resource.
--
-- /See:/ 'mkUpdateClientCertificate' smart constructor.
data UpdateClientCertificate = UpdateClientCertificate'
  { clientCertificateId :: Core.Text
    -- ^ [Required] The identifier of the 'ClientCertificate' resource to be updated.
  , patchOperations :: Core.Maybe [Types.PatchOperation]
    -- ^ A list of update operations to be applied to the specified resource and in the order specified in this list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateClientCertificate' value with any optional fields omitted.
mkUpdateClientCertificate
    :: Core.Text -- ^ 'clientCertificateId'
    -> UpdateClientCertificate
mkUpdateClientCertificate clientCertificateId
  = UpdateClientCertificate'{clientCertificateId,
                             patchOperations = Core.Nothing}

-- | [Required] The identifier of the 'ClientCertificate' resource to be updated.
--
-- /Note:/ Consider using 'clientCertificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccClientCertificateId :: Lens.Lens' UpdateClientCertificate Core.Text
uccClientCertificateId = Lens.field @"clientCertificateId"
{-# INLINEABLE uccClientCertificateId #-}
{-# DEPRECATED clientCertificateId "Use generic-lens or generic-optics with 'clientCertificateId' instead"  #-}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccPatchOperations :: Lens.Lens' UpdateClientCertificate (Core.Maybe [Types.PatchOperation])
uccPatchOperations = Lens.field @"patchOperations"
{-# INLINEABLE uccPatchOperations #-}
{-# DEPRECATED patchOperations "Use generic-lens or generic-optics with 'patchOperations' instead"  #-}

instance Core.ToQuery UpdateClientCertificate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateClientCertificate where
        toHeaders UpdateClientCertificate{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON UpdateClientCertificate where
        toJSON UpdateClientCertificate{..}
          = Core.object
              (Core.catMaybes
                 [("patchOperations" Core..=) Core.<$> patchOperations])

instance Core.AWSRequest UpdateClientCertificate where
        type Rs UpdateClientCertificate = Types.ClientCertificate
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath =
                           "/clientcertificates/" Core.<> Core.toText clientCertificateId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
