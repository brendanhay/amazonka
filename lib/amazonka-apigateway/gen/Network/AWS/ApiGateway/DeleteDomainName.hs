{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.DeleteDomainName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the 'DomainName' resource.
module Network.AWS.ApiGateway.DeleteDomainName
    (
    -- * Creating a request
      DeleteDomainName (..)
    , mkDeleteDomainName
    -- ** Request lenses
    , ddnDomainName

    -- * Destructuring the response
    , DeleteDomainNameResponse (..)
    , mkDeleteDomainNameResponse
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to delete the 'DomainName' resource.
--
-- /See:/ 'mkDeleteDomainName' smart constructor.
newtype DeleteDomainName = DeleteDomainName'
  { domainName :: Core.Text
    -- ^ [Required] The name of the 'DomainName' resource to be deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDomainName' value with any optional fields omitted.
mkDeleteDomainName
    :: Core.Text -- ^ 'domainName'
    -> DeleteDomainName
mkDeleteDomainName domainName = DeleteDomainName'{domainName}

-- | [Required] The name of the 'DomainName' resource to be deleted.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddnDomainName :: Lens.Lens' DeleteDomainName Core.Text
ddnDomainName = Lens.field @"domainName"
{-# INLINEABLE ddnDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

instance Core.ToQuery DeleteDomainName where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteDomainName where
        toHeaders DeleteDomainName{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest DeleteDomainName where
        type Rs DeleteDomainName = DeleteDomainNameResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/domainnames/" Core.<> Core.toText domainName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteDomainNameResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDomainNameResponse' smart constructor.
data DeleteDomainNameResponse = DeleteDomainNameResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDomainNameResponse' value with any optional fields omitted.
mkDeleteDomainNameResponse
    :: DeleteDomainNameResponse
mkDeleteDomainNameResponse = DeleteDomainNameResponse'
