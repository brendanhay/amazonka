{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.DeleteDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @DeleteDomain@ operation deletes a domain. Any items (and their attributes) in the domain are deleted as well. The @DeleteDomain@ operation might take 10 or more seconds to complete. 
module Network.AWS.SDB.DeleteDomain
    (
    -- * Creating a request
      DeleteDomain (..)
    , mkDeleteDomain
    -- ** Request lenses
    , ddDomainName

    -- * Destructuring the response
    , DeleteDomainResponse (..)
    , mkDeleteDomainResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SDB.Types as Types

-- | /See:/ 'mkDeleteDomain' smart constructor.
newtype DeleteDomain = DeleteDomain'
  { domainName :: Core.Text
    -- ^ The name of the domain to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDomain' value with any optional fields omitted.
mkDeleteDomain
    :: Core.Text -- ^ 'domainName'
    -> DeleteDomain
mkDeleteDomain domainName = DeleteDomain'{domainName}

-- | The name of the domain to delete.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDomainName :: Lens.Lens' DeleteDomain Core.Text
ddDomainName = Lens.field @"domainName"
{-# INLINEABLE ddDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

instance Core.ToQuery DeleteDomain where
        toQuery DeleteDomain{..}
          = Core.toQueryPair "Action" ("DeleteDomain" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2009-04-15" :: Core.Text)
              Core.<> Core.toQueryPair "DomainName" domainName

instance Core.ToHeaders DeleteDomain where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteDomain where
        type Rs DeleteDomain = DeleteDomainResponse
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
        parseResponse = Response.receiveNull DeleteDomainResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDomainResponse' smart constructor.
data DeleteDomainResponse = DeleteDomainResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDomainResponse' value with any optional fields omitted.
mkDeleteDomainResponse
    :: DeleteDomainResponse
mkDeleteDomainResponse = DeleteDomainResponse'
