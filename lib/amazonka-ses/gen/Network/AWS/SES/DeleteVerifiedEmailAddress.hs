{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DeleteVerifiedEmailAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecated. Use the @DeleteIdentity@ operation to delete email addresses and domains.
module Network.AWS.SES.DeleteVerifiedEmailAddress
    (
    -- * Creating a request
      DeleteVerifiedEmailAddress (..)
    , mkDeleteVerifiedEmailAddress
    -- ** Request lenses
    , dveaEmailAddress

    -- * Destructuring the response
    , DeleteVerifiedEmailAddressResponse (..)
    , mkDeleteVerifiedEmailAddressResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to delete an email address from the list of email addresses you have attempted to verify under your AWS account.
--
-- /See:/ 'mkDeleteVerifiedEmailAddress' smart constructor.
newtype DeleteVerifiedEmailAddress = DeleteVerifiedEmailAddress'
  { emailAddress :: Types.Address
    -- ^ An email address to be removed from the list of verified addresses.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVerifiedEmailAddress' value with any optional fields omitted.
mkDeleteVerifiedEmailAddress
    :: Types.Address -- ^ 'emailAddress'
    -> DeleteVerifiedEmailAddress
mkDeleteVerifiedEmailAddress emailAddress
  = DeleteVerifiedEmailAddress'{emailAddress}

-- | An email address to be removed from the list of verified addresses.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dveaEmailAddress :: Lens.Lens' DeleteVerifiedEmailAddress Types.Address
dveaEmailAddress = Lens.field @"emailAddress"
{-# INLINEABLE dveaEmailAddress #-}
{-# DEPRECATED emailAddress "Use generic-lens or generic-optics with 'emailAddress' instead"  #-}

instance Core.ToQuery DeleteVerifiedEmailAddress where
        toQuery DeleteVerifiedEmailAddress{..}
          = Core.toQueryPair "Action"
              ("DeleteVerifiedEmailAddress" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "EmailAddress" emailAddress

instance Core.ToHeaders DeleteVerifiedEmailAddress where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteVerifiedEmailAddress where
        type Rs DeleteVerifiedEmailAddress =
             DeleteVerifiedEmailAddressResponse
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
        parseResponse
          = Response.receiveNull DeleteVerifiedEmailAddressResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteVerifiedEmailAddressResponse' smart constructor.
data DeleteVerifiedEmailAddressResponse = DeleteVerifiedEmailAddressResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVerifiedEmailAddressResponse' value with any optional fields omitted.
mkDeleteVerifiedEmailAddressResponse
    :: DeleteVerifiedEmailAddressResponse
mkDeleteVerifiedEmailAddressResponse
  = DeleteVerifiedEmailAddressResponse'
