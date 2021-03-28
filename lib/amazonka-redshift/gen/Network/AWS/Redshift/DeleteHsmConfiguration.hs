{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteHsmConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Amazon Redshift HSM configuration.
module Network.AWS.Redshift.DeleteHsmConfiguration
    (
    -- * Creating a request
      DeleteHsmConfiguration (..)
    , mkDeleteHsmConfiguration
    -- ** Request lenses
    , dhcHsmConfigurationIdentifier

    -- * Destructuring the response
    , DeleteHsmConfigurationResponse (..)
    , mkDeleteHsmConfigurationResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDeleteHsmConfiguration' smart constructor.
newtype DeleteHsmConfiguration = DeleteHsmConfiguration'
  { hsmConfigurationIdentifier :: Core.Text
    -- ^ The identifier of the Amazon Redshift HSM configuration to be deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteHsmConfiguration' value with any optional fields omitted.
mkDeleteHsmConfiguration
    :: Core.Text -- ^ 'hsmConfigurationIdentifier'
    -> DeleteHsmConfiguration
mkDeleteHsmConfiguration hsmConfigurationIdentifier
  = DeleteHsmConfiguration'{hsmConfigurationIdentifier}

-- | The identifier of the Amazon Redshift HSM configuration to be deleted.
--
-- /Note:/ Consider using 'hsmConfigurationIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcHsmConfigurationIdentifier :: Lens.Lens' DeleteHsmConfiguration Core.Text
dhcHsmConfigurationIdentifier = Lens.field @"hsmConfigurationIdentifier"
{-# INLINEABLE dhcHsmConfigurationIdentifier #-}
{-# DEPRECATED hsmConfigurationIdentifier "Use generic-lens or generic-optics with 'hsmConfigurationIdentifier' instead"  #-}

instance Core.ToQuery DeleteHsmConfiguration where
        toQuery DeleteHsmConfiguration{..}
          = Core.toQueryPair "Action" ("DeleteHsmConfiguration" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "HsmConfigurationIdentifier"
                hsmConfigurationIdentifier

instance Core.ToHeaders DeleteHsmConfiguration where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteHsmConfiguration where
        type Rs DeleteHsmConfiguration = DeleteHsmConfigurationResponse
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
          = Response.receiveNull DeleteHsmConfigurationResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteHsmConfigurationResponse' smart constructor.
data DeleteHsmConfigurationResponse = DeleteHsmConfigurationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteHsmConfigurationResponse' value with any optional fields omitted.
mkDeleteHsmConfigurationResponse
    :: DeleteHsmConfigurationResponse
mkDeleteHsmConfigurationResponse = DeleteHsmConfigurationResponse'
