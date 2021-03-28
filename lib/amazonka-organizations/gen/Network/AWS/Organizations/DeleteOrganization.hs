{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.DeleteOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the organization. You can delete an organization only by using credentials from the management account. The organization must be empty of member accounts.
module Network.AWS.Organizations.DeleteOrganization
    (
    -- * Creating a request
      DeleteOrganization (..)
    , mkDeleteOrganization

    -- * Destructuring the response
    , DeleteOrganizationResponse (..)
    , mkDeleteOrganizationResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteOrganization' smart constructor.
data DeleteOrganization = DeleteOrganization'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteOrganization' value with any optional fields omitted.
mkDeleteOrganization
    :: DeleteOrganization
mkDeleteOrganization = DeleteOrganization'

instance Core.ToQuery DeleteOrganization where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteOrganization where
        toHeaders DeleteOrganization{..}
          = Core.pure
              ("X-Amz-Target", "AWSOrganizationsV20161128.DeleteOrganization")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteOrganization where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DeleteOrganization where
        type Rs DeleteOrganization = DeleteOrganizationResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteOrganizationResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteOrganizationResponse' smart constructor.
data DeleteOrganizationResponse = DeleteOrganizationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteOrganizationResponse' value with any optional fields omitted.
mkDeleteOrganizationResponse
    :: DeleteOrganizationResponse
mkDeleteOrganizationResponse = DeleteOrganizationResponse'
