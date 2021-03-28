{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.DeleteOrganizationalUnit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an organizational unit (OU) from a root or another OU. You must first remove all accounts and child OUs from the OU that you want to delete.
--
-- This operation can be called only from the organization's management account.
module Network.AWS.Organizations.DeleteOrganizationalUnit
    (
    -- * Creating a request
      DeleteOrganizationalUnit (..)
    , mkDeleteOrganizationalUnit
    -- ** Request lenses
    , dOrganizationalUnitId

    -- * Destructuring the response
    , DeleteOrganizationalUnitResponse (..)
    , mkDeleteOrganizationalUnitResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteOrganizationalUnit' smart constructor.
newtype DeleteOrganizationalUnit = DeleteOrganizationalUnit'
  { organizationalUnitId :: Types.OrganizationalUnitId
    -- ^ The unique identifier (ID) of the organizational unit that you want to delete. You can get the ID from the 'ListOrganizationalUnitsForParent' operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an organizational unit ID string requires "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that contains the OU). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteOrganizationalUnit' value with any optional fields omitted.
mkDeleteOrganizationalUnit
    :: Types.OrganizationalUnitId -- ^ 'organizationalUnitId'
    -> DeleteOrganizationalUnit
mkDeleteOrganizationalUnit organizationalUnitId
  = DeleteOrganizationalUnit'{organizationalUnitId}

-- | The unique identifier (ID) of the organizational unit that you want to delete. You can get the ID from the 'ListOrganizationalUnitsForParent' operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an organizational unit ID string requires "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that contains the OU). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
-- /Note:/ Consider using 'organizationalUnitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dOrganizationalUnitId :: Lens.Lens' DeleteOrganizationalUnit Types.OrganizationalUnitId
dOrganizationalUnitId = Lens.field @"organizationalUnitId"
{-# INLINEABLE dOrganizationalUnitId #-}
{-# DEPRECATED organizationalUnitId "Use generic-lens or generic-optics with 'organizationalUnitId' instead"  #-}

instance Core.ToQuery DeleteOrganizationalUnit where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteOrganizationalUnit where
        toHeaders DeleteOrganizationalUnit{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSOrganizationsV20161128.DeleteOrganizationalUnit")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteOrganizationalUnit where
        toJSON DeleteOrganizationalUnit{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("OrganizationalUnitId" Core..= organizationalUnitId)])

instance Core.AWSRequest DeleteOrganizationalUnit where
        type Rs DeleteOrganizationalUnit = DeleteOrganizationalUnitResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeleteOrganizationalUnitResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteOrganizationalUnitResponse' smart constructor.
data DeleteOrganizationalUnitResponse = DeleteOrganizationalUnitResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteOrganizationalUnitResponse' value with any optional fields omitted.
mkDeleteOrganizationalUnitResponse
    :: DeleteOrganizationalUnitResponse
mkDeleteOrganizationalUnitResponse
  = DeleteOrganizationalUnitResponse'
