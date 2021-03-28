{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.CreateOrganizationalUnit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an organizational unit (OU) within a root or parent OU. An OU is a container for accounts that enables you to organize your accounts to apply policies according to your business requirements. The number of levels deep that you can nest OUs is dependent upon the policy types enabled for that root. For service control policies, the limit is five.
--
-- For more information about OUs, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_ous.html Managing Organizational Units> in the /AWS Organizations User Guide./ 
-- If the request includes tags, then the requester must have the @organizations:TagResource@ permission.
-- This operation can be called only from the organization's management account.
module Network.AWS.Organizations.CreateOrganizationalUnit
    (
    -- * Creating a request
      CreateOrganizationalUnit (..)
    , mkCreateOrganizationalUnit
    -- ** Request lenses
    , couParentId
    , couName
    , couTags

    -- * Destructuring the response
    , CreateOrganizationalUnitResponse (..)
    , mkCreateOrganizationalUnitResponse
    -- ** Response lenses
    , courrsOrganizationalUnit
    , courrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateOrganizationalUnit' smart constructor.
data CreateOrganizationalUnit = CreateOrganizationalUnit'
  { parentId :: Types.ParentId
    -- ^ The unique identifier (ID) of the parent root or OU that you want to create the new OU in.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:
--
--     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
  , name :: Types.Name
    -- ^ The friendly name to assign to the new OU.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of tags that you want to attach to the newly created OU. For each tag in the list, you must specify both a tag key and a value. You can set the value to an empty string, but you can't set it to @null@ . For more information about tagging, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources> in the AWS Organizations User Guide.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateOrganizationalUnit' value with any optional fields omitted.
mkCreateOrganizationalUnit
    :: Types.ParentId -- ^ 'parentId'
    -> Types.Name -- ^ 'name'
    -> CreateOrganizationalUnit
mkCreateOrganizationalUnit parentId name
  = CreateOrganizationalUnit'{parentId, name, tags = Core.Nothing}

-- | The unique identifier (ID) of the parent root or OU that you want to create the new OU in.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:
--
--     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
--
-- /Note:/ Consider using 'parentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
couParentId :: Lens.Lens' CreateOrganizationalUnit Types.ParentId
couParentId = Lens.field @"parentId"
{-# INLINEABLE couParentId #-}
{-# DEPRECATED parentId "Use generic-lens or generic-optics with 'parentId' instead"  #-}

-- | The friendly name to assign to the new OU.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
couName :: Lens.Lens' CreateOrganizationalUnit Types.Name
couName = Lens.field @"name"
{-# INLINEABLE couName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A list of tags that you want to attach to the newly created OU. For each tag in the list, you must specify both a tag key and a value. You can set the value to an empty string, but you can't set it to @null@ . For more information about tagging, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources> in the AWS Organizations User Guide.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
couTags :: Lens.Lens' CreateOrganizationalUnit (Core.Maybe [Types.Tag])
couTags = Lens.field @"tags"
{-# INLINEABLE couTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateOrganizationalUnit where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateOrganizationalUnit where
        toHeaders CreateOrganizationalUnit{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSOrganizationsV20161128.CreateOrganizationalUnit")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateOrganizationalUnit where
        toJSON CreateOrganizationalUnit{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ParentId" Core..= parentId),
                  Core.Just ("Name" Core..= name), ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateOrganizationalUnit where
        type Rs CreateOrganizationalUnit = CreateOrganizationalUnitResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateOrganizationalUnitResponse' Core.<$>
                   (x Core..:? "OrganizationalUnit") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateOrganizationalUnitResponse' smart constructor.
data CreateOrganizationalUnitResponse = CreateOrganizationalUnitResponse'
  { organizationalUnit :: Core.Maybe Types.OrganizationalUnit
    -- ^ A structure that contains details about the newly created OU.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateOrganizationalUnitResponse' value with any optional fields omitted.
mkCreateOrganizationalUnitResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateOrganizationalUnitResponse
mkCreateOrganizationalUnitResponse responseStatus
  = CreateOrganizationalUnitResponse'{organizationalUnit =
                                        Core.Nothing,
                                      responseStatus}

-- | A structure that contains details about the newly created OU.
--
-- /Note:/ Consider using 'organizationalUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
courrsOrganizationalUnit :: Lens.Lens' CreateOrganizationalUnitResponse (Core.Maybe Types.OrganizationalUnit)
courrsOrganizationalUnit = Lens.field @"organizationalUnit"
{-# INLINEABLE courrsOrganizationalUnit #-}
{-# DEPRECATED organizationalUnit "Use generic-lens or generic-optics with 'organizationalUnit' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
courrsResponseStatus :: Lens.Lens' CreateOrganizationalUnitResponse Core.Int
courrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE courrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
