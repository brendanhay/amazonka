{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeleteOrganizationConfigRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified organization config rule and all of its evaluation results from all member accounts in that organization. 
--
-- Only a master account and a delegated administrator account can delete an organization config rule. When calling this API with a delegated administrator, you must ensure AWS Organizations @ListDelegatedAdministrator@ permissions are added.
-- AWS Config sets the state of a rule to DELETE_IN_PROGRESS until the deletion is complete. You cannot update a rule while it is in this state.
module Network.AWS.Config.DeleteOrganizationConfigRule
    (
    -- * Creating a request
      DeleteOrganizationConfigRule (..)
    , mkDeleteOrganizationConfigRule
    -- ** Request lenses
    , docrOrganizationConfigRuleName

    -- * Destructuring the response
    , DeleteOrganizationConfigRuleResponse (..)
    , mkDeleteOrganizationConfigRuleResponse
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteOrganizationConfigRule' smart constructor.
newtype DeleteOrganizationConfigRule = DeleteOrganizationConfigRule'
  { organizationConfigRuleName :: Types.OrganizationConfigRuleName
    -- ^ The name of organization config rule that you want to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteOrganizationConfigRule' value with any optional fields omitted.
mkDeleteOrganizationConfigRule
    :: Types.OrganizationConfigRuleName -- ^ 'organizationConfigRuleName'
    -> DeleteOrganizationConfigRule
mkDeleteOrganizationConfigRule organizationConfigRuleName
  = DeleteOrganizationConfigRule'{organizationConfigRuleName}

-- | The name of organization config rule that you want to delete.
--
-- /Note:/ Consider using 'organizationConfigRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrOrganizationConfigRuleName :: Lens.Lens' DeleteOrganizationConfigRule Types.OrganizationConfigRuleName
docrOrganizationConfigRuleName = Lens.field @"organizationConfigRuleName"
{-# INLINEABLE docrOrganizationConfigRuleName #-}
{-# DEPRECATED organizationConfigRuleName "Use generic-lens or generic-optics with 'organizationConfigRuleName' instead"  #-}

instance Core.ToQuery DeleteOrganizationConfigRule where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteOrganizationConfigRule where
        toHeaders DeleteOrganizationConfigRule{..}
          = Core.pure
              ("X-Amz-Target",
               "StarlingDoveService.DeleteOrganizationConfigRule")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteOrganizationConfigRule where
        toJSON DeleteOrganizationConfigRule{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("OrganizationConfigRuleName" Core..= organizationConfigRuleName)])

instance Core.AWSRequest DeleteOrganizationConfigRule where
        type Rs DeleteOrganizationConfigRule =
             DeleteOrganizationConfigRuleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeleteOrganizationConfigRuleResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteOrganizationConfigRuleResponse' smart constructor.
data DeleteOrganizationConfigRuleResponse = DeleteOrganizationConfigRuleResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteOrganizationConfigRuleResponse' value with any optional fields omitted.
mkDeleteOrganizationConfigRuleResponse
    :: DeleteOrganizationConfigRuleResponse
mkDeleteOrganizationConfigRuleResponse
  = DeleteOrganizationConfigRuleResponse'
