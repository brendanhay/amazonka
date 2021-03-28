{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.UndeprecateDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undeprecates a previously deprecated domain. After a domain has been undeprecated it can be used to create new workflow executions or register new types.
--
-- __Access Control__ 
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--
--     * You cannot use an IAM policy to constrain this action's parameters.
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
module Network.AWS.SWF.UndeprecateDomain
    (
    -- * Creating a request
      UndeprecateDomain (..)
    , mkUndeprecateDomain
    -- ** Request lenses
    , udName

    -- * Destructuring the response
    , UndeprecateDomainResponse (..)
    , mkUndeprecateDomainResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SWF.Types as Types

-- | /See:/ 'mkUndeprecateDomain' smart constructor.
newtype UndeprecateDomain = UndeprecateDomain'
  { name :: Types.DomainName
    -- ^ The name of the domain of the deprecated workflow type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UndeprecateDomain' value with any optional fields omitted.
mkUndeprecateDomain
    :: Types.DomainName -- ^ 'name'
    -> UndeprecateDomain
mkUndeprecateDomain name = UndeprecateDomain'{name}

-- | The name of the domain of the deprecated workflow type.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udName :: Lens.Lens' UndeprecateDomain Types.DomainName
udName = Lens.field @"name"
{-# INLINEABLE udName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery UndeprecateDomain where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UndeprecateDomain where
        toHeaders UndeprecateDomain{..}
          = Core.pure
              ("X-Amz-Target", "SimpleWorkflowService.UndeprecateDomain")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON UndeprecateDomain where
        toJSON UndeprecateDomain{..}
          = Core.object (Core.catMaybes [Core.Just ("name" Core..= name)])

instance Core.AWSRequest UndeprecateDomain where
        type Rs UndeprecateDomain = UndeprecateDomainResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull UndeprecateDomainResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUndeprecateDomainResponse' smart constructor.
data UndeprecateDomainResponse = UndeprecateDomainResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UndeprecateDomainResponse' value with any optional fields omitted.
mkUndeprecateDomainResponse
    :: UndeprecateDomainResponse
mkUndeprecateDomainResponse = UndeprecateDomainResponse'
