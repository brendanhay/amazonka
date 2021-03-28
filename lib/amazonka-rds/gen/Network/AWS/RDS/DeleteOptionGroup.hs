{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteOptionGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing option group.
module Network.AWS.RDS.DeleteOptionGroup
    (
    -- * Creating a request
      DeleteOptionGroup (..)
    , mkDeleteOptionGroup
    -- ** Request lenses
    , dOptionGroupName

    -- * Destructuring the response
    , DeleteOptionGroupResponse (..)
    , mkDeleteOptionGroupResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDeleteOptionGroup' smart constructor.
newtype DeleteOptionGroup = DeleteOptionGroup'
  { optionGroupName :: Core.Text
    -- ^ The name of the option group to be deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteOptionGroup' value with any optional fields omitted.
mkDeleteOptionGroup
    :: Core.Text -- ^ 'optionGroupName'
    -> DeleteOptionGroup
mkDeleteOptionGroup optionGroupName
  = DeleteOptionGroup'{optionGroupName}

-- | The name of the option group to be deleted.
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dOptionGroupName :: Lens.Lens' DeleteOptionGroup Core.Text
dOptionGroupName = Lens.field @"optionGroupName"
{-# INLINEABLE dOptionGroupName #-}
{-# DEPRECATED optionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead"  #-}

instance Core.ToQuery DeleteOptionGroup where
        toQuery DeleteOptionGroup{..}
          = Core.toQueryPair "Action" ("DeleteOptionGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<> Core.toQueryPair "OptionGroupName" optionGroupName

instance Core.ToHeaders DeleteOptionGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteOptionGroup where
        type Rs DeleteOptionGroup = DeleteOptionGroupResponse
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
        parseResponse = Response.receiveNull DeleteOptionGroupResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteOptionGroupResponse' smart constructor.
data DeleteOptionGroupResponse = DeleteOptionGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteOptionGroupResponse' value with any optional fields omitted.
mkDeleteOptionGroupResponse
    :: DeleteOptionGroupResponse
mkDeleteOptionGroupResponse = DeleteOptionGroupResponse'
