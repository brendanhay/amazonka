{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyOptionGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing option group.
module Network.AWS.RDS.ModifyOptionGroup
    (
    -- * Creating a request
      ModifyOptionGroup (..)
    , mkModifyOptionGroup
    -- ** Request lenses
    , mogOptionGroupName
    , mogApplyImmediately
    , mogOptionsToInclude
    , mogOptionsToRemove

    -- * Destructuring the response
    , ModifyOptionGroupResponse (..)
    , mkModifyOptionGroupResponse
    -- ** Response lenses
    , mogrrsOptionGroup
    , mogrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkModifyOptionGroup' smart constructor.
data ModifyOptionGroup = ModifyOptionGroup'
  { optionGroupName :: Core.Text
    -- ^ The name of the option group to be modified.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group, and that option group can't be removed from a DB instance once it is associated with a DB instance
  , applyImmediately :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to apply the change immediately or during the next maintenance window for each instance associated with the option group.
  , optionsToInclude :: Core.Maybe [Types.OptionConfiguration]
    -- ^ Options in this list are added to the option group or, if already present, the specified configuration is used to update the existing configuration.
  , optionsToRemove :: Core.Maybe [Core.Text]
    -- ^ Options in this list are removed from the option group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyOptionGroup' value with any optional fields omitted.
mkModifyOptionGroup
    :: Core.Text -- ^ 'optionGroupName'
    -> ModifyOptionGroup
mkModifyOptionGroup optionGroupName
  = ModifyOptionGroup'{optionGroupName,
                       applyImmediately = Core.Nothing, optionsToInclude = Core.Nothing,
                       optionsToRemove = Core.Nothing}

-- | The name of the option group to be modified.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group, and that option group can't be removed from a DB instance once it is associated with a DB instance
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mogOptionGroupName :: Lens.Lens' ModifyOptionGroup Core.Text
mogOptionGroupName = Lens.field @"optionGroupName"
{-# INLINEABLE mogOptionGroupName #-}
{-# DEPRECATED optionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead"  #-}

-- | A value that indicates whether to apply the change immediately or during the next maintenance window for each instance associated with the option group.
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mogApplyImmediately :: Lens.Lens' ModifyOptionGroup (Core.Maybe Core.Bool)
mogApplyImmediately = Lens.field @"applyImmediately"
{-# INLINEABLE mogApplyImmediately #-}
{-# DEPRECATED applyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead"  #-}

-- | Options in this list are added to the option group or, if already present, the specified configuration is used to update the existing configuration.
--
-- /Note:/ Consider using 'optionsToInclude' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mogOptionsToInclude :: Lens.Lens' ModifyOptionGroup (Core.Maybe [Types.OptionConfiguration])
mogOptionsToInclude = Lens.field @"optionsToInclude"
{-# INLINEABLE mogOptionsToInclude #-}
{-# DEPRECATED optionsToInclude "Use generic-lens or generic-optics with 'optionsToInclude' instead"  #-}

-- | Options in this list are removed from the option group.
--
-- /Note:/ Consider using 'optionsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mogOptionsToRemove :: Lens.Lens' ModifyOptionGroup (Core.Maybe [Core.Text])
mogOptionsToRemove = Lens.field @"optionsToRemove"
{-# INLINEABLE mogOptionsToRemove #-}
{-# DEPRECATED optionsToRemove "Use generic-lens or generic-optics with 'optionsToRemove' instead"  #-}

instance Core.ToQuery ModifyOptionGroup where
        toQuery ModifyOptionGroup{..}
          = Core.toQueryPair "Action" ("ModifyOptionGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<> Core.toQueryPair "OptionGroupName" optionGroupName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ApplyImmediately")
                applyImmediately
              Core.<>
              Core.toQueryPair "OptionsToInclude"
                (Core.maybe Core.mempty (Core.toQueryList "OptionConfiguration")
                   optionsToInclude)
              Core.<>
              Core.toQueryPair "OptionsToRemove"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   optionsToRemove)

instance Core.ToHeaders ModifyOptionGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyOptionGroup where
        type Rs ModifyOptionGroup = ModifyOptionGroupResponse
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
          = Response.receiveXMLWrapper "ModifyOptionGroupResult"
              (\ s h x ->
                 ModifyOptionGroupResponse' Core.<$>
                   (x Core..@? "OptionGroup") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyOptionGroupResponse' smart constructor.
data ModifyOptionGroupResponse = ModifyOptionGroupResponse'
  { optionGroup :: Core.Maybe Types.OptionGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyOptionGroupResponse' value with any optional fields omitted.
mkModifyOptionGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyOptionGroupResponse
mkModifyOptionGroupResponse responseStatus
  = ModifyOptionGroupResponse'{optionGroup = Core.Nothing,
                               responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'optionGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mogrrsOptionGroup :: Lens.Lens' ModifyOptionGroupResponse (Core.Maybe Types.OptionGroup)
mogrrsOptionGroup = Lens.field @"optionGroup"
{-# INLINEABLE mogrrsOptionGroup #-}
{-# DEPRECATED optionGroup "Use generic-lens or generic-optics with 'optionGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mogrrsResponseStatus :: Lens.Lens' ModifyOptionGroupResponse Core.Int
mogrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mogrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
