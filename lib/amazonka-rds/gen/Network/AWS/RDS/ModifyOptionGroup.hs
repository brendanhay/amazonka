{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ModifyOptionGroup (..),
    mkModifyOptionGroup,

    -- ** Request lenses
    mogOptionGroupName,
    mogApplyImmediately,
    mogOptionsToInclude,
    mogOptionsToRemove,

    -- * Destructuring the response
    ModifyOptionGroupResponse (..),
    mkModifyOptionGroupResponse,

    -- ** Response lenses
    mogrrsOptionGroup,
    mogrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkModifyOptionGroup' smart constructor.
data ModifyOptionGroup = ModifyOptionGroup'
  { -- | The name of the option group to be modified.
    --
    -- Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group, and that option group can't be removed from a DB instance once it is associated with a DB instance
    optionGroupName :: Types.OptionGroupName,
    -- | A value that indicates whether to apply the change immediately or during the next maintenance window for each instance associated with the option group.
    applyImmediately :: Core.Maybe Core.Bool,
    -- | Options in this list are added to the option group or, if already present, the specified configuration is used to update the existing configuration.
    optionsToInclude :: Core.Maybe [Types.OptionConfiguration],
    -- | Options in this list are removed from the option group.
    optionsToRemove :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyOptionGroup' value with any optional fields omitted.
mkModifyOptionGroup ::
  -- | 'optionGroupName'
  Types.OptionGroupName ->
  ModifyOptionGroup
mkModifyOptionGroup optionGroupName =
  ModifyOptionGroup'
    { optionGroupName,
      applyImmediately = Core.Nothing,
      optionsToInclude = Core.Nothing,
      optionsToRemove = Core.Nothing
    }

-- | The name of the option group to be modified.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group, and that option group can't be removed from a DB instance once it is associated with a DB instance
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mogOptionGroupName :: Lens.Lens' ModifyOptionGroup Types.OptionGroupName
mogOptionGroupName = Lens.field @"optionGroupName"
{-# DEPRECATED mogOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

-- | A value that indicates whether to apply the change immediately or during the next maintenance window for each instance associated with the option group.
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mogApplyImmediately :: Lens.Lens' ModifyOptionGroup (Core.Maybe Core.Bool)
mogApplyImmediately = Lens.field @"applyImmediately"
{-# DEPRECATED mogApplyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead." #-}

-- | Options in this list are added to the option group or, if already present, the specified configuration is used to update the existing configuration.
--
-- /Note:/ Consider using 'optionsToInclude' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mogOptionsToInclude :: Lens.Lens' ModifyOptionGroup (Core.Maybe [Types.OptionConfiguration])
mogOptionsToInclude = Lens.field @"optionsToInclude"
{-# DEPRECATED mogOptionsToInclude "Use generic-lens or generic-optics with 'optionsToInclude' instead." #-}

-- | Options in this list are removed from the option group.
--
-- /Note:/ Consider using 'optionsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mogOptionsToRemove :: Lens.Lens' ModifyOptionGroup (Core.Maybe [Types.String])
mogOptionsToRemove = Lens.field @"optionsToRemove"
{-# DEPRECATED mogOptionsToRemove "Use generic-lens or generic-optics with 'optionsToRemove' instead." #-}

instance Core.AWSRequest ModifyOptionGroup where
  type Rs ModifyOptionGroup = ModifyOptionGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ModifyOptionGroup")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "OptionGroupName" optionGroupName)
                Core.<> (Core.toQueryValue "ApplyImmediately" Core.<$> applyImmediately)
                Core.<> ( Core.toQueryValue
                            "OptionsToInclude"
                            ( Core.toQueryList "OptionConfiguration"
                                Core.<$> optionsToInclude
                            )
                        )
                Core.<> ( Core.toQueryValue
                            "OptionsToRemove"
                            (Core.toQueryList "member" Core.<$> optionsToRemove)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ModifyOptionGroupResult"
      ( \s h x ->
          ModifyOptionGroupResponse'
            Core.<$> (x Core..@? "OptionGroup") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyOptionGroupResponse' smart constructor.
data ModifyOptionGroupResponse = ModifyOptionGroupResponse'
  { optionGroup :: Core.Maybe Types.OptionGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyOptionGroupResponse' value with any optional fields omitted.
mkModifyOptionGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyOptionGroupResponse
mkModifyOptionGroupResponse responseStatus =
  ModifyOptionGroupResponse'
    { optionGroup = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'optionGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mogrrsOptionGroup :: Lens.Lens' ModifyOptionGroupResponse (Core.Maybe Types.OptionGroup)
mogrrsOptionGroup = Lens.field @"optionGroup"
{-# DEPRECATED mogrrsOptionGroup "Use generic-lens or generic-optics with 'optionGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mogrrsResponseStatus :: Lens.Lens' ModifyOptionGroupResponse Core.Int
mogrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mogrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
