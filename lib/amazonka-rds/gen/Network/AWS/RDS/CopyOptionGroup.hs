{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CopyOptionGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified option group.
module Network.AWS.RDS.CopyOptionGroup
  ( -- * Creating a request
    CopyOptionGroup (..),
    mkCopyOptionGroup,

    -- ** Request lenses
    cogfSourceOptionGroupIdentifier,
    cogfTargetOptionGroupIdentifier,
    cogfTargetOptionGroupDescription,
    cogfTags,

    -- * Destructuring the response
    CopyOptionGroupResponse (..),
    mkCopyOptionGroupResponse,

    -- ** Response lenses
    cogrfrsOptionGroup,
    cogrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkCopyOptionGroup' smart constructor.
data CopyOptionGroup = CopyOptionGroup'
  { -- | The identifier for the source option group.
    --
    -- Constraints:
    --
    --     * Must specify a valid option group.
    sourceOptionGroupIdentifier :: Types.String,
    -- | The identifier for the copied option group.
    --
    -- Constraints:
    --
    --     * Can't be null, empty, or blank
    --
    --
    --     * Must contain from 1 to 255 letters, numbers, or hyphens
    --
    --
    --     * First character must be a letter
    --
    --
    --     * Can't end with a hyphen or contain two consecutive hyphens
    --
    --
    -- Example: @my-option-group@
    targetOptionGroupIdentifier :: Types.String,
    -- | The description for the copied option group.
    targetOptionGroupDescription :: Types.String,
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopyOptionGroup' value with any optional fields omitted.
mkCopyOptionGroup ::
  -- | 'sourceOptionGroupIdentifier'
  Types.String ->
  -- | 'targetOptionGroupIdentifier'
  Types.String ->
  -- | 'targetOptionGroupDescription'
  Types.String ->
  CopyOptionGroup
mkCopyOptionGroup
  sourceOptionGroupIdentifier
  targetOptionGroupIdentifier
  targetOptionGroupDescription =
    CopyOptionGroup'
      { sourceOptionGroupIdentifier,
        targetOptionGroupIdentifier,
        targetOptionGroupDescription,
        tags = Core.Nothing
      }

-- | The identifier for the source option group.
--
-- Constraints:
--
--     * Must specify a valid option group.
--
--
--
-- /Note:/ Consider using 'sourceOptionGroupIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cogfSourceOptionGroupIdentifier :: Lens.Lens' CopyOptionGroup Types.String
cogfSourceOptionGroupIdentifier = Lens.field @"sourceOptionGroupIdentifier"
{-# DEPRECATED cogfSourceOptionGroupIdentifier "Use generic-lens or generic-optics with 'sourceOptionGroupIdentifier' instead." #-}

-- | The identifier for the copied option group.
--
-- Constraints:
--
--     * Can't be null, empty, or blank
--
--
--     * Must contain from 1 to 255 letters, numbers, or hyphens
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
-- Example: @my-option-group@
--
-- /Note:/ Consider using 'targetOptionGroupIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cogfTargetOptionGroupIdentifier :: Lens.Lens' CopyOptionGroup Types.String
cogfTargetOptionGroupIdentifier = Lens.field @"targetOptionGroupIdentifier"
{-# DEPRECATED cogfTargetOptionGroupIdentifier "Use generic-lens or generic-optics with 'targetOptionGroupIdentifier' instead." #-}

-- | The description for the copied option group.
--
-- /Note:/ Consider using 'targetOptionGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cogfTargetOptionGroupDescription :: Lens.Lens' CopyOptionGroup Types.String
cogfTargetOptionGroupDescription = Lens.field @"targetOptionGroupDescription"
{-# DEPRECATED cogfTargetOptionGroupDescription "Use generic-lens or generic-optics with 'targetOptionGroupDescription' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cogfTags :: Lens.Lens' CopyOptionGroup (Core.Maybe [Types.Tag])
cogfTags = Lens.field @"tags"
{-# DEPRECATED cogfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.AWSRequest CopyOptionGroup where
  type Rs CopyOptionGroup = CopyOptionGroupResponse
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
            ( Core.pure ("Action", "CopyOptionGroup")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> ( Core.toQueryValue
                            "SourceOptionGroupIdentifier"
                            sourceOptionGroupIdentifier
                        )
                Core.<> ( Core.toQueryValue
                            "TargetOptionGroupIdentifier"
                            targetOptionGroupIdentifier
                        )
                Core.<> ( Core.toQueryValue
                            "TargetOptionGroupDescription"
                            targetOptionGroupDescription
                        )
                Core.<> (Core.toQueryValue "Tags" (Core.toQueryList "Tag" Core.<$> tags))
            )
      }
  response =
    Response.receiveXMLWrapper
      "CopyOptionGroupResult"
      ( \s h x ->
          CopyOptionGroupResponse'
            Core.<$> (x Core..@? "OptionGroup") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCopyOptionGroupResponse' smart constructor.
data CopyOptionGroupResponse = CopyOptionGroupResponse'
  { optionGroup :: Core.Maybe Types.OptionGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopyOptionGroupResponse' value with any optional fields omitted.
mkCopyOptionGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CopyOptionGroupResponse
mkCopyOptionGroupResponse responseStatus =
  CopyOptionGroupResponse'
    { optionGroup = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'optionGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cogrfrsOptionGroup :: Lens.Lens' CopyOptionGroupResponse (Core.Maybe Types.OptionGroup)
cogrfrsOptionGroup = Lens.field @"optionGroup"
{-# DEPRECATED cogrfrsOptionGroup "Use generic-lens or generic-optics with 'optionGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cogrfrsResponseStatus :: Lens.Lens' CopyOptionGroupResponse Core.Int
cogrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cogrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
