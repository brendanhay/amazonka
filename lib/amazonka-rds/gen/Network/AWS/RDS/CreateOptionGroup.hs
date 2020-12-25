{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateOptionGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new option group. You can create up to 20 option groups.
module Network.AWS.RDS.CreateOptionGroup
  ( -- * Creating a request
    CreateOptionGroup (..),
    mkCreateOptionGroup,

    -- ** Request lenses
    cogOptionGroupName,
    cogEngineName,
    cogMajorEngineVersion,
    cogOptionGroupDescription,
    cogTags,

    -- * Destructuring the response
    CreateOptionGroupResponse (..),
    mkCreateOptionGroupResponse,

    -- ** Response lenses
    cogrrsOptionGroup,
    cogrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkCreateOptionGroup' smart constructor.
data CreateOptionGroup = CreateOptionGroup'
  { -- | Specifies the name of the option group to be created.
    --
    -- Constraints:
    --
    --     * Must be 1 to 255 letters, numbers, or hyphens
    --
    --
    --     * First character must be a letter
    --
    --
    --     * Can't end with a hyphen or contain two consecutive hyphens
    --
    --
    -- Example: @myoptiongroup@
    optionGroupName :: Types.String,
    -- | Specifies the name of the engine that this option group should be associated with.
    engineName :: Types.String,
    -- | Specifies the major version of the engine that this option group should be associated with.
    majorEngineVersion :: Types.String,
    -- | The description of the option group.
    optionGroupDescription :: Types.String,
    -- | Tags to assign to the option group.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateOptionGroup' value with any optional fields omitted.
mkCreateOptionGroup ::
  -- | 'optionGroupName'
  Types.String ->
  -- | 'engineName'
  Types.String ->
  -- | 'majorEngineVersion'
  Types.String ->
  -- | 'optionGroupDescription'
  Types.String ->
  CreateOptionGroup
mkCreateOptionGroup
  optionGroupName
  engineName
  majorEngineVersion
  optionGroupDescription =
    CreateOptionGroup'
      { optionGroupName,
        engineName,
        majorEngineVersion,
        optionGroupDescription,
        tags = Core.Nothing
      }

-- | Specifies the name of the option group to be created.
--
-- Constraints:
--
--     * Must be 1 to 255 letters, numbers, or hyphens
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
-- Example: @myoptiongroup@
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cogOptionGroupName :: Lens.Lens' CreateOptionGroup Types.String
cogOptionGroupName = Lens.field @"optionGroupName"
{-# DEPRECATED cogOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

-- | Specifies the name of the engine that this option group should be associated with.
--
-- /Note:/ Consider using 'engineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cogEngineName :: Lens.Lens' CreateOptionGroup Types.String
cogEngineName = Lens.field @"engineName"
{-# DEPRECATED cogEngineName "Use generic-lens or generic-optics with 'engineName' instead." #-}

-- | Specifies the major version of the engine that this option group should be associated with.
--
-- /Note:/ Consider using 'majorEngineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cogMajorEngineVersion :: Lens.Lens' CreateOptionGroup Types.String
cogMajorEngineVersion = Lens.field @"majorEngineVersion"
{-# DEPRECATED cogMajorEngineVersion "Use generic-lens or generic-optics with 'majorEngineVersion' instead." #-}

-- | The description of the option group.
--
-- /Note:/ Consider using 'optionGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cogOptionGroupDescription :: Lens.Lens' CreateOptionGroup Types.String
cogOptionGroupDescription = Lens.field @"optionGroupDescription"
{-# DEPRECATED cogOptionGroupDescription "Use generic-lens or generic-optics with 'optionGroupDescription' instead." #-}

-- | Tags to assign to the option group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cogTags :: Lens.Lens' CreateOptionGroup (Core.Maybe [Types.Tag])
cogTags = Lens.field @"tags"
{-# DEPRECATED cogTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.AWSRequest CreateOptionGroup where
  type Rs CreateOptionGroup = CreateOptionGroupResponse
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
            ( Core.pure ("Action", "CreateOptionGroup")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "OptionGroupName" optionGroupName)
                Core.<> (Core.toQueryValue "EngineName" engineName)
                Core.<> (Core.toQueryValue "MajorEngineVersion" majorEngineVersion)
                Core.<> (Core.toQueryValue "OptionGroupDescription" optionGroupDescription)
                Core.<> (Core.toQueryValue "Tags" (Core.toQueryList "Tag" Core.<$> tags))
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateOptionGroupResult"
      ( \s h x ->
          CreateOptionGroupResponse'
            Core.<$> (x Core..@? "OptionGroup") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateOptionGroupResponse' smart constructor.
data CreateOptionGroupResponse = CreateOptionGroupResponse'
  { optionGroup :: Core.Maybe Types.OptionGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateOptionGroupResponse' value with any optional fields omitted.
mkCreateOptionGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateOptionGroupResponse
mkCreateOptionGroupResponse responseStatus =
  CreateOptionGroupResponse'
    { optionGroup = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'optionGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cogrrsOptionGroup :: Lens.Lens' CreateOptionGroupResponse (Core.Maybe Types.OptionGroup)
cogrrsOptionGroup = Lens.field @"optionGroup"
{-# DEPRECATED cogrrsOptionGroup "Use generic-lens or generic-optics with 'optionGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cogrrsResponseStatus :: Lens.Lens' CreateOptionGroupResponse Core.Int
cogrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cogrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
