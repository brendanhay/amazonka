{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateOptionGroup (..)
    , mkCreateOptionGroup
    -- ** Request lenses
    , cogOptionGroupName
    , cogEngineName
    , cogMajorEngineVersion
    , cogOptionGroupDescription
    , cogTags

    -- * Destructuring the response
    , CreateOptionGroupResponse (..)
    , mkCreateOptionGroupResponse
    -- ** Response lenses
    , cogrrsOptionGroup
    , cogrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkCreateOptionGroup' smart constructor.
data CreateOptionGroup = CreateOptionGroup'
  { optionGroupName :: Core.Text
    -- ^ Specifies the name of the option group to be created.
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
  , engineName :: Core.Text
    -- ^ Specifies the name of the engine that this option group should be associated with.
  , majorEngineVersion :: Core.Text
    -- ^ Specifies the major version of the engine that this option group should be associated with.
  , optionGroupDescription :: Core.Text
    -- ^ The description of the option group.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Tags to assign to the option group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateOptionGroup' value with any optional fields omitted.
mkCreateOptionGroup
    :: Core.Text -- ^ 'optionGroupName'
    -> Core.Text -- ^ 'engineName'
    -> Core.Text -- ^ 'majorEngineVersion'
    -> Core.Text -- ^ 'optionGroupDescription'
    -> CreateOptionGroup
mkCreateOptionGroup optionGroupName engineName majorEngineVersion
  optionGroupDescription
  = CreateOptionGroup'{optionGroupName, engineName,
                       majorEngineVersion, optionGroupDescription, tags = Core.Nothing}

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
cogOptionGroupName :: Lens.Lens' CreateOptionGroup Core.Text
cogOptionGroupName = Lens.field @"optionGroupName"
{-# INLINEABLE cogOptionGroupName #-}
{-# DEPRECATED optionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead"  #-}

-- | Specifies the name of the engine that this option group should be associated with.
--
-- /Note:/ Consider using 'engineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cogEngineName :: Lens.Lens' CreateOptionGroup Core.Text
cogEngineName = Lens.field @"engineName"
{-# INLINEABLE cogEngineName #-}
{-# DEPRECATED engineName "Use generic-lens or generic-optics with 'engineName' instead"  #-}

-- | Specifies the major version of the engine that this option group should be associated with.
--
-- /Note:/ Consider using 'majorEngineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cogMajorEngineVersion :: Lens.Lens' CreateOptionGroup Core.Text
cogMajorEngineVersion = Lens.field @"majorEngineVersion"
{-# INLINEABLE cogMajorEngineVersion #-}
{-# DEPRECATED majorEngineVersion "Use generic-lens or generic-optics with 'majorEngineVersion' instead"  #-}

-- | The description of the option group.
--
-- /Note:/ Consider using 'optionGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cogOptionGroupDescription :: Lens.Lens' CreateOptionGroup Core.Text
cogOptionGroupDescription = Lens.field @"optionGroupDescription"
{-# INLINEABLE cogOptionGroupDescription #-}
{-# DEPRECATED optionGroupDescription "Use generic-lens or generic-optics with 'optionGroupDescription' instead"  #-}

-- | Tags to assign to the option group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cogTags :: Lens.Lens' CreateOptionGroup (Core.Maybe [Types.Tag])
cogTags = Lens.field @"tags"
{-# INLINEABLE cogTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateOptionGroup where
        toQuery CreateOptionGroup{..}
          = Core.toQueryPair "Action" ("CreateOptionGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<> Core.toQueryPair "OptionGroupName" optionGroupName
              Core.<> Core.toQueryPair "EngineName" engineName
              Core.<> Core.toQueryPair "MajorEngineVersion" majorEngineVersion
              Core.<>
              Core.toQueryPair "OptionGroupDescription" optionGroupDescription
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "Tag") tags)

instance Core.ToHeaders CreateOptionGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateOptionGroup where
        type Rs CreateOptionGroup = CreateOptionGroupResponse
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
          = Response.receiveXMLWrapper "CreateOptionGroupResult"
              (\ s h x ->
                 CreateOptionGroupResponse' Core.<$>
                   (x Core..@? "OptionGroup") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateOptionGroupResponse' smart constructor.
data CreateOptionGroupResponse = CreateOptionGroupResponse'
  { optionGroup :: Core.Maybe Types.OptionGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateOptionGroupResponse' value with any optional fields omitted.
mkCreateOptionGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateOptionGroupResponse
mkCreateOptionGroupResponse responseStatus
  = CreateOptionGroupResponse'{optionGroup = Core.Nothing,
                               responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'optionGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cogrrsOptionGroup :: Lens.Lens' CreateOptionGroupResponse (Core.Maybe Types.OptionGroup)
cogrrsOptionGroup = Lens.field @"optionGroup"
{-# INLINEABLE cogrrsOptionGroup #-}
{-# DEPRECATED optionGroup "Use generic-lens or generic-optics with 'optionGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cogrrsResponseStatus :: Lens.Lens' CreateOptionGroupResponse Core.Int
cogrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cogrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
