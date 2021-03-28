{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CopyOptionGroup (..)
    , mkCopyOptionGroup
    -- ** Request lenses
    , cogfSourceOptionGroupIdentifier
    , cogfTargetOptionGroupIdentifier
    , cogfTargetOptionGroupDescription
    , cogfTags

    -- * Destructuring the response
    , CopyOptionGroupResponse (..)
    , mkCopyOptionGroupResponse
    -- ** Response lenses
    , cogrfrsOptionGroup
    , cogrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkCopyOptionGroup' smart constructor.
data CopyOptionGroup = CopyOptionGroup'
  { sourceOptionGroupIdentifier :: Core.Text
    -- ^ The identifier for the source option group. 
--
-- Constraints:
--
--     * Must specify a valid option group.
--
--
  , targetOptionGroupIdentifier :: Core.Text
    -- ^ The identifier for the copied option group.
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
  , targetOptionGroupDescription :: Core.Text
    -- ^ The description for the copied option group.
  , tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopyOptionGroup' value with any optional fields omitted.
mkCopyOptionGroup
    :: Core.Text -- ^ 'sourceOptionGroupIdentifier'
    -> Core.Text -- ^ 'targetOptionGroupIdentifier'
    -> Core.Text -- ^ 'targetOptionGroupDescription'
    -> CopyOptionGroup
mkCopyOptionGroup sourceOptionGroupIdentifier
  targetOptionGroupIdentifier targetOptionGroupDescription
  = CopyOptionGroup'{sourceOptionGroupIdentifier,
                     targetOptionGroupIdentifier, targetOptionGroupDescription,
                     tags = Core.Nothing}

-- | The identifier for the source option group. 
--
-- Constraints:
--
--     * Must specify a valid option group.
--
--
--
-- /Note:/ Consider using 'sourceOptionGroupIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cogfSourceOptionGroupIdentifier :: Lens.Lens' CopyOptionGroup Core.Text
cogfSourceOptionGroupIdentifier = Lens.field @"sourceOptionGroupIdentifier"
{-# INLINEABLE cogfSourceOptionGroupIdentifier #-}
{-# DEPRECATED sourceOptionGroupIdentifier "Use generic-lens or generic-optics with 'sourceOptionGroupIdentifier' instead"  #-}

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
cogfTargetOptionGroupIdentifier :: Lens.Lens' CopyOptionGroup Core.Text
cogfTargetOptionGroupIdentifier = Lens.field @"targetOptionGroupIdentifier"
{-# INLINEABLE cogfTargetOptionGroupIdentifier #-}
{-# DEPRECATED targetOptionGroupIdentifier "Use generic-lens or generic-optics with 'targetOptionGroupIdentifier' instead"  #-}

-- | The description for the copied option group.
--
-- /Note:/ Consider using 'targetOptionGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cogfTargetOptionGroupDescription :: Lens.Lens' CopyOptionGroup Core.Text
cogfTargetOptionGroupDescription = Lens.field @"targetOptionGroupDescription"
{-# INLINEABLE cogfTargetOptionGroupDescription #-}
{-# DEPRECATED targetOptionGroupDescription "Use generic-lens or generic-optics with 'targetOptionGroupDescription' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cogfTags :: Lens.Lens' CopyOptionGroup (Core.Maybe [Types.Tag])
cogfTags = Lens.field @"tags"
{-# INLINEABLE cogfTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CopyOptionGroup where
        toQuery CopyOptionGroup{..}
          = Core.toQueryPair "Action" ("CopyOptionGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "SourceOptionGroupIdentifier"
                sourceOptionGroupIdentifier
              Core.<>
              Core.toQueryPair "TargetOptionGroupIdentifier"
                targetOptionGroupIdentifier
              Core.<>
              Core.toQueryPair "TargetOptionGroupDescription"
                targetOptionGroupDescription
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "Tag") tags)

instance Core.ToHeaders CopyOptionGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CopyOptionGroup where
        type Rs CopyOptionGroup = CopyOptionGroupResponse
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
          = Response.receiveXMLWrapper "CopyOptionGroupResult"
              (\ s h x ->
                 CopyOptionGroupResponse' Core.<$>
                   (x Core..@? "OptionGroup") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCopyOptionGroupResponse' smart constructor.
data CopyOptionGroupResponse = CopyOptionGroupResponse'
  { optionGroup :: Core.Maybe Types.OptionGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopyOptionGroupResponse' value with any optional fields omitted.
mkCopyOptionGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CopyOptionGroupResponse
mkCopyOptionGroupResponse responseStatus
  = CopyOptionGroupResponse'{optionGroup = Core.Nothing,
                             responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'optionGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cogrfrsOptionGroup :: Lens.Lens' CopyOptionGroupResponse (Core.Maybe Types.OptionGroup)
cogrfrsOptionGroup = Lens.field @"optionGroup"
{-# INLINEABLE cogrfrsOptionGroup #-}
{-# DEPRECATED optionGroup "Use generic-lens or generic-optics with 'optionGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cogrfrsResponseStatus :: Lens.Lens' CopyOptionGroupResponse Core.Int
cogrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cogrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
