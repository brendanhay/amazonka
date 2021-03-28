{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CopyDBParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified DB parameter group.
module Network.AWS.RDS.CopyDBParameterGroup
    (
    -- * Creating a request
      CopyDBParameterGroup (..)
    , mkCopyDBParameterGroup
    -- ** Request lenses
    , cdbpgSourceDBParameterGroupIdentifier
    , cdbpgTargetDBParameterGroupIdentifier
    , cdbpgTargetDBParameterGroupDescription
    , cdbpgTags

    -- * Destructuring the response
    , CopyDBParameterGroupResponse (..)
    , mkCopyDBParameterGroupResponse
    -- ** Response lenses
    , cdbpgrrsDBParameterGroup
    , cdbpgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkCopyDBParameterGroup' smart constructor.
data CopyDBParameterGroup = CopyDBParameterGroup'
  { sourceDBParameterGroupIdentifier :: Core.Text
    -- ^ The identifier or ARN for the source DB parameter group. For information about creating an ARN, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS> in the /Amazon RDS User Guide/ . 
--
-- Constraints:
--
--     * Must specify a valid DB parameter group.
--
--
--     * Must specify a valid DB parameter group identifier, for example @my-db-param-group@ , or a valid ARN.
--
--
  , targetDBParameterGroupIdentifier :: Core.Text
    -- ^ The identifier for the copied DB parameter group.
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
-- Example: @my-db-parameter-group@ 
  , targetDBParameterGroupDescription :: Core.Text
    -- ^ A description for the copied DB parameter group.
  , tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopyDBParameterGroup' value with any optional fields omitted.
mkCopyDBParameterGroup
    :: Core.Text -- ^ 'sourceDBParameterGroupIdentifier'
    -> Core.Text -- ^ 'targetDBParameterGroupIdentifier'
    -> Core.Text -- ^ 'targetDBParameterGroupDescription'
    -> CopyDBParameterGroup
mkCopyDBParameterGroup sourceDBParameterGroupIdentifier
  targetDBParameterGroupIdentifier targetDBParameterGroupDescription
  = CopyDBParameterGroup'{sourceDBParameterGroupIdentifier,
                          targetDBParameterGroupIdentifier,
                          targetDBParameterGroupDescription, tags = Core.Nothing}

-- | The identifier or ARN for the source DB parameter group. For information about creating an ARN, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS> in the /Amazon RDS User Guide/ . 
--
-- Constraints:
--
--     * Must specify a valid DB parameter group.
--
--
--     * Must specify a valid DB parameter group identifier, for example @my-db-param-group@ , or a valid ARN.
--
--
--
-- /Note:/ Consider using 'sourceDBParameterGroupIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbpgSourceDBParameterGroupIdentifier :: Lens.Lens' CopyDBParameterGroup Core.Text
cdbpgSourceDBParameterGroupIdentifier = Lens.field @"sourceDBParameterGroupIdentifier"
{-# INLINEABLE cdbpgSourceDBParameterGroupIdentifier #-}
{-# DEPRECATED sourceDBParameterGroupIdentifier "Use generic-lens or generic-optics with 'sourceDBParameterGroupIdentifier' instead"  #-}

-- | The identifier for the copied DB parameter group.
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
-- Example: @my-db-parameter-group@ 
--
-- /Note:/ Consider using 'targetDBParameterGroupIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbpgTargetDBParameterGroupIdentifier :: Lens.Lens' CopyDBParameterGroup Core.Text
cdbpgTargetDBParameterGroupIdentifier = Lens.field @"targetDBParameterGroupIdentifier"
{-# INLINEABLE cdbpgTargetDBParameterGroupIdentifier #-}
{-# DEPRECATED targetDBParameterGroupIdentifier "Use generic-lens or generic-optics with 'targetDBParameterGroupIdentifier' instead"  #-}

-- | A description for the copied DB parameter group.
--
-- /Note:/ Consider using 'targetDBParameterGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbpgTargetDBParameterGroupDescription :: Lens.Lens' CopyDBParameterGroup Core.Text
cdbpgTargetDBParameterGroupDescription = Lens.field @"targetDBParameterGroupDescription"
{-# INLINEABLE cdbpgTargetDBParameterGroupDescription #-}
{-# DEPRECATED targetDBParameterGroupDescription "Use generic-lens or generic-optics with 'targetDBParameterGroupDescription' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbpgTags :: Lens.Lens' CopyDBParameterGroup (Core.Maybe [Types.Tag])
cdbpgTags = Lens.field @"tags"
{-# INLINEABLE cdbpgTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CopyDBParameterGroup where
        toQuery CopyDBParameterGroup{..}
          = Core.toQueryPair "Action" ("CopyDBParameterGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "SourceDBParameterGroupIdentifier"
                sourceDBParameterGroupIdentifier
              Core.<>
              Core.toQueryPair "TargetDBParameterGroupIdentifier"
                targetDBParameterGroupIdentifier
              Core.<>
              Core.toQueryPair "TargetDBParameterGroupDescription"
                targetDBParameterGroupDescription
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "Tag") tags)

instance Core.ToHeaders CopyDBParameterGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CopyDBParameterGroup where
        type Rs CopyDBParameterGroup = CopyDBParameterGroupResponse
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
          = Response.receiveXMLWrapper "CopyDBParameterGroupResult"
              (\ s h x ->
                 CopyDBParameterGroupResponse' Core.<$>
                   (x Core..@? "DBParameterGroup") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCopyDBParameterGroupResponse' smart constructor.
data CopyDBParameterGroupResponse = CopyDBParameterGroupResponse'
  { dBParameterGroup :: Core.Maybe Types.DBParameterGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopyDBParameterGroupResponse' value with any optional fields omitted.
mkCopyDBParameterGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CopyDBParameterGroupResponse
mkCopyDBParameterGroupResponse responseStatus
  = CopyDBParameterGroupResponse'{dBParameterGroup = Core.Nothing,
                                  responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBParameterGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbpgrrsDBParameterGroup :: Lens.Lens' CopyDBParameterGroupResponse (Core.Maybe Types.DBParameterGroup)
cdbpgrrsDBParameterGroup = Lens.field @"dBParameterGroup"
{-# INLINEABLE cdbpgrrsDBParameterGroup #-}
{-# DEPRECATED dBParameterGroup "Use generic-lens or generic-optics with 'dBParameterGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbpgrrsResponseStatus :: Lens.Lens' CopyDBParameterGroupResponse Core.Int
cdbpgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdbpgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
