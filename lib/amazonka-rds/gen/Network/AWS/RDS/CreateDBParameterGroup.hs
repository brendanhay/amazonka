{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB parameter group.
--
-- A DB parameter group is initially created with the default parameters for the database engine used by the DB instance. To provide custom values for any of the parameters, you must modify the group after creating it using /ModifyDBParameterGroup/ . Once you've created a DB parameter group, you need to associate it with your DB instance using /ModifyDBInstance/ . When you associate a new DB parameter group with a running DB instance, you need to reboot the DB instance without failover for the new DB parameter group and associated settings to take effect. 
-- /Important:/ After you create a DB parameter group, you should wait at least 5 minutes before creating your first DB instance that uses that DB parameter group as the default parameter group. This allows Amazon RDS to fully complete the create action before the parameter group is used as the default for a new DB instance. This is especially important for parameters that are critical when creating the default database for a DB instance, such as the character set for the default database defined by the @character_set_database@ parameter. You can use the /Parameter Groups/ option of the <https://console.aws.amazon.com/rds/ Amazon RDS console> or the /DescribeDBParameters/ command to verify that your DB parameter group has been created or modified.
module Network.AWS.RDS.CreateDBParameterGroup
    (
    -- * Creating a request
      CreateDBParameterGroup (..)
    , mkCreateDBParameterGroup
    -- ** Request lenses
    , cdbpgfDBParameterGroupName
    , cdbpgfDBParameterGroupFamily
    , cdbpgfDescription
    , cdbpgfTags

    -- * Destructuring the response
    , CreateDBParameterGroupResponse (..)
    , mkCreateDBParameterGroupResponse
    -- ** Response lenses
    , cdbpgrfrsDBParameterGroup
    , cdbpgrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkCreateDBParameterGroup' smart constructor.
data CreateDBParameterGroup = CreateDBParameterGroup'
  { dBParameterGroupName :: Core.Text
    -- ^ The name of the DB parameter group.
--
-- Constraints:
--
--     * Must be 1 to 255 letters, numbers, or hyphens.
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
  , dBParameterGroupFamily :: Core.Text
    -- ^ The DB parameter group family name. A DB parameter group can be associated with one and only one DB parameter group family, and can be applied only to a DB instance running a database engine and engine version compatible with that DB parameter group family.
--
-- To list all of the available parameter group families, use the following command:
-- @aws rds describe-db-engine-versions --query "DBEngineVersions[].DBParameterGroupFamily"@ 
  , description :: Core.Text
    -- ^ The description for the DB parameter group.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Tags to assign to the DB parameter group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDBParameterGroup' value with any optional fields omitted.
mkCreateDBParameterGroup
    :: Core.Text -- ^ 'dBParameterGroupName'
    -> Core.Text -- ^ 'dBParameterGroupFamily'
    -> Core.Text -- ^ 'description'
    -> CreateDBParameterGroup
mkCreateDBParameterGroup dBParameterGroupName
  dBParameterGroupFamily description
  = CreateDBParameterGroup'{dBParameterGroupName,
                            dBParameterGroupFamily, description, tags = Core.Nothing}

-- | The name of the DB parameter group.
--
-- Constraints:
--
--     * Must be 1 to 255 letters, numbers, or hyphens.
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
--
-- /Note:/ Consider using 'dBParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbpgfDBParameterGroupName :: Lens.Lens' CreateDBParameterGroup Core.Text
cdbpgfDBParameterGroupName = Lens.field @"dBParameterGroupName"
{-# INLINEABLE cdbpgfDBParameterGroupName #-}
{-# DEPRECATED dBParameterGroupName "Use generic-lens or generic-optics with 'dBParameterGroupName' instead"  #-}

-- | The DB parameter group family name. A DB parameter group can be associated with one and only one DB parameter group family, and can be applied only to a DB instance running a database engine and engine version compatible with that DB parameter group family.
--
-- To list all of the available parameter group families, use the following command:
-- @aws rds describe-db-engine-versions --query "DBEngineVersions[].DBParameterGroupFamily"@ 
--
-- /Note:/ Consider using 'dBParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbpgfDBParameterGroupFamily :: Lens.Lens' CreateDBParameterGroup Core.Text
cdbpgfDBParameterGroupFamily = Lens.field @"dBParameterGroupFamily"
{-# INLINEABLE cdbpgfDBParameterGroupFamily #-}
{-# DEPRECATED dBParameterGroupFamily "Use generic-lens or generic-optics with 'dBParameterGroupFamily' instead"  #-}

-- | The description for the DB parameter group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbpgfDescription :: Lens.Lens' CreateDBParameterGroup Core.Text
cdbpgfDescription = Lens.field @"description"
{-# INLINEABLE cdbpgfDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Tags to assign to the DB parameter group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbpgfTags :: Lens.Lens' CreateDBParameterGroup (Core.Maybe [Types.Tag])
cdbpgfTags = Lens.field @"tags"
{-# INLINEABLE cdbpgfTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateDBParameterGroup where
        toQuery CreateDBParameterGroup{..}
          = Core.toQueryPair "Action" ("CreateDBParameterGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "DBParameterGroupName" dBParameterGroupName
              Core.<>
              Core.toQueryPair "DBParameterGroupFamily" dBParameterGroupFamily
              Core.<> Core.toQueryPair "Description" description
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "Tag") tags)

instance Core.ToHeaders CreateDBParameterGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateDBParameterGroup where
        type Rs CreateDBParameterGroup = CreateDBParameterGroupResponse
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
          = Response.receiveXMLWrapper "CreateDBParameterGroupResult"
              (\ s h x ->
                 CreateDBParameterGroupResponse' Core.<$>
                   (x Core..@? "DBParameterGroup") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDBParameterGroupResponse' smart constructor.
data CreateDBParameterGroupResponse = CreateDBParameterGroupResponse'
  { dBParameterGroup :: Core.Maybe Types.DBParameterGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDBParameterGroupResponse' value with any optional fields omitted.
mkCreateDBParameterGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDBParameterGroupResponse
mkCreateDBParameterGroupResponse responseStatus
  = CreateDBParameterGroupResponse'{dBParameterGroup = Core.Nothing,
                                    responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBParameterGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbpgrfrsDBParameterGroup :: Lens.Lens' CreateDBParameterGroupResponse (Core.Maybe Types.DBParameterGroup)
cdbpgrfrsDBParameterGroup = Lens.field @"dBParameterGroup"
{-# INLINEABLE cdbpgrfrsDBParameterGroup #-}
{-# DEPRECATED dBParameterGroup "Use generic-lens or generic-optics with 'dBParameterGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbpgrfrsResponseStatus :: Lens.Lens' CreateDBParameterGroupResponse Core.Int
cdbpgrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdbpgrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
