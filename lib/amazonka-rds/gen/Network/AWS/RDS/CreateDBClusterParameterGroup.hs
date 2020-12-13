{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBClusterParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB cluster parameter group.
--
-- Parameters in a DB cluster parameter group apply to all of the instances in a DB cluster.
-- A DB cluster parameter group is initially created with the default parameters for the database engine used by instances in the DB cluster. To provide custom values for any of the parameters, you must modify the group after creating it using @ModifyDBClusterParameterGroup@ . Once you've created a DB cluster parameter group, you need to associate it with your DB cluster using @ModifyDBCluster@ . When you associate a new DB cluster parameter group with a running DB cluster, you need to reboot the DB instances in the DB cluster without failover for the new DB cluster parameter group and associated settings to take effect.
-- /Important:/ After you create a DB cluster parameter group, you should wait at least 5 minutes before creating your first DB cluster that uses that DB cluster parameter group as the default parameter group. This allows Amazon RDS to fully complete the create action before the DB cluster parameter group is used as the default for a new DB cluster. This is especially important for parameters that are critical when creating the default database for a DB cluster, such as the character set for the default database defined by the @character_set_database@ parameter. You can use the /Parameter Groups/ option of the <https://console.aws.amazon.com/rds/ Amazon RDS console> or the @DescribeDBClusterParameters@ action to verify that your DB cluster parameter group has been created or modified.
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
module Network.AWS.RDS.CreateDBClusterParameterGroup
  ( -- * Creating a request
    CreateDBClusterParameterGroup (..),
    mkCreateDBClusterParameterGroup,

    -- ** Request lenses
    cdcpgDBParameterGroupFamily,
    cdcpgDBClusterParameterGroupName,
    cdcpgDescription,
    cdcpgTags,

    -- * Destructuring the response
    CreateDBClusterParameterGroupResponse (..),
    mkCreateDBClusterParameterGroupResponse,

    -- ** Response lenses
    cdbcpgrsDBClusterParameterGroup,
    cdbcpgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkCreateDBClusterParameterGroup' smart constructor.
data CreateDBClusterParameterGroup = CreateDBClusterParameterGroup'
  { -- | The DB cluster parameter group family name. A DB cluster parameter group can be associated with one and only one DB cluster parameter group family, and can be applied only to a DB cluster running a database engine and engine version compatible with that DB cluster parameter group family.
    --
    -- __Aurora MySQL__
    -- Example: @aurora5.6@ , @aurora-mysql5.7@
    -- __Aurora PostgreSQL__
    -- Example: @aurora-postgresql9.6@
    dbParameterGroupFamily :: Lude.Text,
    -- | The name of the DB cluster parameter group.
    --
    -- Constraints:
    --
    --     * Must match the name of an existing DB cluster parameter group.
    dbClusterParameterGroupName :: Lude.Text,
    -- | The description for the DB cluster parameter group.
    description :: Lude.Text,
    -- | Tags to assign to the DB cluster parameter group.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDBClusterParameterGroup' with the minimum fields required to make a request.
--
-- * 'dbParameterGroupFamily' - The DB cluster parameter group family name. A DB cluster parameter group can be associated with one and only one DB cluster parameter group family, and can be applied only to a DB cluster running a database engine and engine version compatible with that DB cluster parameter group family.
--
-- __Aurora MySQL__
-- Example: @aurora5.6@ , @aurora-mysql5.7@
-- __Aurora PostgreSQL__
-- Example: @aurora-postgresql9.6@
-- * 'dbClusterParameterGroupName' - The name of the DB cluster parameter group.
--
-- Constraints:
--
--     * Must match the name of an existing DB cluster parameter group.
--
--
-- * 'description' - The description for the DB cluster parameter group.
-- * 'tags' - Tags to assign to the DB cluster parameter group.
mkCreateDBClusterParameterGroup ::
  -- | 'dbParameterGroupFamily'
  Lude.Text ->
  -- | 'dbClusterParameterGroupName'
  Lude.Text ->
  -- | 'description'
  Lude.Text ->
  CreateDBClusterParameterGroup
mkCreateDBClusterParameterGroup
  pDBParameterGroupFamily_
  pDBClusterParameterGroupName_
  pDescription_ =
    CreateDBClusterParameterGroup'
      { dbParameterGroupFamily =
          pDBParameterGroupFamily_,
        dbClusterParameterGroupName = pDBClusterParameterGroupName_,
        description = pDescription_,
        tags = Lude.Nothing
      }

-- | The DB cluster parameter group family name. A DB cluster parameter group can be associated with one and only one DB cluster parameter group family, and can be applied only to a DB cluster running a database engine and engine version compatible with that DB cluster parameter group family.
--
-- __Aurora MySQL__
-- Example: @aurora5.6@ , @aurora-mysql5.7@
-- __Aurora PostgreSQL__
-- Example: @aurora-postgresql9.6@
--
-- /Note:/ Consider using 'dbParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcpgDBParameterGroupFamily :: Lens.Lens' CreateDBClusterParameterGroup Lude.Text
cdcpgDBParameterGroupFamily = Lens.lens (dbParameterGroupFamily :: CreateDBClusterParameterGroup -> Lude.Text) (\s a -> s {dbParameterGroupFamily = a} :: CreateDBClusterParameterGroup)
{-# DEPRECATED cdcpgDBParameterGroupFamily "Use generic-lens or generic-optics with 'dbParameterGroupFamily' instead." #-}

-- | The name of the DB cluster parameter group.
--
-- Constraints:
--
--     * Must match the name of an existing DB cluster parameter group.
--
--
--
-- /Note:/ Consider using 'dbClusterParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcpgDBClusterParameterGroupName :: Lens.Lens' CreateDBClusterParameterGroup Lude.Text
cdcpgDBClusterParameterGroupName = Lens.lens (dbClusterParameterGroupName :: CreateDBClusterParameterGroup -> Lude.Text) (\s a -> s {dbClusterParameterGroupName = a} :: CreateDBClusterParameterGroup)
{-# DEPRECATED cdcpgDBClusterParameterGroupName "Use generic-lens or generic-optics with 'dbClusterParameterGroupName' instead." #-}

-- | The description for the DB cluster parameter group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcpgDescription :: Lens.Lens' CreateDBClusterParameterGroup Lude.Text
cdcpgDescription = Lens.lens (description :: CreateDBClusterParameterGroup -> Lude.Text) (\s a -> s {description = a} :: CreateDBClusterParameterGroup)
{-# DEPRECATED cdcpgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Tags to assign to the DB cluster parameter group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcpgTags :: Lens.Lens' CreateDBClusterParameterGroup (Lude.Maybe [Tag])
cdcpgTags = Lens.lens (tags :: CreateDBClusterParameterGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateDBClusterParameterGroup)
{-# DEPRECATED cdcpgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateDBClusterParameterGroup where
  type
    Rs CreateDBClusterParameterGroup =
      CreateDBClusterParameterGroupResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "CreateDBClusterParameterGroupResult"
      ( \s h x ->
          CreateDBClusterParameterGroupResponse'
            Lude.<$> (x Lude..@? "DBClusterParameterGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDBClusterParameterGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateDBClusterParameterGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDBClusterParameterGroup where
  toQuery CreateDBClusterParameterGroup' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CreateDBClusterParameterGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBParameterGroupFamily" Lude.=: dbParameterGroupFamily,
        "DBClusterParameterGroupName" Lude.=: dbClusterParameterGroupName,
        "Description" Lude.=: description,
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags)
      ]

-- | /See:/ 'mkCreateDBClusterParameterGroupResponse' smart constructor.
data CreateDBClusterParameterGroupResponse = CreateDBClusterParameterGroupResponse'
  { dbClusterParameterGroup :: Lude.Maybe DBClusterParameterGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDBClusterParameterGroupResponse' with the minimum fields required to make a request.
--
-- * 'dbClusterParameterGroup' -
-- * 'responseStatus' - The response status code.
mkCreateDBClusterParameterGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDBClusterParameterGroupResponse
mkCreateDBClusterParameterGroupResponse pResponseStatus_ =
  CreateDBClusterParameterGroupResponse'
    { dbClusterParameterGroup =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbClusterParameterGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcpgrsDBClusterParameterGroup :: Lens.Lens' CreateDBClusterParameterGroupResponse (Lude.Maybe DBClusterParameterGroup)
cdbcpgrsDBClusterParameterGroup = Lens.lens (dbClusterParameterGroup :: CreateDBClusterParameterGroupResponse -> Lude.Maybe DBClusterParameterGroup) (\s a -> s {dbClusterParameterGroup = a} :: CreateDBClusterParameterGroupResponse)
{-# DEPRECATED cdbcpgrsDBClusterParameterGroup "Use generic-lens or generic-optics with 'dbClusterParameterGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcpgrsResponseStatus :: Lens.Lens' CreateDBClusterParameterGroupResponse Lude.Int
cdbcpgrsResponseStatus = Lens.lens (responseStatus :: CreateDBClusterParameterGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDBClusterParameterGroupResponse)
{-# DEPRECATED cdbcpgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
