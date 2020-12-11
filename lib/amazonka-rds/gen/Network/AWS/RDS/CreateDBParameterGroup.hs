{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateDBParameterGroup (..),
    mkCreateDBParameterGroup,

    -- ** Request lenses
    cdbpgTags,
    cdbpgDBParameterGroupName,
    cdbpgDBParameterGroupFamily,
    cdbpgDescription,

    -- * Destructuring the response
    CreateDBParameterGroupResponse (..),
    mkCreateDBParameterGroupResponse,

    -- ** Response lenses
    cdpgrsDBParameterGroup,
    cdpgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkCreateDBParameterGroup' smart constructor.
data CreateDBParameterGroup = CreateDBParameterGroup'
  { tags ::
      Lude.Maybe [Tag],
    dbParameterGroupName :: Lude.Text,
    dbParameterGroupFamily :: Lude.Text,
    description :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDBParameterGroup' with the minimum fields required to make a request.
--
-- * 'dbParameterGroupFamily' - The DB parameter group family name. A DB parameter group can be associated with one and only one DB parameter group family, and can be applied only to a DB instance running a database engine and engine version compatible with that DB parameter group family.
--
-- To list all of the available parameter group families, use the following command:
-- @aws rds describe-db-engine-versions --query "DBEngineVersions[].DBParameterGroupFamily"@
-- * 'dbParameterGroupName' - The name of the DB parameter group.
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
-- * 'description' - The description for the DB parameter group.
-- * 'tags' - Tags to assign to the DB parameter group.
mkCreateDBParameterGroup ::
  -- | 'dbParameterGroupName'
  Lude.Text ->
  -- | 'dbParameterGroupFamily'
  Lude.Text ->
  -- | 'description'
  Lude.Text ->
  CreateDBParameterGroup
mkCreateDBParameterGroup
  pDBParameterGroupName_
  pDBParameterGroupFamily_
  pDescription_ =
    CreateDBParameterGroup'
      { tags = Lude.Nothing,
        dbParameterGroupName = pDBParameterGroupName_,
        dbParameterGroupFamily = pDBParameterGroupFamily_,
        description = pDescription_
      }

-- | Tags to assign to the DB parameter group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbpgTags :: Lens.Lens' CreateDBParameterGroup (Lude.Maybe [Tag])
cdbpgTags = Lens.lens (tags :: CreateDBParameterGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateDBParameterGroup)
{-# DEPRECATED cdbpgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

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
-- /Note:/ Consider using 'dbParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbpgDBParameterGroupName :: Lens.Lens' CreateDBParameterGroup Lude.Text
cdbpgDBParameterGroupName = Lens.lens (dbParameterGroupName :: CreateDBParameterGroup -> Lude.Text) (\s a -> s {dbParameterGroupName = a} :: CreateDBParameterGroup)
{-# DEPRECATED cdbpgDBParameterGroupName "Use generic-lens or generic-optics with 'dbParameterGroupName' instead." #-}

-- | The DB parameter group family name. A DB parameter group can be associated with one and only one DB parameter group family, and can be applied only to a DB instance running a database engine and engine version compatible with that DB parameter group family.
--
-- To list all of the available parameter group families, use the following command:
-- @aws rds describe-db-engine-versions --query "DBEngineVersions[].DBParameterGroupFamily"@
--
-- /Note:/ Consider using 'dbParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbpgDBParameterGroupFamily :: Lens.Lens' CreateDBParameterGroup Lude.Text
cdbpgDBParameterGroupFamily = Lens.lens (dbParameterGroupFamily :: CreateDBParameterGroup -> Lude.Text) (\s a -> s {dbParameterGroupFamily = a} :: CreateDBParameterGroup)
{-# DEPRECATED cdbpgDBParameterGroupFamily "Use generic-lens or generic-optics with 'dbParameterGroupFamily' instead." #-}

-- | The description for the DB parameter group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbpgDescription :: Lens.Lens' CreateDBParameterGroup Lude.Text
cdbpgDescription = Lens.lens (description :: CreateDBParameterGroup -> Lude.Text) (\s a -> s {description = a} :: CreateDBParameterGroup)
{-# DEPRECATED cdbpgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest CreateDBParameterGroup where
  type Rs CreateDBParameterGroup = CreateDBParameterGroupResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "CreateDBParameterGroupResult"
      ( \s h x ->
          CreateDBParameterGroupResponse'
            Lude.<$> (x Lude..@? "DBParameterGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDBParameterGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateDBParameterGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDBParameterGroup where
  toQuery CreateDBParameterGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateDBParameterGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags),
        "DBParameterGroupName" Lude.=: dbParameterGroupName,
        "DBParameterGroupFamily" Lude.=: dbParameterGroupFamily,
        "Description" Lude.=: description
      ]

-- | /See:/ 'mkCreateDBParameterGroupResponse' smart constructor.
data CreateDBParameterGroupResponse = CreateDBParameterGroupResponse'
  { dbParameterGroup ::
      Lude.Maybe DBParameterGroup,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDBParameterGroupResponse' with the minimum fields required to make a request.
--
-- * 'dbParameterGroup' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkCreateDBParameterGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDBParameterGroupResponse
mkCreateDBParameterGroupResponse pResponseStatus_ =
  CreateDBParameterGroupResponse'
    { dbParameterGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbParameterGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpgrsDBParameterGroup :: Lens.Lens' CreateDBParameterGroupResponse (Lude.Maybe DBParameterGroup)
cdpgrsDBParameterGroup = Lens.lens (dbParameterGroup :: CreateDBParameterGroupResponse -> Lude.Maybe DBParameterGroup) (\s a -> s {dbParameterGroup = a} :: CreateDBParameterGroupResponse)
{-# DEPRECATED cdpgrsDBParameterGroup "Use generic-lens or generic-optics with 'dbParameterGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpgrsResponseStatus :: Lens.Lens' CreateDBParameterGroupResponse Lude.Int
cdpgrsResponseStatus = Lens.lens (responseStatus :: CreateDBParameterGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDBParameterGroupResponse)
{-# DEPRECATED cdpgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
