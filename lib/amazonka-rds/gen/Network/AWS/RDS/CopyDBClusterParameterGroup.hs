{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CopyDBClusterParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified DB cluster parameter group.
module Network.AWS.RDS.CopyDBClusterParameterGroup
  ( -- * Creating a request
    CopyDBClusterParameterGroup (..),
    mkCopyDBClusterParameterGroup,

    -- ** Request lenses
    cdbcpgSourceDBClusterParameterGroupIdentifier,
    cdbcpgTargetDBClusterParameterGroupIdentifier,
    cdbcpgTargetDBClusterParameterGroupDescription,
    cdbcpgTags,

    -- * Destructuring the response
    CopyDBClusterParameterGroupResponse (..),
    mkCopyDBClusterParameterGroupResponse,

    -- ** Response lenses
    cdcpgrsDBClusterParameterGroup,
    cdcpgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCopyDBClusterParameterGroup' smart constructor.
data CopyDBClusterParameterGroup = CopyDBClusterParameterGroup'
  { -- | The identifier or Amazon Resource Name (ARN) for the source DB cluster parameter group. For information about creating an ARN, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS> in the /Amazon Aurora User Guide/ .
    --
    -- Constraints:
    --
    --     * Must specify a valid DB cluster parameter group.
    --
    --
    --     * If the source DB cluster parameter group is in the same AWS Region as the copy, specify a valid DB parameter group identifier, for example @my-db-cluster-param-group@ , or a valid ARN.
    --
    --
    --     * If the source DB parameter group is in a different AWS Region than the copy, specify a valid DB cluster parameter group ARN, for example @arn:aws:rds:us-east-1:123456789012:cluster-pg:custom-cluster-group1@ .
    sourceDBClusterParameterGroupIdentifier :: Lude.Text,
    -- | The identifier for the copied DB cluster parameter group.
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
    -- Example: @my-cluster-param-group1@
    targetDBClusterParameterGroupIdentifier :: Lude.Text,
    -- | A description for the copied DB cluster parameter group.
    targetDBClusterParameterGroupDescription :: Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CopyDBClusterParameterGroup' with the minimum fields required to make a request.
--
-- * 'sourceDBClusterParameterGroupIdentifier' - The identifier or Amazon Resource Name (ARN) for the source DB cluster parameter group. For information about creating an ARN, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS> in the /Amazon Aurora User Guide/ .
--
-- Constraints:
--
--     * Must specify a valid DB cluster parameter group.
--
--
--     * If the source DB cluster parameter group is in the same AWS Region as the copy, specify a valid DB parameter group identifier, for example @my-db-cluster-param-group@ , or a valid ARN.
--
--
--     * If the source DB parameter group is in a different AWS Region than the copy, specify a valid DB cluster parameter group ARN, for example @arn:aws:rds:us-east-1:123456789012:cluster-pg:custom-cluster-group1@ .
--
--
-- * 'targetDBClusterParameterGroupIdentifier' - The identifier for the copied DB cluster parameter group.
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
-- Example: @my-cluster-param-group1@
-- * 'targetDBClusterParameterGroupDescription' - A description for the copied DB cluster parameter group.
-- * 'tags' -
mkCopyDBClusterParameterGroup ::
  -- | 'sourceDBClusterParameterGroupIdentifier'
  Lude.Text ->
  -- | 'targetDBClusterParameterGroupIdentifier'
  Lude.Text ->
  -- | 'targetDBClusterParameterGroupDescription'
  Lude.Text ->
  CopyDBClusterParameterGroup
mkCopyDBClusterParameterGroup
  pSourceDBClusterParameterGroupIdentifier_
  pTargetDBClusterParameterGroupIdentifier_
  pTargetDBClusterParameterGroupDescription_ =
    CopyDBClusterParameterGroup'
      { sourceDBClusterParameterGroupIdentifier =
          pSourceDBClusterParameterGroupIdentifier_,
        targetDBClusterParameterGroupIdentifier =
          pTargetDBClusterParameterGroupIdentifier_,
        targetDBClusterParameterGroupDescription =
          pTargetDBClusterParameterGroupDescription_,
        tags = Lude.Nothing
      }

-- | The identifier or Amazon Resource Name (ARN) for the source DB cluster parameter group. For information about creating an ARN, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS> in the /Amazon Aurora User Guide/ .
--
-- Constraints:
--
--     * Must specify a valid DB cluster parameter group.
--
--
--     * If the source DB cluster parameter group is in the same AWS Region as the copy, specify a valid DB parameter group identifier, for example @my-db-cluster-param-group@ , or a valid ARN.
--
--
--     * If the source DB parameter group is in a different AWS Region than the copy, specify a valid DB cluster parameter group ARN, for example @arn:aws:rds:us-east-1:123456789012:cluster-pg:custom-cluster-group1@ .
--
--
--
-- /Note:/ Consider using 'sourceDBClusterParameterGroupIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcpgSourceDBClusterParameterGroupIdentifier :: Lens.Lens' CopyDBClusterParameterGroup Lude.Text
cdbcpgSourceDBClusterParameterGroupIdentifier = Lens.lens (sourceDBClusterParameterGroupIdentifier :: CopyDBClusterParameterGroup -> Lude.Text) (\s a -> s {sourceDBClusterParameterGroupIdentifier = a} :: CopyDBClusterParameterGroup)
{-# DEPRECATED cdbcpgSourceDBClusterParameterGroupIdentifier "Use generic-lens or generic-optics with 'sourceDBClusterParameterGroupIdentifier' instead." #-}

-- | The identifier for the copied DB cluster parameter group.
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
-- Example: @my-cluster-param-group1@
--
-- /Note:/ Consider using 'targetDBClusterParameterGroupIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcpgTargetDBClusterParameterGroupIdentifier :: Lens.Lens' CopyDBClusterParameterGroup Lude.Text
cdbcpgTargetDBClusterParameterGroupIdentifier = Lens.lens (targetDBClusterParameterGroupIdentifier :: CopyDBClusterParameterGroup -> Lude.Text) (\s a -> s {targetDBClusterParameterGroupIdentifier = a} :: CopyDBClusterParameterGroup)
{-# DEPRECATED cdbcpgTargetDBClusterParameterGroupIdentifier "Use generic-lens or generic-optics with 'targetDBClusterParameterGroupIdentifier' instead." #-}

-- | A description for the copied DB cluster parameter group.
--
-- /Note:/ Consider using 'targetDBClusterParameterGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcpgTargetDBClusterParameterGroupDescription :: Lens.Lens' CopyDBClusterParameterGroup Lude.Text
cdbcpgTargetDBClusterParameterGroupDescription = Lens.lens (targetDBClusterParameterGroupDescription :: CopyDBClusterParameterGroup -> Lude.Text) (\s a -> s {targetDBClusterParameterGroupDescription = a} :: CopyDBClusterParameterGroup)
{-# DEPRECATED cdbcpgTargetDBClusterParameterGroupDescription "Use generic-lens or generic-optics with 'targetDBClusterParameterGroupDescription' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcpgTags :: Lens.Lens' CopyDBClusterParameterGroup (Lude.Maybe [Tag])
cdbcpgTags = Lens.lens (tags :: CopyDBClusterParameterGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CopyDBClusterParameterGroup)
{-# DEPRECATED cdbcpgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CopyDBClusterParameterGroup where
  type
    Rs CopyDBClusterParameterGroup =
      CopyDBClusterParameterGroupResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "CopyDBClusterParameterGroupResult"
      ( \s h x ->
          CopyDBClusterParameterGroupResponse'
            Lude.<$> (x Lude..@? "DBClusterParameterGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CopyDBClusterParameterGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CopyDBClusterParameterGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CopyDBClusterParameterGroup where
  toQuery CopyDBClusterParameterGroup' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CopyDBClusterParameterGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "SourceDBClusterParameterGroupIdentifier"
          Lude.=: sourceDBClusterParameterGroupIdentifier,
        "TargetDBClusterParameterGroupIdentifier"
          Lude.=: targetDBClusterParameterGroupIdentifier,
        "TargetDBClusterParameterGroupDescription"
          Lude.=: targetDBClusterParameterGroupDescription,
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags)
      ]

-- | /See:/ 'mkCopyDBClusterParameterGroupResponse' smart constructor.
data CopyDBClusterParameterGroupResponse = CopyDBClusterParameterGroupResponse'
  { dbClusterParameterGroup :: Lude.Maybe DBClusterParameterGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CopyDBClusterParameterGroupResponse' with the minimum fields required to make a request.
--
-- * 'dbClusterParameterGroup' -
-- * 'responseStatus' - The response status code.
mkCopyDBClusterParameterGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CopyDBClusterParameterGroupResponse
mkCopyDBClusterParameterGroupResponse pResponseStatus_ =
  CopyDBClusterParameterGroupResponse'
    { dbClusterParameterGroup =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbClusterParameterGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcpgrsDBClusterParameterGroup :: Lens.Lens' CopyDBClusterParameterGroupResponse (Lude.Maybe DBClusterParameterGroup)
cdcpgrsDBClusterParameterGroup = Lens.lens (dbClusterParameterGroup :: CopyDBClusterParameterGroupResponse -> Lude.Maybe DBClusterParameterGroup) (\s a -> s {dbClusterParameterGroup = a} :: CopyDBClusterParameterGroupResponse)
{-# DEPRECATED cdcpgrsDBClusterParameterGroup "Use generic-lens or generic-optics with 'dbClusterParameterGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcpgrsResponseStatus :: Lens.Lens' CopyDBClusterParameterGroupResponse Lude.Int
cdcpgrsResponseStatus = Lens.lens (responseStatus :: CopyDBClusterParameterGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CopyDBClusterParameterGroupResponse)
{-# DEPRECATED cdcpgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
