{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CopyDBParameterGroup (..),
    mkCopyDBParameterGroup,

    -- ** Request lenses
    cdbpgTargetDBParameterGroupDescription,
    cdbpgSourceDBParameterGroupIdentifier,
    cdbpgTargetDBParameterGroupIdentifier,
    cdbpgTags,

    -- * Destructuring the response
    CopyDBParameterGroupResponse (..),
    mkCopyDBParameterGroupResponse,

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
-- /See:/ 'mkCopyDBParameterGroup' smart constructor.
data CopyDBParameterGroup = CopyDBParameterGroup'
  { -- | A description for the copied DB parameter group.
    targetDBParameterGroupDescription :: Lude.Text,
    -- | The identifier or ARN for the source DB parameter group. For information about creating an ARN, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS> in the /Amazon RDS User Guide/ .
    --
    -- Constraints:
    --
    --     * Must specify a valid DB parameter group.
    --
    --
    --     * Must specify a valid DB parameter group identifier, for example @my-db-param-group@ , or a valid ARN.
    sourceDBParameterGroupIdentifier :: Lude.Text,
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
    targetDBParameterGroupIdentifier :: Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CopyDBParameterGroup' with the minimum fields required to make a request.
--
-- * 'targetDBParameterGroupDescription' - A description for the copied DB parameter group.
-- * 'sourceDBParameterGroupIdentifier' - The identifier or ARN for the source DB parameter group. For information about creating an ARN, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS> in the /Amazon RDS User Guide/ .
--
-- Constraints:
--
--     * Must specify a valid DB parameter group.
--
--
--     * Must specify a valid DB parameter group identifier, for example @my-db-param-group@ , or a valid ARN.
--
--
-- * 'targetDBParameterGroupIdentifier' - The identifier for the copied DB parameter group.
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
-- * 'tags' -
mkCopyDBParameterGroup ::
  -- | 'targetDBParameterGroupDescription'
  Lude.Text ->
  -- | 'sourceDBParameterGroupIdentifier'
  Lude.Text ->
  -- | 'targetDBParameterGroupIdentifier'
  Lude.Text ->
  CopyDBParameterGroup
mkCopyDBParameterGroup
  pTargetDBParameterGroupDescription_
  pSourceDBParameterGroupIdentifier_
  pTargetDBParameterGroupIdentifier_ =
    CopyDBParameterGroup'
      { targetDBParameterGroupDescription =
          pTargetDBParameterGroupDescription_,
        sourceDBParameterGroupIdentifier =
          pSourceDBParameterGroupIdentifier_,
        targetDBParameterGroupIdentifier =
          pTargetDBParameterGroupIdentifier_,
        tags = Lude.Nothing
      }

-- | A description for the copied DB parameter group.
--
-- /Note:/ Consider using 'targetDBParameterGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbpgTargetDBParameterGroupDescription :: Lens.Lens' CopyDBParameterGroup Lude.Text
cdbpgTargetDBParameterGroupDescription = Lens.lens (targetDBParameterGroupDescription :: CopyDBParameterGroup -> Lude.Text) (\s a -> s {targetDBParameterGroupDescription = a} :: CopyDBParameterGroup)
{-# DEPRECATED cdbpgTargetDBParameterGroupDescription "Use generic-lens or generic-optics with 'targetDBParameterGroupDescription' instead." #-}

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
cdbpgSourceDBParameterGroupIdentifier :: Lens.Lens' CopyDBParameterGroup Lude.Text
cdbpgSourceDBParameterGroupIdentifier = Lens.lens (sourceDBParameterGroupIdentifier :: CopyDBParameterGroup -> Lude.Text) (\s a -> s {sourceDBParameterGroupIdentifier = a} :: CopyDBParameterGroup)
{-# DEPRECATED cdbpgSourceDBParameterGroupIdentifier "Use generic-lens or generic-optics with 'sourceDBParameterGroupIdentifier' instead." #-}

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
cdbpgTargetDBParameterGroupIdentifier :: Lens.Lens' CopyDBParameterGroup Lude.Text
cdbpgTargetDBParameterGroupIdentifier = Lens.lens (targetDBParameterGroupIdentifier :: CopyDBParameterGroup -> Lude.Text) (\s a -> s {targetDBParameterGroupIdentifier = a} :: CopyDBParameterGroup)
{-# DEPRECATED cdbpgTargetDBParameterGroupIdentifier "Use generic-lens or generic-optics with 'targetDBParameterGroupIdentifier' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbpgTags :: Lens.Lens' CopyDBParameterGroup (Lude.Maybe [Tag])
cdbpgTags = Lens.lens (tags :: CopyDBParameterGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CopyDBParameterGroup)
{-# DEPRECATED cdbpgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CopyDBParameterGroup where
  type Rs CopyDBParameterGroup = CopyDBParameterGroupResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "CopyDBParameterGroupResult"
      ( \s h x ->
          CopyDBParameterGroupResponse'
            Lude.<$> (x Lude..@? "DBParameterGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CopyDBParameterGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CopyDBParameterGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CopyDBParameterGroup where
  toQuery CopyDBParameterGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CopyDBParameterGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "TargetDBParameterGroupDescription"
          Lude.=: targetDBParameterGroupDescription,
        "SourceDBParameterGroupIdentifier"
          Lude.=: sourceDBParameterGroupIdentifier,
        "TargetDBParameterGroupIdentifier"
          Lude.=: targetDBParameterGroupIdentifier,
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags)
      ]

-- | /See:/ 'mkCopyDBParameterGroupResponse' smart constructor.
data CopyDBParameterGroupResponse = CopyDBParameterGroupResponse'
  { dbParameterGroup :: Lude.Maybe DBParameterGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CopyDBParameterGroupResponse' with the minimum fields required to make a request.
--
-- * 'dbParameterGroup' -
-- * 'responseStatus' - The response status code.
mkCopyDBParameterGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CopyDBParameterGroupResponse
mkCopyDBParameterGroupResponse pResponseStatus_ =
  CopyDBParameterGroupResponse'
    { dbParameterGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbParameterGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpgrsDBParameterGroup :: Lens.Lens' CopyDBParameterGroupResponse (Lude.Maybe DBParameterGroup)
cdpgrsDBParameterGroup = Lens.lens (dbParameterGroup :: CopyDBParameterGroupResponse -> Lude.Maybe DBParameterGroup) (\s a -> s {dbParameterGroup = a} :: CopyDBParameterGroupResponse)
{-# DEPRECATED cdpgrsDBParameterGroup "Use generic-lens or generic-optics with 'dbParameterGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpgrsResponseStatus :: Lens.Lens' CopyDBParameterGroupResponse Lude.Int
cdpgrsResponseStatus = Lens.lens (responseStatus :: CopyDBParameterGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CopyDBParameterGroupResponse)
{-# DEPRECATED cdpgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
