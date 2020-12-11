{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    cdpgTags,
    cdpgSourceDBParameterGroupIdentifier,
    cdpgTargetDBParameterGroupIdentifier,
    cdpgTargetDBParameterGroupDescription,

    -- * Destructuring the response
    CopyDBParameterGroupResponse (..),
    mkCopyDBParameterGroupResponse,

    -- ** Response lenses
    cdbpgrsDBParameterGroup,
    cdbpgrsResponseStatus,
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
  { tags ::
      Lude.Maybe [Tag],
    sourceDBParameterGroupIdentifier :: Lude.Text,
    targetDBParameterGroupIdentifier :: Lude.Text,
    targetDBParameterGroupDescription :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CopyDBParameterGroup' with the minimum fields required to make a request.
--
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
-- * 'tags' - Undocumented field.
-- * 'targetDBParameterGroupDescription' - A description for the copied DB parameter group.
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
mkCopyDBParameterGroup ::
  -- | 'sourceDBParameterGroupIdentifier'
  Lude.Text ->
  -- | 'targetDBParameterGroupIdentifier'
  Lude.Text ->
  -- | 'targetDBParameterGroupDescription'
  Lude.Text ->
  CopyDBParameterGroup
mkCopyDBParameterGroup
  pSourceDBParameterGroupIdentifier_
  pTargetDBParameterGroupIdentifier_
  pTargetDBParameterGroupDescription_ =
    CopyDBParameterGroup'
      { tags = Lude.Nothing,
        sourceDBParameterGroupIdentifier =
          pSourceDBParameterGroupIdentifier_,
        targetDBParameterGroupIdentifier =
          pTargetDBParameterGroupIdentifier_,
        targetDBParameterGroupDescription =
          pTargetDBParameterGroupDescription_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpgTags :: Lens.Lens' CopyDBParameterGroup (Lude.Maybe [Tag])
cdpgTags = Lens.lens (tags :: CopyDBParameterGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CopyDBParameterGroup)
{-# DEPRECATED cdpgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

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
cdpgSourceDBParameterGroupIdentifier :: Lens.Lens' CopyDBParameterGroup Lude.Text
cdpgSourceDBParameterGroupIdentifier = Lens.lens (sourceDBParameterGroupIdentifier :: CopyDBParameterGroup -> Lude.Text) (\s a -> s {sourceDBParameterGroupIdentifier = a} :: CopyDBParameterGroup)
{-# DEPRECATED cdpgSourceDBParameterGroupIdentifier "Use generic-lens or generic-optics with 'sourceDBParameterGroupIdentifier' instead." #-}

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
cdpgTargetDBParameterGroupIdentifier :: Lens.Lens' CopyDBParameterGroup Lude.Text
cdpgTargetDBParameterGroupIdentifier = Lens.lens (targetDBParameterGroupIdentifier :: CopyDBParameterGroup -> Lude.Text) (\s a -> s {targetDBParameterGroupIdentifier = a} :: CopyDBParameterGroup)
{-# DEPRECATED cdpgTargetDBParameterGroupIdentifier "Use generic-lens or generic-optics with 'targetDBParameterGroupIdentifier' instead." #-}

-- | A description for the copied DB parameter group.
--
-- /Note:/ Consider using 'targetDBParameterGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpgTargetDBParameterGroupDescription :: Lens.Lens' CopyDBParameterGroup Lude.Text
cdpgTargetDBParameterGroupDescription = Lens.lens (targetDBParameterGroupDescription :: CopyDBParameterGroup -> Lude.Text) (\s a -> s {targetDBParameterGroupDescription = a} :: CopyDBParameterGroup)
{-# DEPRECATED cdpgTargetDBParameterGroupDescription "Use generic-lens or generic-optics with 'targetDBParameterGroupDescription' instead." #-}

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
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags),
        "SourceDBParameterGroupIdentifier"
          Lude.=: sourceDBParameterGroupIdentifier,
        "TargetDBParameterGroupIdentifier"
          Lude.=: targetDBParameterGroupIdentifier,
        "TargetDBParameterGroupDescription"
          Lude.=: targetDBParameterGroupDescription
      ]

-- | /See:/ 'mkCopyDBParameterGroupResponse' smart constructor.
data CopyDBParameterGroupResponse = CopyDBParameterGroupResponse'
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

-- | Creates a value of 'CopyDBParameterGroupResponse' with the minimum fields required to make a request.
--
-- * 'dbParameterGroup' - Undocumented field.
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
cdbpgrsDBParameterGroup :: Lens.Lens' CopyDBParameterGroupResponse (Lude.Maybe DBParameterGroup)
cdbpgrsDBParameterGroup = Lens.lens (dbParameterGroup :: CopyDBParameterGroupResponse -> Lude.Maybe DBParameterGroup) (\s a -> s {dbParameterGroup = a} :: CopyDBParameterGroupResponse)
{-# DEPRECATED cdbpgrsDBParameterGroup "Use generic-lens or generic-optics with 'dbParameterGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbpgrsResponseStatus :: Lens.Lens' CopyDBParameterGroupResponse Lude.Int
cdbpgrsResponseStatus = Lens.lens (responseStatus :: CopyDBParameterGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CopyDBParameterGroupResponse)
{-# DEPRECATED cdbpgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
