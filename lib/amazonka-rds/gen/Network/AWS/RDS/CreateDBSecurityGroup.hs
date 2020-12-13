{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB security group. DB security groups control access to a DB instance.
module Network.AWS.RDS.CreateDBSecurityGroup
  ( -- * Creating a request
    CreateDBSecurityGroup (..),
    mkCreateDBSecurityGroup,

    -- ** Request lenses
    cdbsgDBSecurityGroupName,
    cdbsgDBSecurityGroupDescription,
    cdbsgTags,

    -- * Destructuring the response
    CreateDBSecurityGroupResponse (..),
    mkCreateDBSecurityGroupResponse,

    -- ** Response lenses
    cdbsgrsDBSecurityGroup,
    cdbsgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkCreateDBSecurityGroup' smart constructor.
data CreateDBSecurityGroup = CreateDBSecurityGroup'
  { -- | The name for the DB security group. This value is stored as a lowercase string.
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
    --     * Must not be "Default"
    --
    --
    -- Example: @mysecuritygroup@
    dbSecurityGroupName :: Lude.Text,
    -- | The description for the DB security group.
    dbSecurityGroupDescription :: Lude.Text,
    -- | Tags to assign to the DB security group.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDBSecurityGroup' with the minimum fields required to make a request.
--
-- * 'dbSecurityGroupName' - The name for the DB security group. This value is stored as a lowercase string.
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
--     * Must not be "Default"
--
--
-- Example: @mysecuritygroup@
-- * 'dbSecurityGroupDescription' - The description for the DB security group.
-- * 'tags' - Tags to assign to the DB security group.
mkCreateDBSecurityGroup ::
  -- | 'dbSecurityGroupName'
  Lude.Text ->
  -- | 'dbSecurityGroupDescription'
  Lude.Text ->
  CreateDBSecurityGroup
mkCreateDBSecurityGroup
  pDBSecurityGroupName_
  pDBSecurityGroupDescription_ =
    CreateDBSecurityGroup'
      { dbSecurityGroupName =
          pDBSecurityGroupName_,
        dbSecurityGroupDescription = pDBSecurityGroupDescription_,
        tags = Lude.Nothing
      }

-- | The name for the DB security group. This value is stored as a lowercase string.
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
--     * Must not be "Default"
--
--
-- Example: @mysecuritygroup@
--
-- /Note:/ Consider using 'dbSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsgDBSecurityGroupName :: Lens.Lens' CreateDBSecurityGroup Lude.Text
cdbsgDBSecurityGroupName = Lens.lens (dbSecurityGroupName :: CreateDBSecurityGroup -> Lude.Text) (\s a -> s {dbSecurityGroupName = a} :: CreateDBSecurityGroup)
{-# DEPRECATED cdbsgDBSecurityGroupName "Use generic-lens or generic-optics with 'dbSecurityGroupName' instead." #-}

-- | The description for the DB security group.
--
-- /Note:/ Consider using 'dbSecurityGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsgDBSecurityGroupDescription :: Lens.Lens' CreateDBSecurityGroup Lude.Text
cdbsgDBSecurityGroupDescription = Lens.lens (dbSecurityGroupDescription :: CreateDBSecurityGroup -> Lude.Text) (\s a -> s {dbSecurityGroupDescription = a} :: CreateDBSecurityGroup)
{-# DEPRECATED cdbsgDBSecurityGroupDescription "Use generic-lens or generic-optics with 'dbSecurityGroupDescription' instead." #-}

-- | Tags to assign to the DB security group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsgTags :: Lens.Lens' CreateDBSecurityGroup (Lude.Maybe [Tag])
cdbsgTags = Lens.lens (tags :: CreateDBSecurityGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateDBSecurityGroup)
{-# DEPRECATED cdbsgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateDBSecurityGroup where
  type Rs CreateDBSecurityGroup = CreateDBSecurityGroupResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "CreateDBSecurityGroupResult"
      ( \s h x ->
          CreateDBSecurityGroupResponse'
            Lude.<$> (x Lude..@? "DBSecurityGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDBSecurityGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateDBSecurityGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDBSecurityGroup where
  toQuery CreateDBSecurityGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateDBSecurityGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBSecurityGroupName" Lude.=: dbSecurityGroupName,
        "DBSecurityGroupDescription" Lude.=: dbSecurityGroupDescription,
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags)
      ]

-- | /See:/ 'mkCreateDBSecurityGroupResponse' smart constructor.
data CreateDBSecurityGroupResponse = CreateDBSecurityGroupResponse'
  { dbSecurityGroup :: Lude.Maybe DBSecurityGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDBSecurityGroupResponse' with the minimum fields required to make a request.
--
-- * 'dbSecurityGroup' -
-- * 'responseStatus' - The response status code.
mkCreateDBSecurityGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDBSecurityGroupResponse
mkCreateDBSecurityGroupResponse pResponseStatus_ =
  CreateDBSecurityGroupResponse'
    { dbSecurityGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsgrsDBSecurityGroup :: Lens.Lens' CreateDBSecurityGroupResponse (Lude.Maybe DBSecurityGroup)
cdbsgrsDBSecurityGroup = Lens.lens (dbSecurityGroup :: CreateDBSecurityGroupResponse -> Lude.Maybe DBSecurityGroup) (\s a -> s {dbSecurityGroup = a} :: CreateDBSecurityGroupResponse)
{-# DEPRECATED cdbsgrsDBSecurityGroup "Use generic-lens or generic-optics with 'dbSecurityGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsgrsResponseStatus :: Lens.Lens' CreateDBSecurityGroupResponse Lude.Int
cdbsgrsResponseStatus = Lens.lens (responseStatus :: CreateDBSecurityGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDBSecurityGroupResponse)
{-# DEPRECATED cdbsgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
