{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB subnet group. DB subnet groups must contain at least one subnet in at least two AZs in the AWS Region.
module Network.AWS.RDS.CreateDBSubnetGroup
  ( -- * Creating a request
    CreateDBSubnetGroup (..),
    mkCreateDBSubnetGroup,

    -- ** Request lenses
    cdsgDBSubnetGroupName,
    cdsgSubnetIds,
    cdsgDBSubnetGroupDescription,
    cdsgTags,

    -- * Destructuring the response
    CreateDBSubnetGroupResponse (..),
    mkCreateDBSubnetGroupResponse,

    -- ** Response lenses
    cdsgrsDBSubnetGroup,
    cdsgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkCreateDBSubnetGroup' smart constructor.
data CreateDBSubnetGroup = CreateDBSubnetGroup'
  { -- | The name for the DB subnet group. This value is stored as a lowercase string.
    --
    -- Constraints: Must contain no more than 255 letters, numbers, periods, underscores, spaces, or hyphens. Must not be default.
    -- Example: @mySubnetgroup@
    dbSubnetGroupName :: Lude.Text,
    -- | The EC2 Subnet IDs for the DB subnet group.
    subnetIds :: [Lude.Text],
    -- | The description for the DB subnet group.
    dbSubnetGroupDescription :: Lude.Text,
    -- | Tags to assign to the DB subnet group.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDBSubnetGroup' with the minimum fields required to make a request.
--
-- * 'dbSubnetGroupName' - The name for the DB subnet group. This value is stored as a lowercase string.
--
-- Constraints: Must contain no more than 255 letters, numbers, periods, underscores, spaces, or hyphens. Must not be default.
-- Example: @mySubnetgroup@
-- * 'subnetIds' - The EC2 Subnet IDs for the DB subnet group.
-- * 'dbSubnetGroupDescription' - The description for the DB subnet group.
-- * 'tags' - Tags to assign to the DB subnet group.
mkCreateDBSubnetGroup ::
  -- | 'dbSubnetGroupName'
  Lude.Text ->
  -- | 'dbSubnetGroupDescription'
  Lude.Text ->
  CreateDBSubnetGroup
mkCreateDBSubnetGroup
  pDBSubnetGroupName_
  pDBSubnetGroupDescription_ =
    CreateDBSubnetGroup'
      { dbSubnetGroupName = pDBSubnetGroupName_,
        subnetIds = Lude.mempty,
        dbSubnetGroupDescription = pDBSubnetGroupDescription_,
        tags = Lude.Nothing
      }

-- | The name for the DB subnet group. This value is stored as a lowercase string.
--
-- Constraints: Must contain no more than 255 letters, numbers, periods, underscores, spaces, or hyphens. Must not be default.
-- Example: @mySubnetgroup@
--
-- /Note:/ Consider using 'dbSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsgDBSubnetGroupName :: Lens.Lens' CreateDBSubnetGroup Lude.Text
cdsgDBSubnetGroupName = Lens.lens (dbSubnetGroupName :: CreateDBSubnetGroup -> Lude.Text) (\s a -> s {dbSubnetGroupName = a} :: CreateDBSubnetGroup)
{-# DEPRECATED cdsgDBSubnetGroupName "Use generic-lens or generic-optics with 'dbSubnetGroupName' instead." #-}

-- | The EC2 Subnet IDs for the DB subnet group.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsgSubnetIds :: Lens.Lens' CreateDBSubnetGroup [Lude.Text]
cdsgSubnetIds = Lens.lens (subnetIds :: CreateDBSubnetGroup -> [Lude.Text]) (\s a -> s {subnetIds = a} :: CreateDBSubnetGroup)
{-# DEPRECATED cdsgSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The description for the DB subnet group.
--
-- /Note:/ Consider using 'dbSubnetGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsgDBSubnetGroupDescription :: Lens.Lens' CreateDBSubnetGroup Lude.Text
cdsgDBSubnetGroupDescription = Lens.lens (dbSubnetGroupDescription :: CreateDBSubnetGroup -> Lude.Text) (\s a -> s {dbSubnetGroupDescription = a} :: CreateDBSubnetGroup)
{-# DEPRECATED cdsgDBSubnetGroupDescription "Use generic-lens or generic-optics with 'dbSubnetGroupDescription' instead." #-}

-- | Tags to assign to the DB subnet group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsgTags :: Lens.Lens' CreateDBSubnetGroup (Lude.Maybe [Tag])
cdsgTags = Lens.lens (tags :: CreateDBSubnetGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateDBSubnetGroup)
{-# DEPRECATED cdsgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateDBSubnetGroup where
  type Rs CreateDBSubnetGroup = CreateDBSubnetGroupResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "CreateDBSubnetGroupResult"
      ( \s h x ->
          CreateDBSubnetGroupResponse'
            Lude.<$> (x Lude..@? "DBSubnetGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDBSubnetGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateDBSubnetGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDBSubnetGroup where
  toQuery CreateDBSubnetGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateDBSubnetGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBSubnetGroupName" Lude.=: dbSubnetGroupName,
        "SubnetIds" Lude.=: Lude.toQueryList "SubnetIdentifier" subnetIds,
        "DBSubnetGroupDescription" Lude.=: dbSubnetGroupDescription,
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags)
      ]

-- | /See:/ 'mkCreateDBSubnetGroupResponse' smart constructor.
data CreateDBSubnetGroupResponse = CreateDBSubnetGroupResponse'
  { dbSubnetGroup :: Lude.Maybe DBSubnetGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDBSubnetGroupResponse' with the minimum fields required to make a request.
--
-- * 'dbSubnetGroup' -
-- * 'responseStatus' - The response status code.
mkCreateDBSubnetGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDBSubnetGroupResponse
mkCreateDBSubnetGroupResponse pResponseStatus_ =
  CreateDBSubnetGroupResponse'
    { dbSubnetGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbSubnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsgrsDBSubnetGroup :: Lens.Lens' CreateDBSubnetGroupResponse (Lude.Maybe DBSubnetGroup)
cdsgrsDBSubnetGroup = Lens.lens (dbSubnetGroup :: CreateDBSubnetGroupResponse -> Lude.Maybe DBSubnetGroup) (\s a -> s {dbSubnetGroup = a} :: CreateDBSubnetGroupResponse)
{-# DEPRECATED cdsgrsDBSubnetGroup "Use generic-lens or generic-optics with 'dbSubnetGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsgrsResponseStatus :: Lens.Lens' CreateDBSubnetGroupResponse Lude.Int
cdsgrsResponseStatus = Lens.lens (responseStatus :: CreateDBSubnetGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDBSubnetGroupResponse)
{-# DEPRECATED cdsgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
