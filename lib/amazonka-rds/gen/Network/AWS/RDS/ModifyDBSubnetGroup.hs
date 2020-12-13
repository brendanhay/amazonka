{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyDBSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing DB subnet group. DB subnet groups must contain at least one subnet in at least two AZs in the AWS Region.
module Network.AWS.RDS.ModifyDBSubnetGroup
  ( -- * Creating a request
    ModifyDBSubnetGroup (..),
    mkModifyDBSubnetGroup,

    -- ** Request lenses
    mdsgDBSubnetGroupName,
    mdsgSubnetIds,
    mdsgDBSubnetGroupDescription,

    -- * Destructuring the response
    ModifyDBSubnetGroupResponse (..),
    mkModifyDBSubnetGroupResponse,

    -- ** Response lenses
    mdsgrsDBSubnetGroup,
    mdsgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkModifyDBSubnetGroup' smart constructor.
data ModifyDBSubnetGroup = ModifyDBSubnetGroup'
  { -- | The name for the DB subnet group. This value is stored as a lowercase string. You can't modify the default subnet group.
    --
    -- Constraints: Must match the name of an existing DBSubnetGroup. Must not be default.
    -- Example: @mySubnetgroup@
    dbSubnetGroupName :: Lude.Text,
    -- | The EC2 subnet IDs for the DB subnet group.
    subnetIds :: [Lude.Text],
    -- | The description for the DB subnet group.
    dbSubnetGroupDescription :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyDBSubnetGroup' with the minimum fields required to make a request.
--
-- * 'dbSubnetGroupName' - The name for the DB subnet group. This value is stored as a lowercase string. You can't modify the default subnet group.
--
-- Constraints: Must match the name of an existing DBSubnetGroup. Must not be default.
-- Example: @mySubnetgroup@
-- * 'subnetIds' - The EC2 subnet IDs for the DB subnet group.
-- * 'dbSubnetGroupDescription' - The description for the DB subnet group.
mkModifyDBSubnetGroup ::
  -- | 'dbSubnetGroupName'
  Lude.Text ->
  ModifyDBSubnetGroup
mkModifyDBSubnetGroup pDBSubnetGroupName_ =
  ModifyDBSubnetGroup'
    { dbSubnetGroupName = pDBSubnetGroupName_,
      subnetIds = Lude.mempty,
      dbSubnetGroupDescription = Lude.Nothing
    }

-- | The name for the DB subnet group. This value is stored as a lowercase string. You can't modify the default subnet group.
--
-- Constraints: Must match the name of an existing DBSubnetGroup. Must not be default.
-- Example: @mySubnetgroup@
--
-- /Note:/ Consider using 'dbSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsgDBSubnetGroupName :: Lens.Lens' ModifyDBSubnetGroup Lude.Text
mdsgDBSubnetGroupName = Lens.lens (dbSubnetGroupName :: ModifyDBSubnetGroup -> Lude.Text) (\s a -> s {dbSubnetGroupName = a} :: ModifyDBSubnetGroup)
{-# DEPRECATED mdsgDBSubnetGroupName "Use generic-lens or generic-optics with 'dbSubnetGroupName' instead." #-}

-- | The EC2 subnet IDs for the DB subnet group.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsgSubnetIds :: Lens.Lens' ModifyDBSubnetGroup [Lude.Text]
mdsgSubnetIds = Lens.lens (subnetIds :: ModifyDBSubnetGroup -> [Lude.Text]) (\s a -> s {subnetIds = a} :: ModifyDBSubnetGroup)
{-# DEPRECATED mdsgSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The description for the DB subnet group.
--
-- /Note:/ Consider using 'dbSubnetGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsgDBSubnetGroupDescription :: Lens.Lens' ModifyDBSubnetGroup (Lude.Maybe Lude.Text)
mdsgDBSubnetGroupDescription = Lens.lens (dbSubnetGroupDescription :: ModifyDBSubnetGroup -> Lude.Maybe Lude.Text) (\s a -> s {dbSubnetGroupDescription = a} :: ModifyDBSubnetGroup)
{-# DEPRECATED mdsgDBSubnetGroupDescription "Use generic-lens or generic-optics with 'dbSubnetGroupDescription' instead." #-}

instance Lude.AWSRequest ModifyDBSubnetGroup where
  type Rs ModifyDBSubnetGroup = ModifyDBSubnetGroupResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "ModifyDBSubnetGroupResult"
      ( \s h x ->
          ModifyDBSubnetGroupResponse'
            Lude.<$> (x Lude..@? "DBSubnetGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyDBSubnetGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyDBSubnetGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyDBSubnetGroup where
  toQuery ModifyDBSubnetGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyDBSubnetGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBSubnetGroupName" Lude.=: dbSubnetGroupName,
        "SubnetIds" Lude.=: Lude.toQueryList "SubnetIdentifier" subnetIds,
        "DBSubnetGroupDescription" Lude.=: dbSubnetGroupDescription
      ]

-- | /See:/ 'mkModifyDBSubnetGroupResponse' smart constructor.
data ModifyDBSubnetGroupResponse = ModifyDBSubnetGroupResponse'
  { dbSubnetGroup :: Lude.Maybe DBSubnetGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyDBSubnetGroupResponse' with the minimum fields required to make a request.
--
-- * 'dbSubnetGroup' -
-- * 'responseStatus' - The response status code.
mkModifyDBSubnetGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyDBSubnetGroupResponse
mkModifyDBSubnetGroupResponse pResponseStatus_ =
  ModifyDBSubnetGroupResponse'
    { dbSubnetGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbSubnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsgrsDBSubnetGroup :: Lens.Lens' ModifyDBSubnetGroupResponse (Lude.Maybe DBSubnetGroup)
mdsgrsDBSubnetGroup = Lens.lens (dbSubnetGroup :: ModifyDBSubnetGroupResponse -> Lude.Maybe DBSubnetGroup) (\s a -> s {dbSubnetGroup = a} :: ModifyDBSubnetGroupResponse)
{-# DEPRECATED mdsgrsDBSubnetGroup "Use generic-lens or generic-optics with 'dbSubnetGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsgrsResponseStatus :: Lens.Lens' ModifyDBSubnetGroupResponse Lude.Int
mdsgrsResponseStatus = Lens.lens (responseStatus :: ModifyDBSubnetGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyDBSubnetGroupResponse)
{-# DEPRECATED mdsgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
