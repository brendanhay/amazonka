{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.CreateSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new subnet group.
module Network.AWS.DAX.CreateSubnetGroup
  ( -- * Creating a request
    CreateSubnetGroup (..),
    mkCreateSubnetGroup,

    -- ** Request lenses
    csgSubnetIds,
    csgSubnetGroupName,
    csgDescription,

    -- * Destructuring the response
    CreateSubnetGroupResponse (..),
    mkCreateSubnetGroupResponse,

    -- ** Response lenses
    csgrsSubnetGroup,
    csgrsResponseStatus,
  )
where

import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateSubnetGroup' smart constructor.
data CreateSubnetGroup = CreateSubnetGroup'
  { -- | A list of VPC subnet IDs for the subnet group.
    subnetIds :: [Lude.Text],
    -- | A name for the subnet group. This value is stored as a lowercase string.
    subnetGroupName :: Lude.Text,
    -- | A description for the subnet group
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSubnetGroup' with the minimum fields required to make a request.
--
-- * 'subnetIds' - A list of VPC subnet IDs for the subnet group.
-- * 'subnetGroupName' - A name for the subnet group. This value is stored as a lowercase string.
-- * 'description' - A description for the subnet group
mkCreateSubnetGroup ::
  -- | 'subnetGroupName'
  Lude.Text ->
  CreateSubnetGroup
mkCreateSubnetGroup pSubnetGroupName_ =
  CreateSubnetGroup'
    { subnetIds = Lude.mempty,
      subnetGroupName = pSubnetGroupName_,
      description = Lude.Nothing
    }

-- | A list of VPC subnet IDs for the subnet group.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgSubnetIds :: Lens.Lens' CreateSubnetGroup [Lude.Text]
csgSubnetIds = Lens.lens (subnetIds :: CreateSubnetGroup -> [Lude.Text]) (\s a -> s {subnetIds = a} :: CreateSubnetGroup)
{-# DEPRECATED csgSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | A name for the subnet group. This value is stored as a lowercase string.
--
-- /Note:/ Consider using 'subnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgSubnetGroupName :: Lens.Lens' CreateSubnetGroup Lude.Text
csgSubnetGroupName = Lens.lens (subnetGroupName :: CreateSubnetGroup -> Lude.Text) (\s a -> s {subnetGroupName = a} :: CreateSubnetGroup)
{-# DEPRECATED csgSubnetGroupName "Use generic-lens or generic-optics with 'subnetGroupName' instead." #-}

-- | A description for the subnet group
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgDescription :: Lens.Lens' CreateSubnetGroup (Lude.Maybe Lude.Text)
csgDescription = Lens.lens (description :: CreateSubnetGroup -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateSubnetGroup)
{-# DEPRECATED csgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest CreateSubnetGroup where
  type Rs CreateSubnetGroup = CreateSubnetGroupResponse
  request = Req.postJSON daxService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateSubnetGroupResponse'
            Lude.<$> (x Lude..?> "SubnetGroup") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateSubnetGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDAXV3.CreateSubnetGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateSubnetGroup where
  toJSON CreateSubnetGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SubnetIds" Lude..= subnetIds),
            Lude.Just ("SubnetGroupName" Lude..= subnetGroupName),
            ("Description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath CreateSubnetGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateSubnetGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateSubnetGroupResponse' smart constructor.
data CreateSubnetGroupResponse = CreateSubnetGroupResponse'
  { -- | Represents the output of a /CreateSubnetGroup/ operation.
    subnetGroup :: Lude.Maybe SubnetGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSubnetGroupResponse' with the minimum fields required to make a request.
--
-- * 'subnetGroup' - Represents the output of a /CreateSubnetGroup/ operation.
-- * 'responseStatus' - The response status code.
mkCreateSubnetGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateSubnetGroupResponse
mkCreateSubnetGroupResponse pResponseStatus_ =
  CreateSubnetGroupResponse'
    { subnetGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Represents the output of a /CreateSubnetGroup/ operation.
--
-- /Note:/ Consider using 'subnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgrsSubnetGroup :: Lens.Lens' CreateSubnetGroupResponse (Lude.Maybe SubnetGroup)
csgrsSubnetGroup = Lens.lens (subnetGroup :: CreateSubnetGroupResponse -> Lude.Maybe SubnetGroup) (\s a -> s {subnetGroup = a} :: CreateSubnetGroupResponse)
{-# DEPRECATED csgrsSubnetGroup "Use generic-lens or generic-optics with 'subnetGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgrsResponseStatus :: Lens.Lens' CreateSubnetGroupResponse Lude.Int
csgrsResponseStatus = Lens.lens (responseStatus :: CreateSubnetGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateSubnetGroupResponse)
{-# DEPRECATED csgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
