{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.UpdateSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing subnet group.
module Network.AWS.DAX.UpdateSubnetGroup
  ( -- * Creating a request
    UpdateSubnetGroup (..),
    mkUpdateSubnetGroup,

    -- ** Request lenses
    usgSubnetIds,
    usgDescription,
    usgSubnetGroupName,

    -- * Destructuring the response
    UpdateSubnetGroupResponse (..),
    mkUpdateSubnetGroupResponse,

    -- ** Response lenses
    usgrsSubnetGroup,
    usgrsResponseStatus,
  )
where

import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateSubnetGroup' smart constructor.
data UpdateSubnetGroup = UpdateSubnetGroup'
  { subnetIds ::
      Lude.Maybe [Lude.Text],
    description :: Lude.Maybe Lude.Text,
    subnetGroupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSubnetGroup' with the minimum fields required to make a request.
--
-- * 'description' - A description of the subnet group.
-- * 'subnetGroupName' - The name of the subnet group.
-- * 'subnetIds' - A list of subnet IDs in the subnet group.
mkUpdateSubnetGroup ::
  -- | 'subnetGroupName'
  Lude.Text ->
  UpdateSubnetGroup
mkUpdateSubnetGroup pSubnetGroupName_ =
  UpdateSubnetGroup'
    { subnetIds = Lude.Nothing,
      description = Lude.Nothing,
      subnetGroupName = pSubnetGroupName_
    }

-- | A list of subnet IDs in the subnet group.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgSubnetIds :: Lens.Lens' UpdateSubnetGroup (Lude.Maybe [Lude.Text])
usgSubnetIds = Lens.lens (subnetIds :: UpdateSubnetGroup -> Lude.Maybe [Lude.Text]) (\s a -> s {subnetIds = a} :: UpdateSubnetGroup)
{-# DEPRECATED usgSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | A description of the subnet group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgDescription :: Lens.Lens' UpdateSubnetGroup (Lude.Maybe Lude.Text)
usgDescription = Lens.lens (description :: UpdateSubnetGroup -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateSubnetGroup)
{-# DEPRECATED usgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the subnet group.
--
-- /Note:/ Consider using 'subnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgSubnetGroupName :: Lens.Lens' UpdateSubnetGroup Lude.Text
usgSubnetGroupName = Lens.lens (subnetGroupName :: UpdateSubnetGroup -> Lude.Text) (\s a -> s {subnetGroupName = a} :: UpdateSubnetGroup)
{-# DEPRECATED usgSubnetGroupName "Use generic-lens or generic-optics with 'subnetGroupName' instead." #-}

instance Lude.AWSRequest UpdateSubnetGroup where
  type Rs UpdateSubnetGroup = UpdateSubnetGroupResponse
  request = Req.postJSON daxService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateSubnetGroupResponse'
            Lude.<$> (x Lude..?> "SubnetGroup") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateSubnetGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDAXV3.UpdateSubnetGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateSubnetGroup where
  toJSON UpdateSubnetGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SubnetIds" Lude..=) Lude.<$> subnetIds,
            ("Description" Lude..=) Lude.<$> description,
            Lude.Just ("SubnetGroupName" Lude..= subnetGroupName)
          ]
      )

instance Lude.ToPath UpdateSubnetGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateSubnetGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateSubnetGroupResponse' smart constructor.
data UpdateSubnetGroupResponse = UpdateSubnetGroupResponse'
  { subnetGroup ::
      Lude.Maybe SubnetGroup,
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

-- | Creates a value of 'UpdateSubnetGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'subnetGroup' - The subnet group that has been modified.
mkUpdateSubnetGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateSubnetGroupResponse
mkUpdateSubnetGroupResponse pResponseStatus_ =
  UpdateSubnetGroupResponse'
    { subnetGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The subnet group that has been modified.
--
-- /Note:/ Consider using 'subnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrsSubnetGroup :: Lens.Lens' UpdateSubnetGroupResponse (Lude.Maybe SubnetGroup)
usgrsSubnetGroup = Lens.lens (subnetGroup :: UpdateSubnetGroupResponse -> Lude.Maybe SubnetGroup) (\s a -> s {subnetGroup = a} :: UpdateSubnetGroupResponse)
{-# DEPRECATED usgrsSubnetGroup "Use generic-lens or generic-optics with 'subnetGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrsResponseStatus :: Lens.Lens' UpdateSubnetGroupResponse Lude.Int
usgrsResponseStatus = Lens.lens (responseStatus :: UpdateSubnetGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateSubnetGroupResponse)
{-# DEPRECATED usgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
