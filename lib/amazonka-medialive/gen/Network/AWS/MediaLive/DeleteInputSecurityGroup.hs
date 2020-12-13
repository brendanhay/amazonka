{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DeleteInputSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Input Security Group
module Network.AWS.MediaLive.DeleteInputSecurityGroup
  ( -- * Creating a request
    DeleteInputSecurityGroup (..),
    mkDeleteInputSecurityGroup,

    -- ** Request lenses
    disgInputSecurityGroupId,

    -- * Destructuring the response
    DeleteInputSecurityGroupResponse (..),
    mkDeleteInputSecurityGroupResponse,

    -- ** Response lenses
    disgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for DeleteInputSecurityGroupRequest
--
-- /See:/ 'mkDeleteInputSecurityGroup' smart constructor.
newtype DeleteInputSecurityGroup = DeleteInputSecurityGroup'
  { -- | The Input Security Group to delete
    inputSecurityGroupId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteInputSecurityGroup' with the minimum fields required to make a request.
--
-- * 'inputSecurityGroupId' - The Input Security Group to delete
mkDeleteInputSecurityGroup ::
  -- | 'inputSecurityGroupId'
  Lude.Text ->
  DeleteInputSecurityGroup
mkDeleteInputSecurityGroup pInputSecurityGroupId_ =
  DeleteInputSecurityGroup'
    { inputSecurityGroupId =
        pInputSecurityGroupId_
    }

-- | The Input Security Group to delete
--
-- /Note:/ Consider using 'inputSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disgInputSecurityGroupId :: Lens.Lens' DeleteInputSecurityGroup Lude.Text
disgInputSecurityGroupId = Lens.lens (inputSecurityGroupId :: DeleteInputSecurityGroup -> Lude.Text) (\s a -> s {inputSecurityGroupId = a} :: DeleteInputSecurityGroup)
{-# DEPRECATED disgInputSecurityGroupId "Use generic-lens or generic-optics with 'inputSecurityGroupId' instead." #-}

instance Lude.AWSRequest DeleteInputSecurityGroup where
  type Rs DeleteInputSecurityGroup = DeleteInputSecurityGroupResponse
  request = Req.delete mediaLiveService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteInputSecurityGroupResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteInputSecurityGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteInputSecurityGroup where
  toPath DeleteInputSecurityGroup' {..} =
    Lude.mconcat
      ["/prod/inputSecurityGroups/", Lude.toBS inputSecurityGroupId]

instance Lude.ToQuery DeleteInputSecurityGroup where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for DeleteInputSecurityGroupResponse
--
-- /See:/ 'mkDeleteInputSecurityGroupResponse' smart constructor.
newtype DeleteInputSecurityGroupResponse = DeleteInputSecurityGroupResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteInputSecurityGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteInputSecurityGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteInputSecurityGroupResponse
mkDeleteInputSecurityGroupResponse pResponseStatus_ =
  DeleteInputSecurityGroupResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disgrsResponseStatus :: Lens.Lens' DeleteInputSecurityGroupResponse Lude.Int
disgrsResponseStatus = Lens.lens (responseStatus :: DeleteInputSecurityGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteInputSecurityGroupResponse)
{-# DEPRECATED disgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
