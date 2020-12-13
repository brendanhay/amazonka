{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.GetWorkGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the workgroup with the specified name.
module Network.AWS.Athena.GetWorkGroup
  ( -- * Creating a request
    GetWorkGroup (..),
    mkGetWorkGroup,

    -- ** Request lenses
    gwgWorkGroup,

    -- * Destructuring the response
    GetWorkGroupResponse (..),
    mkGetWorkGroupResponse,

    -- ** Response lenses
    gwgrsWorkGroup,
    gwgrsResponseStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetWorkGroup' smart constructor.
newtype GetWorkGroup = GetWorkGroup'
  { -- | The name of the workgroup.
    workGroup :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetWorkGroup' with the minimum fields required to make a request.
--
-- * 'workGroup' - The name of the workgroup.
mkGetWorkGroup ::
  -- | 'workGroup'
  Lude.Text ->
  GetWorkGroup
mkGetWorkGroup pWorkGroup_ = GetWorkGroup' {workGroup = pWorkGroup_}

-- | The name of the workgroup.
--
-- /Note:/ Consider using 'workGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwgWorkGroup :: Lens.Lens' GetWorkGroup Lude.Text
gwgWorkGroup = Lens.lens (workGroup :: GetWorkGroup -> Lude.Text) (\s a -> s {workGroup = a} :: GetWorkGroup)
{-# DEPRECATED gwgWorkGroup "Use generic-lens or generic-optics with 'workGroup' instead." #-}

instance Lude.AWSRequest GetWorkGroup where
  type Rs GetWorkGroup = GetWorkGroupResponse
  request = Req.postJSON athenaService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetWorkGroupResponse'
            Lude.<$> (x Lude..?> "WorkGroup") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetWorkGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonAthena.GetWorkGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetWorkGroup where
  toJSON GetWorkGroup' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("WorkGroup" Lude..= workGroup)])

instance Lude.ToPath GetWorkGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery GetWorkGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetWorkGroupResponse' smart constructor.
data GetWorkGroupResponse = GetWorkGroupResponse'
  { -- | Information about the workgroup.
    workGroup :: Lude.Maybe WorkGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetWorkGroupResponse' with the minimum fields required to make a request.
--
-- * 'workGroup' - Information about the workgroup.
-- * 'responseStatus' - The response status code.
mkGetWorkGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetWorkGroupResponse
mkGetWorkGroupResponse pResponseStatus_ =
  GetWorkGroupResponse'
    { workGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the workgroup.
--
-- /Note:/ Consider using 'workGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwgrsWorkGroup :: Lens.Lens' GetWorkGroupResponse (Lude.Maybe WorkGroup)
gwgrsWorkGroup = Lens.lens (workGroup :: GetWorkGroupResponse -> Lude.Maybe WorkGroup) (\s a -> s {workGroup = a} :: GetWorkGroupResponse)
{-# DEPRECATED gwgrsWorkGroup "Use generic-lens or generic-optics with 'workGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwgrsResponseStatus :: Lens.Lens' GetWorkGroupResponse Lude.Int
gwgrsResponseStatus = Lens.lens (responseStatus :: GetWorkGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetWorkGroupResponse)
{-# DEPRECATED gwgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
