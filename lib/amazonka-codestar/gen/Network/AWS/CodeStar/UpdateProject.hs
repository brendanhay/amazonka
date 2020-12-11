{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.UpdateProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a project in AWS CodeStar.
module Network.AWS.CodeStar.UpdateProject
  ( -- * Creating a request
    UpdateProject (..),
    mkUpdateProject,

    -- ** Request lenses
    upName,
    upDescription,
    upId,

    -- * Destructuring the response
    UpdateProjectResponse (..),
    mkUpdateProjectResponse,

    -- ** Response lenses
    uprsResponseStatus,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateProject' smart constructor.
data UpdateProject = UpdateProject'
  { name ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    description :: Lude.Maybe (Lude.Sensitive Lude.Text),
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateProject' with the minimum fields required to make a request.
--
-- * 'description' - The description of the project, if any.
-- * 'id' - The ID of the project you want to update.
-- * 'name' - The name of the project you want to update.
mkUpdateProject ::
  -- | 'id'
  Lude.Text ->
  UpdateProject
mkUpdateProject pId_ =
  UpdateProject'
    { name = Lude.Nothing,
      description = Lude.Nothing,
      id = pId_
    }

-- | The name of the project you want to update.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upName :: Lens.Lens' UpdateProject (Lude.Maybe (Lude.Sensitive Lude.Text))
upName = Lens.lens (name :: UpdateProject -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {name = a} :: UpdateProject)
{-# DEPRECATED upName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The description of the project, if any.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upDescription :: Lens.Lens' UpdateProject (Lude.Maybe (Lude.Sensitive Lude.Text))
upDescription = Lens.lens (description :: UpdateProject -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {description = a} :: UpdateProject)
{-# DEPRECATED upDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID of the project you want to update.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upId :: Lens.Lens' UpdateProject Lude.Text
upId = Lens.lens (id :: UpdateProject -> Lude.Text) (\s a -> s {id = a} :: UpdateProject)
{-# DEPRECATED upId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest UpdateProject where
  type Rs UpdateProject = UpdateProjectResponse
  request = Req.postJSON codeStarService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateProjectResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateProject where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeStar_20170419.UpdateProject" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateProject where
  toJSON UpdateProject' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("name" Lude..=) Lude.<$> name,
            ("description" Lude..=) Lude.<$> description,
            Lude.Just ("id" Lude..= id)
          ]
      )

instance Lude.ToPath UpdateProject where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateProject where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateProjectResponse' smart constructor.
newtype UpdateProjectResponse = UpdateProjectResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateProjectResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateProjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateProjectResponse
mkUpdateProjectResponse pResponseStatus_ =
  UpdateProjectResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsResponseStatus :: Lens.Lens' UpdateProjectResponse Lude.Int
uprsResponseStatus = Lens.lens (responseStatus :: UpdateProjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateProjectResponse)
{-# DEPRECATED uprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
