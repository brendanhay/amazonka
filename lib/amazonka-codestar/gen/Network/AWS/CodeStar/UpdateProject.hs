{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    uName,
    uId,
    uDescription,

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
  { -- | The name of the project you want to update.
    name :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The ID of the project you want to update.
    id :: Lude.Text,
    -- | The description of the project, if any.
    description :: Lude.Maybe (Lude.Sensitive Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateProject' with the minimum fields required to make a request.
--
-- * 'name' - The name of the project you want to update.
-- * 'id' - The ID of the project you want to update.
-- * 'description' - The description of the project, if any.
mkUpdateProject ::
  -- | 'id'
  Lude.Text ->
  UpdateProject
mkUpdateProject pId_ =
  UpdateProject'
    { name = Lude.Nothing,
      id = pId_,
      description = Lude.Nothing
    }

-- | The name of the project you want to update.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uName :: Lens.Lens' UpdateProject (Lude.Maybe (Lude.Sensitive Lude.Text))
uName = Lens.lens (name :: UpdateProject -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {name = a} :: UpdateProject)
{-# DEPRECATED uName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the project you want to update.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uId :: Lens.Lens' UpdateProject Lude.Text
uId = Lens.lens (id :: UpdateProject -> Lude.Text) (\s a -> s {id = a} :: UpdateProject)
{-# DEPRECATED uId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The description of the project, if any.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uDescription :: Lens.Lens' UpdateProject (Lude.Maybe (Lude.Sensitive Lude.Text))
uDescription = Lens.lens (description :: UpdateProject -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {description = a} :: UpdateProject)
{-# DEPRECATED uDescription "Use generic-lens or generic-optics with 'description' instead." #-}

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
            Lude.Just ("id" Lude..= id),
            ("description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath UpdateProject where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateProject where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateProjectResponse' smart constructor.
newtype UpdateProjectResponse = UpdateProjectResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
