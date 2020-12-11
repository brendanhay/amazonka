{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.UpdateTestGridProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Change details of a project.
module Network.AWS.DeviceFarm.UpdateTestGridProject
  ( -- * Creating a request
    UpdateTestGridProject (..),
    mkUpdateTestGridProject,

    -- ** Request lenses
    utgpName,
    utgpDescription,
    utgpProjectARN,

    -- * Destructuring the response
    UpdateTestGridProjectResponse (..),
    mkUpdateTestGridProjectResponse,

    -- ** Response lenses
    utgprsTestGridProject,
    utgprsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateTestGridProject' smart constructor.
data UpdateTestGridProject = UpdateTestGridProject'
  { name ::
      Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    projectARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTestGridProject' with the minimum fields required to make a request.
--
-- * 'description' - Human-readable description for the project.
-- * 'name' - Human-readable name for the project.
-- * 'projectARN' - ARN of the project to update.
mkUpdateTestGridProject ::
  -- | 'projectARN'
  Lude.Text ->
  UpdateTestGridProject
mkUpdateTestGridProject pProjectARN_ =
  UpdateTestGridProject'
    { name = Lude.Nothing,
      description = Lude.Nothing,
      projectARN = pProjectARN_
    }

-- | Human-readable name for the project.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utgpName :: Lens.Lens' UpdateTestGridProject (Lude.Maybe Lude.Text)
utgpName = Lens.lens (name :: UpdateTestGridProject -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateTestGridProject)
{-# DEPRECATED utgpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Human-readable description for the project.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utgpDescription :: Lens.Lens' UpdateTestGridProject (Lude.Maybe Lude.Text)
utgpDescription = Lens.lens (description :: UpdateTestGridProject -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateTestGridProject)
{-# DEPRECATED utgpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | ARN of the project to update.
--
-- /Note:/ Consider using 'projectARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utgpProjectARN :: Lens.Lens' UpdateTestGridProject Lude.Text
utgpProjectARN = Lens.lens (projectARN :: UpdateTestGridProject -> Lude.Text) (\s a -> s {projectARN = a} :: UpdateTestGridProject)
{-# DEPRECATED utgpProjectARN "Use generic-lens or generic-optics with 'projectARN' instead." #-}

instance Lude.AWSRequest UpdateTestGridProject where
  type Rs UpdateTestGridProject = UpdateTestGridProjectResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateTestGridProjectResponse'
            Lude.<$> (x Lude..?> "testGridProject")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateTestGridProject where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.UpdateTestGridProject" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateTestGridProject where
  toJSON UpdateTestGridProject' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("name" Lude..=) Lude.<$> name,
            ("description" Lude..=) Lude.<$> description,
            Lude.Just ("projectArn" Lude..= projectARN)
          ]
      )

instance Lude.ToPath UpdateTestGridProject where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateTestGridProject where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateTestGridProjectResponse' smart constructor.
data UpdateTestGridProjectResponse = UpdateTestGridProjectResponse'
  { testGridProject ::
      Lude.Maybe TestGridProject,
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

-- | Creates a value of 'UpdateTestGridProjectResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'testGridProject' - The project, including updated information.
mkUpdateTestGridProjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateTestGridProjectResponse
mkUpdateTestGridProjectResponse pResponseStatus_ =
  UpdateTestGridProjectResponse'
    { testGridProject = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The project, including updated information.
--
-- /Note:/ Consider using 'testGridProject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utgprsTestGridProject :: Lens.Lens' UpdateTestGridProjectResponse (Lude.Maybe TestGridProject)
utgprsTestGridProject = Lens.lens (testGridProject :: UpdateTestGridProjectResponse -> Lude.Maybe TestGridProject) (\s a -> s {testGridProject = a} :: UpdateTestGridProjectResponse)
{-# DEPRECATED utgprsTestGridProject "Use generic-lens or generic-optics with 'testGridProject' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utgprsResponseStatus :: Lens.Lens' UpdateTestGridProjectResponse Lude.Int
utgprsResponseStatus = Lens.lens (responseStatus :: UpdateTestGridProjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateTestGridProjectResponse)
{-# DEPRECATED utgprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
