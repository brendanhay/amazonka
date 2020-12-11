{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.CreateTestGridProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Selenium testing project. Projects are used to track 'TestGridSession' instances.
module Network.AWS.DeviceFarm.CreateTestGridProject
  ( -- * Creating a request
    CreateTestGridProject (..),
    mkCreateTestGridProject,

    -- ** Request lenses
    ctgpDescription,
    ctgpName,

    -- * Destructuring the response
    CreateTestGridProjectResponse (..),
    mkCreateTestGridProjectResponse,

    -- ** Response lenses
    ctgprsTestGridProject,
    ctgprsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateTestGridProject' smart constructor.
data CreateTestGridProject = CreateTestGridProject'
  { description ::
      Lude.Maybe Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTestGridProject' with the minimum fields required to make a request.
--
-- * 'description' - Human-readable description of the project.
-- * 'name' - Human-readable name of the Selenium testing project.
mkCreateTestGridProject ::
  -- | 'name'
  Lude.Text ->
  CreateTestGridProject
mkCreateTestGridProject pName_ =
  CreateTestGridProject' {description = Lude.Nothing, name = pName_}

-- | Human-readable description of the project.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgpDescription :: Lens.Lens' CreateTestGridProject (Lude.Maybe Lude.Text)
ctgpDescription = Lens.lens (description :: CreateTestGridProject -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateTestGridProject)
{-# DEPRECATED ctgpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Human-readable name of the Selenium testing project.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgpName :: Lens.Lens' CreateTestGridProject Lude.Text
ctgpName = Lens.lens (name :: CreateTestGridProject -> Lude.Text) (\s a -> s {name = a} :: CreateTestGridProject)
{-# DEPRECATED ctgpName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest CreateTestGridProject where
  type Rs CreateTestGridProject = CreateTestGridProjectResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateTestGridProjectResponse'
            Lude.<$> (x Lude..?> "testGridProject")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTestGridProject where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.CreateTestGridProject" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateTestGridProject where
  toJSON CreateTestGridProject' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("description" Lude..=) Lude.<$> description,
            Lude.Just ("name" Lude..= name)
          ]
      )

instance Lude.ToPath CreateTestGridProject where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTestGridProject where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateTestGridProjectResponse' smart constructor.
data CreateTestGridProjectResponse = CreateTestGridProjectResponse'
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

-- | Creates a value of 'CreateTestGridProjectResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'testGridProject' - ARN of the Selenium testing project that was created.
mkCreateTestGridProjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTestGridProjectResponse
mkCreateTestGridProjectResponse pResponseStatus_ =
  CreateTestGridProjectResponse'
    { testGridProject = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | ARN of the Selenium testing project that was created.
--
-- /Note:/ Consider using 'testGridProject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgprsTestGridProject :: Lens.Lens' CreateTestGridProjectResponse (Lude.Maybe TestGridProject)
ctgprsTestGridProject = Lens.lens (testGridProject :: CreateTestGridProjectResponse -> Lude.Maybe TestGridProject) (\s a -> s {testGridProject = a} :: CreateTestGridProjectResponse)
{-# DEPRECATED ctgprsTestGridProject "Use generic-lens or generic-optics with 'testGridProject' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgprsResponseStatus :: Lens.Lens' CreateTestGridProjectResponse Lude.Int
ctgprsResponseStatus = Lens.lens (responseStatus :: CreateTestGridProjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTestGridProjectResponse)
{-# DEPRECATED ctgprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
