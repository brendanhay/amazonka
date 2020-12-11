{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetTestGridProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a Selenium testing project.
module Network.AWS.DeviceFarm.GetTestGridProject
  ( -- * Creating a request
    GetTestGridProject (..),
    mkGetTestGridProject,

    -- ** Request lenses
    gtgpProjectARN,

    -- * Destructuring the response
    GetTestGridProjectResponse (..),
    mkGetTestGridProjectResponse,

    -- ** Response lenses
    gtgprsTestGridProject,
    gtgprsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetTestGridProject' smart constructor.
newtype GetTestGridProject = GetTestGridProject'
  { projectARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTestGridProject' with the minimum fields required to make a request.
--
-- * 'projectARN' - The ARN of the Selenium testing project, from either 'CreateTestGridProject' or 'ListTestGridProjects' .
mkGetTestGridProject ::
  -- | 'projectARN'
  Lude.Text ->
  GetTestGridProject
mkGetTestGridProject pProjectARN_ =
  GetTestGridProject' {projectARN = pProjectARN_}

-- | The ARN of the Selenium testing project, from either 'CreateTestGridProject' or 'ListTestGridProjects' .
--
-- /Note:/ Consider using 'projectARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgpProjectARN :: Lens.Lens' GetTestGridProject Lude.Text
gtgpProjectARN = Lens.lens (projectARN :: GetTestGridProject -> Lude.Text) (\s a -> s {projectARN = a} :: GetTestGridProject)
{-# DEPRECATED gtgpProjectARN "Use generic-lens or generic-optics with 'projectARN' instead." #-}

instance Lude.AWSRequest GetTestGridProject where
  type Rs GetTestGridProject = GetTestGridProjectResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetTestGridProjectResponse'
            Lude.<$> (x Lude..?> "testGridProject")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTestGridProject where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.GetTestGridProject" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetTestGridProject where
  toJSON GetTestGridProject' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("projectArn" Lude..= projectARN)])

instance Lude.ToPath GetTestGridProject where
  toPath = Lude.const "/"

instance Lude.ToQuery GetTestGridProject where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetTestGridProjectResponse' smart constructor.
data GetTestGridProjectResponse = GetTestGridProjectResponse'
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

-- | Creates a value of 'GetTestGridProjectResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'testGridProject' - A 'TestGridProject' .
mkGetTestGridProjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTestGridProjectResponse
mkGetTestGridProjectResponse pResponseStatus_ =
  GetTestGridProjectResponse'
    { testGridProject = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A 'TestGridProject' .
--
-- /Note:/ Consider using 'testGridProject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgprsTestGridProject :: Lens.Lens' GetTestGridProjectResponse (Lude.Maybe TestGridProject)
gtgprsTestGridProject = Lens.lens (testGridProject :: GetTestGridProjectResponse -> Lude.Maybe TestGridProject) (\s a -> s {testGridProject = a} :: GetTestGridProjectResponse)
{-# DEPRECATED gtgprsTestGridProject "Use generic-lens or generic-optics with 'testGridProject' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgprsResponseStatus :: Lens.Lens' GetTestGridProjectResponse Lude.Int
gtgprsResponseStatus = Lens.lens (responseStatus :: GetTestGridProjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTestGridProjectResponse)
{-# DEPRECATED gtgprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
