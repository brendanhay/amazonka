{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.UpdateProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an existing project.
module Network.AWS.Mobile.UpdateProject
  ( -- * Creating a request
    UpdateProject (..),
    mkUpdateProject,

    -- ** Request lenses
    upContents,
    upProjectId,

    -- * Destructuring the response
    UpdateProjectResponse (..),
    mkUpdateProjectResponse,

    -- ** Response lenses
    uprsDetails,
    uprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Mobile.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request structure used for requests to update project configuration.
--
-- /See:/ 'mkUpdateProject' smart constructor.
data UpdateProject = UpdateProject'
  { contents ::
      Lude.Maybe Lude.ByteString,
    projectId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateProject' with the minimum fields required to make a request.
--
-- * 'contents' - ZIP or YAML file which contains project configuration to be updated. This should be the contents of the file downloaded from the URL provided in an export project operation.
-- * 'projectId' - Unique project identifier.
mkUpdateProject ::
  -- | 'projectId'
  Lude.Text ->
  UpdateProject
mkUpdateProject pProjectId_ =
  UpdateProject' {contents = Lude.Nothing, projectId = pProjectId_}

-- | ZIP or YAML file which contains project configuration to be updated. This should be the contents of the file downloaded from the URL provided in an export project operation.
--
-- /Note:/ Consider using 'contents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upContents :: Lens.Lens' UpdateProject (Lude.Maybe Lude.ByteString)
upContents = Lens.lens (contents :: UpdateProject -> Lude.Maybe Lude.ByteString) (\s a -> s {contents = a} :: UpdateProject)
{-# DEPRECATED upContents "Use generic-lens or generic-optics with 'contents' instead." #-}

-- | Unique project identifier.
--
-- /Note:/ Consider using 'projectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upProjectId :: Lens.Lens' UpdateProject Lude.Text
upProjectId = Lens.lens (projectId :: UpdateProject -> Lude.Text) (\s a -> s {projectId = a} :: UpdateProject)
{-# DEPRECATED upProjectId "Use generic-lens or generic-optics with 'projectId' instead." #-}

instance Lude.AWSRequest UpdateProject where
  type Rs UpdateProject = UpdateProjectResponse
  request = Req.postBody mobileService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateProjectResponse'
            Lude.<$> (x Lude..?> "details") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToBody UpdateProject where
  toBody = Lude.toBody Lude.. contents

instance Lude.ToHeaders UpdateProject where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath UpdateProject where
  toPath = Lude.const "/update"

instance Lude.ToQuery UpdateProject where
  toQuery UpdateProject' {..} =
    Lude.mconcat ["projectId" Lude.=: projectId]

-- | Result structure used for requests to updated project configuration.
--
-- /See:/ 'mkUpdateProjectResponse' smart constructor.
data UpdateProjectResponse = UpdateProjectResponse'
  { details ::
      Lude.Maybe ProjectDetails,
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

-- | Creates a value of 'UpdateProjectResponse' with the minimum fields required to make a request.
--
-- * 'details' - Detailed information about the updated AWS Mobile Hub project.
-- * 'responseStatus' - The response status code.
mkUpdateProjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateProjectResponse
mkUpdateProjectResponse pResponseStatus_ =
  UpdateProjectResponse'
    { details = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Detailed information about the updated AWS Mobile Hub project.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsDetails :: Lens.Lens' UpdateProjectResponse (Lude.Maybe ProjectDetails)
uprsDetails = Lens.lens (details :: UpdateProjectResponse -> Lude.Maybe ProjectDetails) (\s a -> s {details = a} :: UpdateProjectResponse)
{-# DEPRECATED uprsDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsResponseStatus :: Lens.Lens' UpdateProjectResponse Lude.Int
uprsResponseStatus = Lens.lens (responseStatus :: UpdateProjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateProjectResponse)
{-# DEPRECATED uprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
