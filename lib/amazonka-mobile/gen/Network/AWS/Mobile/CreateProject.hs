{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.CreateProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Mobile Hub project.
module Network.AWS.Mobile.CreateProject
  ( -- * Creating a request
    CreateProject (..),
    mkCreateProject,

    -- ** Request lenses
    cpContents,
    cpName,
    cpRegion,
    cpSnapshotId,

    -- * Destructuring the response
    CreateProjectResponse (..),
    mkCreateProjectResponse,

    -- ** Response lenses
    cprsDetails,
    cprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Mobile.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request structure used to request a project be created.
--
-- /See:/ 'mkCreateProject' smart constructor.
data CreateProject = CreateProject'
  { -- | ZIP or YAML file which contains configuration settings to be used when creating the project. This may be the contents of the file downloaded from the URL provided in an export project operation.
    contents :: Lude.Maybe Lude.ByteString,
    -- | Name of the project.
    name :: Lude.Maybe Lude.Text,
    -- | Default region where project resources should be created.
    region :: Lude.Maybe Lude.Text,
    -- | Unique identifier for an exported snapshot of project configuration. This snapshot identifier is included in the share URL when a project is exported.
    snapshotId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateProject' with the minimum fields required to make a request.
--
-- * 'contents' - ZIP or YAML file which contains configuration settings to be used when creating the project. This may be the contents of the file downloaded from the URL provided in an export project operation.
-- * 'name' - Name of the project.
-- * 'region' - Default region where project resources should be created.
-- * 'snapshotId' - Unique identifier for an exported snapshot of project configuration. This snapshot identifier is included in the share URL when a project is exported.
mkCreateProject ::
  CreateProject
mkCreateProject =
  CreateProject'
    { contents = Lude.Nothing,
      name = Lude.Nothing,
      region = Lude.Nothing,
      snapshotId = Lude.Nothing
    }

-- | ZIP or YAML file which contains configuration settings to be used when creating the project. This may be the contents of the file downloaded from the URL provided in an export project operation.
--
-- /Note:/ Consider using 'contents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpContents :: Lens.Lens' CreateProject (Lude.Maybe Lude.ByteString)
cpContents = Lens.lens (contents :: CreateProject -> Lude.Maybe Lude.ByteString) (\s a -> s {contents = a} :: CreateProject)
{-# DEPRECATED cpContents "Use generic-lens or generic-optics with 'contents' instead." #-}

-- | Name of the project.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpName :: Lens.Lens' CreateProject (Lude.Maybe Lude.Text)
cpName = Lens.lens (name :: CreateProject -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateProject)
{-# DEPRECATED cpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Default region where project resources should be created.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpRegion :: Lens.Lens' CreateProject (Lude.Maybe Lude.Text)
cpRegion = Lens.lens (region :: CreateProject -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: CreateProject)
{-# DEPRECATED cpRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | Unique identifier for an exported snapshot of project configuration. This snapshot identifier is included in the share URL when a project is exported.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSnapshotId :: Lens.Lens' CreateProject (Lude.Maybe Lude.Text)
cpSnapshotId = Lens.lens (snapshotId :: CreateProject -> Lude.Maybe Lude.Text) (\s a -> s {snapshotId = a} :: CreateProject)
{-# DEPRECATED cpSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

instance Lude.AWSRequest CreateProject where
  type Rs CreateProject = CreateProjectResponse
  request = Req.postBody mobileService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateProjectResponse'
            Lude.<$> (x Lude..?> "details") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToBody CreateProject where
  toBody = Lude.toBody Lude.. contents

instance Lude.ToHeaders CreateProject where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath CreateProject where
  toPath = Lude.const "/projects"

instance Lude.ToQuery CreateProject where
  toQuery CreateProject' {..} =
    Lude.mconcat
      [ "name" Lude.=: name,
        "region" Lude.=: region,
        "snapshotId" Lude.=: snapshotId
      ]

-- | Result structure used in response to a request to create a project.
--
-- /See:/ 'mkCreateProjectResponse' smart constructor.
data CreateProjectResponse = CreateProjectResponse'
  { -- | Detailed information about the created AWS Mobile Hub project.
    details :: Lude.Maybe ProjectDetails,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateProjectResponse' with the minimum fields required to make a request.
--
-- * 'details' - Detailed information about the created AWS Mobile Hub project.
-- * 'responseStatus' - The response status code.
mkCreateProjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateProjectResponse
mkCreateProjectResponse pResponseStatus_ =
  CreateProjectResponse'
    { details = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Detailed information about the created AWS Mobile Hub project.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsDetails :: Lens.Lens' CreateProjectResponse (Lude.Maybe ProjectDetails)
cprsDetails = Lens.lens (details :: CreateProjectResponse -> Lude.Maybe ProjectDetails) (\s a -> s {details = a} :: CreateProjectResponse)
{-# DEPRECATED cprsDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsResponseStatus :: Lens.Lens' CreateProjectResponse Lude.Int
cprsResponseStatus = Lens.lens (responseStatus :: CreateProjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateProjectResponse)
{-# DEPRECATED cprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
