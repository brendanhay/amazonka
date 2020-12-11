{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.DescribeProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a project in AWS Mobile Hub.
module Network.AWS.Mobile.DescribeProject
  ( -- * Creating a request
    DescribeProject (..),
    mkDescribeProject,

    -- ** Request lenses
    dSyncFromResources,
    dProjectId,

    -- * Destructuring the response
    DescribeProjectResponse (..),
    mkDescribeProjectResponse,

    -- ** Response lenses
    drsDetails,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Mobile.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request structure used to request details about a project.
--
-- /See:/ 'mkDescribeProject' smart constructor.
data DescribeProject = DescribeProject'
  { syncFromResources ::
      Lude.Maybe Lude.Bool,
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

-- | Creates a value of 'DescribeProject' with the minimum fields required to make a request.
--
-- * 'projectId' - Unique project identifier.
-- * 'syncFromResources' - If set to true, causes AWS Mobile Hub to synchronize information from other services, e.g., update state of AWS CloudFormation stacks in the AWS Mobile Hub project.
mkDescribeProject ::
  -- | 'projectId'
  Lude.Text ->
  DescribeProject
mkDescribeProject pProjectId_ =
  DescribeProject'
    { syncFromResources = Lude.Nothing,
      projectId = pProjectId_
    }

-- | If set to true, causes AWS Mobile Hub to synchronize information from other services, e.g., update state of AWS CloudFormation stacks in the AWS Mobile Hub project.
--
-- /Note:/ Consider using 'syncFromResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSyncFromResources :: Lens.Lens' DescribeProject (Lude.Maybe Lude.Bool)
dSyncFromResources = Lens.lens (syncFromResources :: DescribeProject -> Lude.Maybe Lude.Bool) (\s a -> s {syncFromResources = a} :: DescribeProject)
{-# DEPRECATED dSyncFromResources "Use generic-lens or generic-optics with 'syncFromResources' instead." #-}

-- | Unique project identifier.
--
-- /Note:/ Consider using 'projectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dProjectId :: Lens.Lens' DescribeProject Lude.Text
dProjectId = Lens.lens (projectId :: DescribeProject -> Lude.Text) (\s a -> s {projectId = a} :: DescribeProject)
{-# DEPRECATED dProjectId "Use generic-lens or generic-optics with 'projectId' instead." #-}

instance Lude.AWSRequest DescribeProject where
  type Rs DescribeProject = DescribeProjectResponse
  request = Req.get mobileService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeProjectResponse'
            Lude.<$> (x Lude..?> "details") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeProject where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeProject where
  toPath = Lude.const "/project"

instance Lude.ToQuery DescribeProject where
  toQuery DescribeProject' {..} =
    Lude.mconcat
      [ "syncFromResources" Lude.=: syncFromResources,
        "projectId" Lude.=: projectId
      ]

-- | Result structure used for requests of project details.
--
-- /See:/ 'mkDescribeProjectResponse' smart constructor.
data DescribeProjectResponse = DescribeProjectResponse'
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

-- | Creates a value of 'DescribeProjectResponse' with the minimum fields required to make a request.
--
-- * 'details' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDescribeProjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeProjectResponse
mkDescribeProjectResponse pResponseStatus_ =
  DescribeProjectResponse'
    { details = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDetails :: Lens.Lens' DescribeProjectResponse (Lude.Maybe ProjectDetails)
drsDetails = Lens.lens (details :: DescribeProjectResponse -> Lude.Maybe ProjectDetails) (\s a -> s {details = a} :: DescribeProjectResponse)
{-# DEPRECATED drsDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeProjectResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeProjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeProjectResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
