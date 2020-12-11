{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteLaunchTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a launch template. Deleting a launch template deletes all of its versions.
module Network.AWS.EC2.DeleteLaunchTemplate
  ( -- * Creating a request
    DeleteLaunchTemplate (..),
    mkDeleteLaunchTemplate,

    -- ** Request lenses
    dltLaunchTemplateName,
    dltLaunchTemplateId,
    dltDryRun,

    -- * Destructuring the response
    DeleteLaunchTemplateResponse (..),
    mkDeleteLaunchTemplateResponse,

    -- ** Response lenses
    dltrsLaunchTemplate,
    dltrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteLaunchTemplate' smart constructor.
data DeleteLaunchTemplate = DeleteLaunchTemplate'
  { launchTemplateName ::
      Lude.Maybe Lude.Text,
    launchTemplateId :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLaunchTemplate' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'launchTemplateId' - The ID of the launch template. You must specify either the launch template ID or launch template name in the request.
-- * 'launchTemplateName' - The name of the launch template. You must specify either the launch template ID or launch template name in the request.
mkDeleteLaunchTemplate ::
  DeleteLaunchTemplate
mkDeleteLaunchTemplate =
  DeleteLaunchTemplate'
    { launchTemplateName = Lude.Nothing,
      launchTemplateId = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The name of the launch template. You must specify either the launch template ID or launch template name in the request.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltLaunchTemplateName :: Lens.Lens' DeleteLaunchTemplate (Lude.Maybe Lude.Text)
dltLaunchTemplateName = Lens.lens (launchTemplateName :: DeleteLaunchTemplate -> Lude.Maybe Lude.Text) (\s a -> s {launchTemplateName = a} :: DeleteLaunchTemplate)
{-# DEPRECATED dltLaunchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead." #-}

-- | The ID of the launch template. You must specify either the launch template ID or launch template name in the request.
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltLaunchTemplateId :: Lens.Lens' DeleteLaunchTemplate (Lude.Maybe Lude.Text)
dltLaunchTemplateId = Lens.lens (launchTemplateId :: DeleteLaunchTemplate -> Lude.Maybe Lude.Text) (\s a -> s {launchTemplateId = a} :: DeleteLaunchTemplate)
{-# DEPRECATED dltLaunchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltDryRun :: Lens.Lens' DeleteLaunchTemplate (Lude.Maybe Lude.Bool)
dltDryRun = Lens.lens (dryRun :: DeleteLaunchTemplate -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteLaunchTemplate)
{-# DEPRECATED dltDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DeleteLaunchTemplate where
  type Rs DeleteLaunchTemplate = DeleteLaunchTemplateResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeleteLaunchTemplateResponse'
            Lude.<$> (x Lude..@? "launchTemplate")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteLaunchTemplate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteLaunchTemplate where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteLaunchTemplate where
  toQuery DeleteLaunchTemplate' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteLaunchTemplate" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "LaunchTemplateName" Lude.=: launchTemplateName,
        "LaunchTemplateId" Lude.=: launchTemplateId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDeleteLaunchTemplateResponse' smart constructor.
data DeleteLaunchTemplateResponse = DeleteLaunchTemplateResponse'
  { launchTemplate ::
      Lude.Maybe LaunchTemplate,
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

-- | Creates a value of 'DeleteLaunchTemplateResponse' with the minimum fields required to make a request.
--
-- * 'launchTemplate' - Information about the launch template.
-- * 'responseStatus' - The response status code.
mkDeleteLaunchTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteLaunchTemplateResponse
mkDeleteLaunchTemplateResponse pResponseStatus_ =
  DeleteLaunchTemplateResponse'
    { launchTemplate = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the launch template.
--
-- /Note:/ Consider using 'launchTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltrsLaunchTemplate :: Lens.Lens' DeleteLaunchTemplateResponse (Lude.Maybe LaunchTemplate)
dltrsLaunchTemplate = Lens.lens (launchTemplate :: DeleteLaunchTemplateResponse -> Lude.Maybe LaunchTemplate) (\s a -> s {launchTemplate = a} :: DeleteLaunchTemplateResponse)
{-# DEPRECATED dltrsLaunchTemplate "Use generic-lens or generic-optics with 'launchTemplate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltrsResponseStatus :: Lens.Lens' DeleteLaunchTemplateResponse Lude.Int
dltrsResponseStatus = Lens.lens (responseStatus :: DeleteLaunchTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteLaunchTemplateResponse)
{-# DEPRECATED dltrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
