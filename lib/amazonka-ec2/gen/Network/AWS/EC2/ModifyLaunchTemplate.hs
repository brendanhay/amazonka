{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyLaunchTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a launch template. You can specify which version of the launch template to set as the default version. When launching an instance, the default version applies when a launch template version is not specified.
module Network.AWS.EC2.ModifyLaunchTemplate
  ( -- * Creating a request
    ModifyLaunchTemplate (..),
    mkModifyLaunchTemplate,

    -- ** Request lenses
    mltLaunchTemplateName,
    mltClientToken,
    mltLaunchTemplateId,
    mltDefaultVersion,
    mltDryRun,

    -- * Destructuring the response
    ModifyLaunchTemplateResponse (..),
    mkModifyLaunchTemplateResponse,

    -- ** Response lenses
    mltrsLaunchTemplate,
    mltrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyLaunchTemplate' smart constructor.
data ModifyLaunchTemplate = ModifyLaunchTemplate'
  { launchTemplateName ::
      Lude.Maybe Lude.Text,
    clientToken :: Lude.Maybe Lude.Text,
    launchTemplateId :: Lude.Maybe Lude.Text,
    defaultVersion :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ModifyLaunchTemplate' with the minimum fields required to make a request.
--
-- * 'clientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- Constraint: Maximum 128 ASCII characters.
-- * 'defaultVersion' - The version number of the launch template to set as the default version.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'launchTemplateId' - The ID of the launch template. You must specify either the launch template ID or launch template name in the request.
-- * 'launchTemplateName' - The name of the launch template. You must specify either the launch template ID or launch template name in the request.
mkModifyLaunchTemplate ::
  ModifyLaunchTemplate
mkModifyLaunchTemplate =
  ModifyLaunchTemplate'
    { launchTemplateName = Lude.Nothing,
      clientToken = Lude.Nothing,
      launchTemplateId = Lude.Nothing,
      defaultVersion = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The name of the launch template. You must specify either the launch template ID or launch template name in the request.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mltLaunchTemplateName :: Lens.Lens' ModifyLaunchTemplate (Lude.Maybe Lude.Text)
mltLaunchTemplateName = Lens.lens (launchTemplateName :: ModifyLaunchTemplate -> Lude.Maybe Lude.Text) (\s a -> s {launchTemplateName = a} :: ModifyLaunchTemplate)
{-# DEPRECATED mltLaunchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead." #-}

-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- Constraint: Maximum 128 ASCII characters.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mltClientToken :: Lens.Lens' ModifyLaunchTemplate (Lude.Maybe Lude.Text)
mltClientToken = Lens.lens (clientToken :: ModifyLaunchTemplate -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: ModifyLaunchTemplate)
{-# DEPRECATED mltClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The ID of the launch template. You must specify either the launch template ID or launch template name in the request.
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mltLaunchTemplateId :: Lens.Lens' ModifyLaunchTemplate (Lude.Maybe Lude.Text)
mltLaunchTemplateId = Lens.lens (launchTemplateId :: ModifyLaunchTemplate -> Lude.Maybe Lude.Text) (\s a -> s {launchTemplateId = a} :: ModifyLaunchTemplate)
{-# DEPRECATED mltLaunchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead." #-}

-- | The version number of the launch template to set as the default version.
--
-- /Note:/ Consider using 'defaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mltDefaultVersion :: Lens.Lens' ModifyLaunchTemplate (Lude.Maybe Lude.Text)
mltDefaultVersion = Lens.lens (defaultVersion :: ModifyLaunchTemplate -> Lude.Maybe Lude.Text) (\s a -> s {defaultVersion = a} :: ModifyLaunchTemplate)
{-# DEPRECATED mltDefaultVersion "Use generic-lens or generic-optics with 'defaultVersion' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mltDryRun :: Lens.Lens' ModifyLaunchTemplate (Lude.Maybe Lude.Bool)
mltDryRun = Lens.lens (dryRun :: ModifyLaunchTemplate -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyLaunchTemplate)
{-# DEPRECATED mltDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest ModifyLaunchTemplate where
  type Rs ModifyLaunchTemplate = ModifyLaunchTemplateResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyLaunchTemplateResponse'
            Lude.<$> (x Lude..@? "launchTemplate")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyLaunchTemplate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyLaunchTemplate where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyLaunchTemplate where
  toQuery ModifyLaunchTemplate' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyLaunchTemplate" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "LaunchTemplateName" Lude.=: launchTemplateName,
        "ClientToken" Lude.=: clientToken,
        "LaunchTemplateId" Lude.=: launchTemplateId,
        "SetDefaultVersion" Lude.=: defaultVersion,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkModifyLaunchTemplateResponse' smart constructor.
data ModifyLaunchTemplateResponse = ModifyLaunchTemplateResponse'
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

-- | Creates a value of 'ModifyLaunchTemplateResponse' with the minimum fields required to make a request.
--
-- * 'launchTemplate' - Information about the launch template.
-- * 'responseStatus' - The response status code.
mkModifyLaunchTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyLaunchTemplateResponse
mkModifyLaunchTemplateResponse pResponseStatus_ =
  ModifyLaunchTemplateResponse'
    { launchTemplate = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the launch template.
--
-- /Note:/ Consider using 'launchTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mltrsLaunchTemplate :: Lens.Lens' ModifyLaunchTemplateResponse (Lude.Maybe LaunchTemplate)
mltrsLaunchTemplate = Lens.lens (launchTemplate :: ModifyLaunchTemplateResponse -> Lude.Maybe LaunchTemplate) (\s a -> s {launchTemplate = a} :: ModifyLaunchTemplateResponse)
{-# DEPRECATED mltrsLaunchTemplate "Use generic-lens or generic-optics with 'launchTemplate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mltrsResponseStatus :: Lens.Lens' ModifyLaunchTemplateResponse Lude.Int
mltrsResponseStatus = Lens.lens (responseStatus :: ModifyLaunchTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyLaunchTemplateResponse)
{-# DEPRECATED mltrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
