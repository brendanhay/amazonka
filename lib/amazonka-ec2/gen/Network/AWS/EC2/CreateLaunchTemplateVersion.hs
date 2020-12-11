{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateLaunchTemplateVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version for a launch template. You can specify an existing version of launch template from which to base the new version.
--
-- Launch template versions are numbered in the order in which they are created. You cannot specify, change, or replace the numbering of launch template versions.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html#manage-launch-template-versions Managing launch template versions> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.CreateLaunchTemplateVersion
  ( -- * Creating a request
    CreateLaunchTemplateVersion (..),
    mkCreateLaunchTemplateVersion,

    -- ** Request lenses
    cltvLaunchTemplateName,
    cltvClientToken,
    cltvLaunchTemplateId,
    cltvVersionDescription,
    cltvSourceVersion,
    cltvDryRun,
    cltvLaunchTemplateData,

    -- * Destructuring the response
    CreateLaunchTemplateVersionResponse (..),
    mkCreateLaunchTemplateVersionResponse,

    -- ** Response lenses
    cltvrsLaunchTemplateVersion,
    cltvrsWarning,
    cltvrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateLaunchTemplateVersion' smart constructor.
data CreateLaunchTemplateVersion = CreateLaunchTemplateVersion'
  { launchTemplateName ::
      Lude.Maybe Lude.Text,
    clientToken :: Lude.Maybe Lude.Text,
    launchTemplateId ::
      Lude.Maybe Lude.Text,
    versionDescription ::
      Lude.Maybe Lude.Text,
    sourceVersion ::
      Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    launchTemplateData ::
      RequestLaunchTemplateData
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLaunchTemplateVersion' with the minimum fields required to make a request.
--
-- * 'clientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- Constraint: Maximum 128 ASCII characters.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'launchTemplateData' - The information for the launch template.
-- * 'launchTemplateId' - The ID of the launch template. You must specify either the launch template ID or launch template name in the request.
-- * 'launchTemplateName' - The name of the launch template. You must specify either the launch template ID or launch template name in the request.
-- * 'sourceVersion' - The version number of the launch template version on which to base the new version. The new version inherits the same launch parameters as the source version, except for parameters that you specify in @LaunchTemplateData@ . Snapshots applied to the block device mapping are ignored when creating a new version unless they are explicitly included.
-- * 'versionDescription' - A description for the version of the launch template.
mkCreateLaunchTemplateVersion ::
  -- | 'launchTemplateData'
  RequestLaunchTemplateData ->
  CreateLaunchTemplateVersion
mkCreateLaunchTemplateVersion pLaunchTemplateData_ =
  CreateLaunchTemplateVersion'
    { launchTemplateName = Lude.Nothing,
      clientToken = Lude.Nothing,
      launchTemplateId = Lude.Nothing,
      versionDescription = Lude.Nothing,
      sourceVersion = Lude.Nothing,
      dryRun = Lude.Nothing,
      launchTemplateData = pLaunchTemplateData_
    }

-- | The name of the launch template. You must specify either the launch template ID or launch template name in the request.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltvLaunchTemplateName :: Lens.Lens' CreateLaunchTemplateVersion (Lude.Maybe Lude.Text)
cltvLaunchTemplateName = Lens.lens (launchTemplateName :: CreateLaunchTemplateVersion -> Lude.Maybe Lude.Text) (\s a -> s {launchTemplateName = a} :: CreateLaunchTemplateVersion)
{-# DEPRECATED cltvLaunchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead." #-}

-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- Constraint: Maximum 128 ASCII characters.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltvClientToken :: Lens.Lens' CreateLaunchTemplateVersion (Lude.Maybe Lude.Text)
cltvClientToken = Lens.lens (clientToken :: CreateLaunchTemplateVersion -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateLaunchTemplateVersion)
{-# DEPRECATED cltvClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The ID of the launch template. You must specify either the launch template ID or launch template name in the request.
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltvLaunchTemplateId :: Lens.Lens' CreateLaunchTemplateVersion (Lude.Maybe Lude.Text)
cltvLaunchTemplateId = Lens.lens (launchTemplateId :: CreateLaunchTemplateVersion -> Lude.Maybe Lude.Text) (\s a -> s {launchTemplateId = a} :: CreateLaunchTemplateVersion)
{-# DEPRECATED cltvLaunchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead." #-}

-- | A description for the version of the launch template.
--
-- /Note:/ Consider using 'versionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltvVersionDescription :: Lens.Lens' CreateLaunchTemplateVersion (Lude.Maybe Lude.Text)
cltvVersionDescription = Lens.lens (versionDescription :: CreateLaunchTemplateVersion -> Lude.Maybe Lude.Text) (\s a -> s {versionDescription = a} :: CreateLaunchTemplateVersion)
{-# DEPRECATED cltvVersionDescription "Use generic-lens or generic-optics with 'versionDescription' instead." #-}

-- | The version number of the launch template version on which to base the new version. The new version inherits the same launch parameters as the source version, except for parameters that you specify in @LaunchTemplateData@ . Snapshots applied to the block device mapping are ignored when creating a new version unless they are explicitly included.
--
-- /Note:/ Consider using 'sourceVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltvSourceVersion :: Lens.Lens' CreateLaunchTemplateVersion (Lude.Maybe Lude.Text)
cltvSourceVersion = Lens.lens (sourceVersion :: CreateLaunchTemplateVersion -> Lude.Maybe Lude.Text) (\s a -> s {sourceVersion = a} :: CreateLaunchTemplateVersion)
{-# DEPRECATED cltvSourceVersion "Use generic-lens or generic-optics with 'sourceVersion' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltvDryRun :: Lens.Lens' CreateLaunchTemplateVersion (Lude.Maybe Lude.Bool)
cltvDryRun = Lens.lens (dryRun :: CreateLaunchTemplateVersion -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateLaunchTemplateVersion)
{-# DEPRECATED cltvDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The information for the launch template.
--
-- /Note:/ Consider using 'launchTemplateData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltvLaunchTemplateData :: Lens.Lens' CreateLaunchTemplateVersion RequestLaunchTemplateData
cltvLaunchTemplateData = Lens.lens (launchTemplateData :: CreateLaunchTemplateVersion -> RequestLaunchTemplateData) (\s a -> s {launchTemplateData = a} :: CreateLaunchTemplateVersion)
{-# DEPRECATED cltvLaunchTemplateData "Use generic-lens or generic-optics with 'launchTemplateData' instead." #-}

instance Lude.AWSRequest CreateLaunchTemplateVersion where
  type
    Rs CreateLaunchTemplateVersion =
      CreateLaunchTemplateVersionResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateLaunchTemplateVersionResponse'
            Lude.<$> (x Lude..@? "launchTemplateVersion")
            Lude.<*> (x Lude..@? "warning")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateLaunchTemplateVersion where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateLaunchTemplateVersion where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateLaunchTemplateVersion where
  toQuery CreateLaunchTemplateVersion' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CreateLaunchTemplateVersion" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "LaunchTemplateName" Lude.=: launchTemplateName,
        "ClientToken" Lude.=: clientToken,
        "LaunchTemplateId" Lude.=: launchTemplateId,
        "VersionDescription" Lude.=: versionDescription,
        "SourceVersion" Lude.=: sourceVersion,
        "DryRun" Lude.=: dryRun,
        "LaunchTemplateData" Lude.=: launchTemplateData
      ]

-- | /See:/ 'mkCreateLaunchTemplateVersionResponse' smart constructor.
data CreateLaunchTemplateVersionResponse = CreateLaunchTemplateVersionResponse'
  { launchTemplateVersion ::
      Lude.Maybe
        LaunchTemplateVersion,
    warning ::
      Lude.Maybe
        ValidationWarning,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLaunchTemplateVersionResponse' with the minimum fields required to make a request.
--
-- * 'launchTemplateVersion' - Information about the launch template version.
-- * 'responseStatus' - The response status code.
-- * 'warning' - If the new version of the launch template contains parameters or parameter combinations that are not valid, an error code and an error message are returned for each issue that's found.
mkCreateLaunchTemplateVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateLaunchTemplateVersionResponse
mkCreateLaunchTemplateVersionResponse pResponseStatus_ =
  CreateLaunchTemplateVersionResponse'
    { launchTemplateVersion =
        Lude.Nothing,
      warning = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the launch template version.
--
-- /Note:/ Consider using 'launchTemplateVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltvrsLaunchTemplateVersion :: Lens.Lens' CreateLaunchTemplateVersionResponse (Lude.Maybe LaunchTemplateVersion)
cltvrsLaunchTemplateVersion = Lens.lens (launchTemplateVersion :: CreateLaunchTemplateVersionResponse -> Lude.Maybe LaunchTemplateVersion) (\s a -> s {launchTemplateVersion = a} :: CreateLaunchTemplateVersionResponse)
{-# DEPRECATED cltvrsLaunchTemplateVersion "Use generic-lens or generic-optics with 'launchTemplateVersion' instead." #-}

-- | If the new version of the launch template contains parameters or parameter combinations that are not valid, an error code and an error message are returned for each issue that's found.
--
-- /Note:/ Consider using 'warning' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltvrsWarning :: Lens.Lens' CreateLaunchTemplateVersionResponse (Lude.Maybe ValidationWarning)
cltvrsWarning = Lens.lens (warning :: CreateLaunchTemplateVersionResponse -> Lude.Maybe ValidationWarning) (\s a -> s {warning = a} :: CreateLaunchTemplateVersionResponse)
{-# DEPRECATED cltvrsWarning "Use generic-lens or generic-optics with 'warning' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltvrsResponseStatus :: Lens.Lens' CreateLaunchTemplateVersionResponse Lude.Int
cltvrsResponseStatus = Lens.lens (responseStatus :: CreateLaunchTemplateVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateLaunchTemplateVersionResponse)
{-# DEPRECATED cltvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
