{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateLaunchTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a launch template. A launch template contains the parameters to launch an instance. When you launch an instance using 'RunInstances' , you can specify a launch template instead of providing the launch parameters in the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html Launching an instance from a launch template> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.CreateLaunchTemplate
  ( -- * Creating a request
    CreateLaunchTemplate (..),
    mkCreateLaunchTemplate,

    -- ** Request lenses
    cltClientToken,
    cltVersionDescription,
    cltTagSpecifications,
    cltDryRun,
    cltLaunchTemplateName,
    cltLaunchTemplateData,

    -- * Destructuring the response
    CreateLaunchTemplateResponse (..),
    mkCreateLaunchTemplateResponse,

    -- ** Response lenses
    cltrsWarning,
    cltrsLaunchTemplate,
    cltrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateLaunchTemplate' smart constructor.
data CreateLaunchTemplate = CreateLaunchTemplate'
  { clientToken ::
      Lude.Maybe Lude.Text,
    versionDescription :: Lude.Maybe Lude.Text,
    tagSpecifications ::
      Lude.Maybe [TagSpecification],
    dryRun :: Lude.Maybe Lude.Bool,
    launchTemplateName :: Lude.Text,
    launchTemplateData :: RequestLaunchTemplateData
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLaunchTemplate' with the minimum fields required to make a request.
--
-- * 'clientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- Constraint: Maximum 128 ASCII characters.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'launchTemplateData' - The information for the launch template.
-- * 'launchTemplateName' - A name for the launch template.
-- * 'tagSpecifications' - The tags to apply to the launch template during creation.
-- * 'versionDescription' - A description for the first version of the launch template.
mkCreateLaunchTemplate ::
  -- | 'launchTemplateName'
  Lude.Text ->
  -- | 'launchTemplateData'
  RequestLaunchTemplateData ->
  CreateLaunchTemplate
mkCreateLaunchTemplate pLaunchTemplateName_ pLaunchTemplateData_ =
  CreateLaunchTemplate'
    { clientToken = Lude.Nothing,
      versionDescription = Lude.Nothing,
      tagSpecifications = Lude.Nothing,
      dryRun = Lude.Nothing,
      launchTemplateName = pLaunchTemplateName_,
      launchTemplateData = pLaunchTemplateData_
    }

-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- Constraint: Maximum 128 ASCII characters.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltClientToken :: Lens.Lens' CreateLaunchTemplate (Lude.Maybe Lude.Text)
cltClientToken = Lens.lens (clientToken :: CreateLaunchTemplate -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateLaunchTemplate)
{-# DEPRECATED cltClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | A description for the first version of the launch template.
--
-- /Note:/ Consider using 'versionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltVersionDescription :: Lens.Lens' CreateLaunchTemplate (Lude.Maybe Lude.Text)
cltVersionDescription = Lens.lens (versionDescription :: CreateLaunchTemplate -> Lude.Maybe Lude.Text) (\s a -> s {versionDescription = a} :: CreateLaunchTemplate)
{-# DEPRECATED cltVersionDescription "Use generic-lens or generic-optics with 'versionDescription' instead." #-}

-- | The tags to apply to the launch template during creation.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltTagSpecifications :: Lens.Lens' CreateLaunchTemplate (Lude.Maybe [TagSpecification])
cltTagSpecifications = Lens.lens (tagSpecifications :: CreateLaunchTemplate -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateLaunchTemplate)
{-# DEPRECATED cltTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltDryRun :: Lens.Lens' CreateLaunchTemplate (Lude.Maybe Lude.Bool)
cltDryRun = Lens.lens (dryRun :: CreateLaunchTemplate -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateLaunchTemplate)
{-# DEPRECATED cltDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | A name for the launch template.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltLaunchTemplateName :: Lens.Lens' CreateLaunchTemplate Lude.Text
cltLaunchTemplateName = Lens.lens (launchTemplateName :: CreateLaunchTemplate -> Lude.Text) (\s a -> s {launchTemplateName = a} :: CreateLaunchTemplate)
{-# DEPRECATED cltLaunchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead." #-}

-- | The information for the launch template.
--
-- /Note:/ Consider using 'launchTemplateData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltLaunchTemplateData :: Lens.Lens' CreateLaunchTemplate RequestLaunchTemplateData
cltLaunchTemplateData = Lens.lens (launchTemplateData :: CreateLaunchTemplate -> RequestLaunchTemplateData) (\s a -> s {launchTemplateData = a} :: CreateLaunchTemplate)
{-# DEPRECATED cltLaunchTemplateData "Use generic-lens or generic-optics with 'launchTemplateData' instead." #-}

instance Lude.AWSRequest CreateLaunchTemplate where
  type Rs CreateLaunchTemplate = CreateLaunchTemplateResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateLaunchTemplateResponse'
            Lude.<$> (x Lude..@? "warning")
            Lude.<*> (x Lude..@? "launchTemplate")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateLaunchTemplate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateLaunchTemplate where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateLaunchTemplate where
  toQuery CreateLaunchTemplate' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateLaunchTemplate" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "ClientToken" Lude.=: clientToken,
        "VersionDescription" Lude.=: versionDescription,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "DryRun" Lude.=: dryRun,
        "LaunchTemplateName" Lude.=: launchTemplateName,
        "LaunchTemplateData" Lude.=: launchTemplateData
      ]

-- | /See:/ 'mkCreateLaunchTemplateResponse' smart constructor.
data CreateLaunchTemplateResponse = CreateLaunchTemplateResponse'
  { warning ::
      Lude.Maybe ValidationWarning,
    launchTemplate ::
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

-- | Creates a value of 'CreateLaunchTemplateResponse' with the minimum fields required to make a request.
--
-- * 'launchTemplate' - Information about the launch template.
-- * 'responseStatus' - The response status code.
-- * 'warning' - If the launch template contains parameters or parameter combinations that are not valid, an error code and an error message are returned for each issue that's found.
mkCreateLaunchTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateLaunchTemplateResponse
mkCreateLaunchTemplateResponse pResponseStatus_ =
  CreateLaunchTemplateResponse'
    { warning = Lude.Nothing,
      launchTemplate = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the launch template contains parameters or parameter combinations that are not valid, an error code and an error message are returned for each issue that's found.
--
-- /Note:/ Consider using 'warning' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltrsWarning :: Lens.Lens' CreateLaunchTemplateResponse (Lude.Maybe ValidationWarning)
cltrsWarning = Lens.lens (warning :: CreateLaunchTemplateResponse -> Lude.Maybe ValidationWarning) (\s a -> s {warning = a} :: CreateLaunchTemplateResponse)
{-# DEPRECATED cltrsWarning "Use generic-lens or generic-optics with 'warning' instead." #-}

-- | Information about the launch template.
--
-- /Note:/ Consider using 'launchTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltrsLaunchTemplate :: Lens.Lens' CreateLaunchTemplateResponse (Lude.Maybe LaunchTemplate)
cltrsLaunchTemplate = Lens.lens (launchTemplate :: CreateLaunchTemplateResponse -> Lude.Maybe LaunchTemplate) (\s a -> s {launchTemplate = a} :: CreateLaunchTemplateResponse)
{-# DEPRECATED cltrsLaunchTemplate "Use generic-lens or generic-optics with 'launchTemplate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltrsResponseStatus :: Lens.Lens' CreateLaunchTemplateResponse Lude.Int
cltrsResponseStatus = Lens.lens (responseStatus :: CreateLaunchTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateLaunchTemplateResponse)
{-# DEPRECATED cltrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
