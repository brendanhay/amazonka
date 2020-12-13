{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeProvisioningTemplateVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a fleet provisioning template version.
module Network.AWS.IoT.DescribeProvisioningTemplateVersion
  ( -- * Creating a request
    DescribeProvisioningTemplateVersion (..),
    mkDescribeProvisioningTemplateVersion,

    -- ** Request lenses
    dptvVersionId,
    dptvTemplateName,

    -- * Destructuring the response
    DescribeProvisioningTemplateVersionResponse (..),
    mkDescribeProvisioningTemplateVersionResponse,

    -- ** Response lenses
    dptvrsVersionId,
    dptvrsCreationDate,
    dptvrsTemplateBody,
    dptvrsIsDefaultVersion,
    dptvrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeProvisioningTemplateVersion' smart constructor.
data DescribeProvisioningTemplateVersion = DescribeProvisioningTemplateVersion'
  { -- | The fleet provisioning template version ID.
    versionId :: Lude.Int,
    -- | The template name.
    templateName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeProvisioningTemplateVersion' with the minimum fields required to make a request.
--
-- * 'versionId' - The fleet provisioning template version ID.
-- * 'templateName' - The template name.
mkDescribeProvisioningTemplateVersion ::
  -- | 'versionId'
  Lude.Int ->
  -- | 'templateName'
  Lude.Text ->
  DescribeProvisioningTemplateVersion
mkDescribeProvisioningTemplateVersion pVersionId_ pTemplateName_ =
  DescribeProvisioningTemplateVersion'
    { versionId = pVersionId_,
      templateName = pTemplateName_
    }

-- | The fleet provisioning template version ID.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptvVersionId :: Lens.Lens' DescribeProvisioningTemplateVersion Lude.Int
dptvVersionId = Lens.lens (versionId :: DescribeProvisioningTemplateVersion -> Lude.Int) (\s a -> s {versionId = a} :: DescribeProvisioningTemplateVersion)
{-# DEPRECATED dptvVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The template name.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptvTemplateName :: Lens.Lens' DescribeProvisioningTemplateVersion Lude.Text
dptvTemplateName = Lens.lens (templateName :: DescribeProvisioningTemplateVersion -> Lude.Text) (\s a -> s {templateName = a} :: DescribeProvisioningTemplateVersion)
{-# DEPRECATED dptvTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

instance Lude.AWSRequest DescribeProvisioningTemplateVersion where
  type
    Rs DescribeProvisioningTemplateVersion =
      DescribeProvisioningTemplateVersionResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeProvisioningTemplateVersionResponse'
            Lude.<$> (x Lude..?> "versionId")
            Lude.<*> (x Lude..?> "creationDate")
            Lude.<*> (x Lude..?> "templateBody")
            Lude.<*> (x Lude..?> "isDefaultVersion")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeProvisioningTemplateVersion where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeProvisioningTemplateVersion where
  toPath DescribeProvisioningTemplateVersion' {..} =
    Lude.mconcat
      [ "/provisioning-templates/",
        Lude.toBS templateName,
        "/versions/",
        Lude.toBS versionId
      ]

instance Lude.ToQuery DescribeProvisioningTemplateVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeProvisioningTemplateVersionResponse' smart constructor.
data DescribeProvisioningTemplateVersionResponse = DescribeProvisioningTemplateVersionResponse'
  { -- | The fleet provisioning template version ID.
    versionId :: Lude.Maybe Lude.Int,
    -- | The date when the fleet provisioning template version was created.
    creationDate :: Lude.Maybe Lude.Timestamp,
    -- | The JSON formatted contents of the fleet provisioning template version.
    templateBody :: Lude.Maybe Lude.Text,
    -- | True if the fleet provisioning template version is the default version.
    isDefaultVersion :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeProvisioningTemplateVersionResponse' with the minimum fields required to make a request.
--
-- * 'versionId' - The fleet provisioning template version ID.
-- * 'creationDate' - The date when the fleet provisioning template version was created.
-- * 'templateBody' - The JSON formatted contents of the fleet provisioning template version.
-- * 'isDefaultVersion' - True if the fleet provisioning template version is the default version.
-- * 'responseStatus' - The response status code.
mkDescribeProvisioningTemplateVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeProvisioningTemplateVersionResponse
mkDescribeProvisioningTemplateVersionResponse pResponseStatus_ =
  DescribeProvisioningTemplateVersionResponse'
    { versionId =
        Lude.Nothing,
      creationDate = Lude.Nothing,
      templateBody = Lude.Nothing,
      isDefaultVersion = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The fleet provisioning template version ID.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptvrsVersionId :: Lens.Lens' DescribeProvisioningTemplateVersionResponse (Lude.Maybe Lude.Int)
dptvrsVersionId = Lens.lens (versionId :: DescribeProvisioningTemplateVersionResponse -> Lude.Maybe Lude.Int) (\s a -> s {versionId = a} :: DescribeProvisioningTemplateVersionResponse)
{-# DEPRECATED dptvrsVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The date when the fleet provisioning template version was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptvrsCreationDate :: Lens.Lens' DescribeProvisioningTemplateVersionResponse (Lude.Maybe Lude.Timestamp)
dptvrsCreationDate = Lens.lens (creationDate :: DescribeProvisioningTemplateVersionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: DescribeProvisioningTemplateVersionResponse)
{-# DEPRECATED dptvrsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The JSON formatted contents of the fleet provisioning template version.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptvrsTemplateBody :: Lens.Lens' DescribeProvisioningTemplateVersionResponse (Lude.Maybe Lude.Text)
dptvrsTemplateBody = Lens.lens (templateBody :: DescribeProvisioningTemplateVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {templateBody = a} :: DescribeProvisioningTemplateVersionResponse)
{-# DEPRECATED dptvrsTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | True if the fleet provisioning template version is the default version.
--
-- /Note:/ Consider using 'isDefaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptvrsIsDefaultVersion :: Lens.Lens' DescribeProvisioningTemplateVersionResponse (Lude.Maybe Lude.Bool)
dptvrsIsDefaultVersion = Lens.lens (isDefaultVersion :: DescribeProvisioningTemplateVersionResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isDefaultVersion = a} :: DescribeProvisioningTemplateVersionResponse)
{-# DEPRECATED dptvrsIsDefaultVersion "Use generic-lens or generic-optics with 'isDefaultVersion' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptvrsResponseStatus :: Lens.Lens' DescribeProvisioningTemplateVersionResponse Lude.Int
dptvrsResponseStatus = Lens.lens (responseStatus :: DescribeProvisioningTemplateVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeProvisioningTemplateVersionResponse)
{-# DEPRECATED dptvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
