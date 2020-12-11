{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.GetCloudFormationTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the specified AWS CloudFormation template.
module Network.AWS.ServerlessApplicationRepository.GetCloudFormationTemplate
  ( -- * Creating a request
    GetCloudFormationTemplate (..),
    mkGetCloudFormationTemplate,

    -- ** Request lenses
    gcftApplicationId,
    gcftTemplateId,

    -- * Destructuring the response
    GetCloudFormationTemplateResponse (..),
    mkGetCloudFormationTemplateResponse,

    -- ** Response lenses
    gcftrsCreationTime,
    gcftrsStatus,
    gcftrsTemplateId,
    gcftrsSemanticVersion,
    gcftrsApplicationId,
    gcftrsTemplateURL,
    gcftrsExpirationTime,
    gcftrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'mkGetCloudFormationTemplate' smart constructor.
data GetCloudFormationTemplate = GetCloudFormationTemplate'
  { applicationId ::
      Lude.Text,
    templateId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCloudFormationTemplate' with the minimum fields required to make a request.
--
-- * 'applicationId' - The Amazon Resource Name (ARN) of the application.
-- * 'templateId' - The UUID returned by CreateCloudFormationTemplate.
--
-- Pattern: [0-9a-fA-F]{8}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{12}
mkGetCloudFormationTemplate ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'templateId'
  Lude.Text ->
  GetCloudFormationTemplate
mkGetCloudFormationTemplate pApplicationId_ pTemplateId_ =
  GetCloudFormationTemplate'
    { applicationId = pApplicationId_,
      templateId = pTemplateId_
    }

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcftApplicationId :: Lens.Lens' GetCloudFormationTemplate Lude.Text
gcftApplicationId = Lens.lens (applicationId :: GetCloudFormationTemplate -> Lude.Text) (\s a -> s {applicationId = a} :: GetCloudFormationTemplate)
{-# DEPRECATED gcftApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The UUID returned by CreateCloudFormationTemplate.
--
-- Pattern: [0-9a-fA-F]{8}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{12}
--
-- /Note:/ Consider using 'templateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcftTemplateId :: Lens.Lens' GetCloudFormationTemplate Lude.Text
gcftTemplateId = Lens.lens (templateId :: GetCloudFormationTemplate -> Lude.Text) (\s a -> s {templateId = a} :: GetCloudFormationTemplate)
{-# DEPRECATED gcftTemplateId "Use generic-lens or generic-optics with 'templateId' instead." #-}

instance Lude.AWSRequest GetCloudFormationTemplate where
  type
    Rs GetCloudFormationTemplate =
      GetCloudFormationTemplateResponse
  request = Req.get serverlessApplicationRepositoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCloudFormationTemplateResponse'
            Lude.<$> (x Lude..?> "creationTime")
            Lude.<*> (x Lude..?> "status")
            Lude.<*> (x Lude..?> "templateId")
            Lude.<*> (x Lude..?> "semanticVersion")
            Lude.<*> (x Lude..?> "applicationId")
            Lude.<*> (x Lude..?> "templateUrl")
            Lude.<*> (x Lude..?> "expirationTime")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCloudFormationTemplate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetCloudFormationTemplate where
  toPath GetCloudFormationTemplate' {..} =
    Lude.mconcat
      [ "/applications/",
        Lude.toBS applicationId,
        "/templates/",
        Lude.toBS templateId
      ]

instance Lude.ToQuery GetCloudFormationTemplate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCloudFormationTemplateResponse' smart constructor.
data GetCloudFormationTemplateResponse = GetCloudFormationTemplateResponse'
  { creationTime ::
      Lude.Maybe Lude.Text,
    status ::
      Lude.Maybe Status,
    templateId ::
      Lude.Maybe Lude.Text,
    semanticVersion ::
      Lude.Maybe Lude.Text,
    applicationId ::
      Lude.Maybe Lude.Text,
    templateURL ::
      Lude.Maybe Lude.Text,
    expirationTime ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetCloudFormationTemplateResponse' with the minimum fields required to make a request.
--
-- * 'applicationId' - The application Amazon Resource Name (ARN).
-- * 'creationTime' - The date and time this resource was created.
-- * 'expirationTime' - The date and time this template expires. Templates
--
--  expire 1 hour after creation.
-- * 'responseStatus' - The response status code.
-- * 'semanticVersion' - The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/>
-- * 'status' - Status of the template creation workflow.
--
-- Possible values: PREPARING | ACTIVE | EXPIRED
--
-- * 'templateId' - The UUID returned by CreateCloudFormationTemplate.
--
-- Pattern: [0-9a-fA-F]{8}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{12}
-- * 'templateURL' - A link to the template that can be used to deploy the application using
--
--  AWS CloudFormation.
mkGetCloudFormationTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCloudFormationTemplateResponse
mkGetCloudFormationTemplateResponse pResponseStatus_ =
  GetCloudFormationTemplateResponse'
    { creationTime = Lude.Nothing,
      status = Lude.Nothing,
      templateId = Lude.Nothing,
      semanticVersion = Lude.Nothing,
      applicationId = Lude.Nothing,
      templateURL = Lude.Nothing,
      expirationTime = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The date and time this resource was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcftrsCreationTime :: Lens.Lens' GetCloudFormationTemplateResponse (Lude.Maybe Lude.Text)
gcftrsCreationTime = Lens.lens (creationTime :: GetCloudFormationTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTime = a} :: GetCloudFormationTemplateResponse)
{-# DEPRECATED gcftrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Status of the template creation workflow.
--
-- Possible values: PREPARING | ACTIVE | EXPIRED
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcftrsStatus :: Lens.Lens' GetCloudFormationTemplateResponse (Lude.Maybe Status)
gcftrsStatus = Lens.lens (status :: GetCloudFormationTemplateResponse -> Lude.Maybe Status) (\s a -> s {status = a} :: GetCloudFormationTemplateResponse)
{-# DEPRECATED gcftrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The UUID returned by CreateCloudFormationTemplate.
--
-- Pattern: [0-9a-fA-F]{8}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{12}
--
-- /Note:/ Consider using 'templateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcftrsTemplateId :: Lens.Lens' GetCloudFormationTemplateResponse (Lude.Maybe Lude.Text)
gcftrsTemplateId = Lens.lens (templateId :: GetCloudFormationTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {templateId = a} :: GetCloudFormationTemplateResponse)
{-# DEPRECATED gcftrsTemplateId "Use generic-lens or generic-optics with 'templateId' instead." #-}

-- | The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/>
--
-- /Note:/ Consider using 'semanticVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcftrsSemanticVersion :: Lens.Lens' GetCloudFormationTemplateResponse (Lude.Maybe Lude.Text)
gcftrsSemanticVersion = Lens.lens (semanticVersion :: GetCloudFormationTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {semanticVersion = a} :: GetCloudFormationTemplateResponse)
{-# DEPRECATED gcftrsSemanticVersion "Use generic-lens or generic-optics with 'semanticVersion' instead." #-}

-- | The application Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcftrsApplicationId :: Lens.Lens' GetCloudFormationTemplateResponse (Lude.Maybe Lude.Text)
gcftrsApplicationId = Lens.lens (applicationId :: GetCloudFormationTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {applicationId = a} :: GetCloudFormationTemplateResponse)
{-# DEPRECATED gcftrsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | A link to the template that can be used to deploy the application using
--
--  AWS CloudFormation.
--
-- /Note:/ Consider using 'templateURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcftrsTemplateURL :: Lens.Lens' GetCloudFormationTemplateResponse (Lude.Maybe Lude.Text)
gcftrsTemplateURL = Lens.lens (templateURL :: GetCloudFormationTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {templateURL = a} :: GetCloudFormationTemplateResponse)
{-# DEPRECATED gcftrsTemplateURL "Use generic-lens or generic-optics with 'templateURL' instead." #-}

-- | The date and time this template expires. Templates
--
--  expire 1 hour after creation.
--
-- /Note:/ Consider using 'expirationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcftrsExpirationTime :: Lens.Lens' GetCloudFormationTemplateResponse (Lude.Maybe Lude.Text)
gcftrsExpirationTime = Lens.lens (expirationTime :: GetCloudFormationTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {expirationTime = a} :: GetCloudFormationTemplateResponse)
{-# DEPRECATED gcftrsExpirationTime "Use generic-lens or generic-optics with 'expirationTime' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcftrsResponseStatus :: Lens.Lens' GetCloudFormationTemplateResponse Lude.Int
gcftrsResponseStatus = Lens.lens (responseStatus :: GetCloudFormationTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCloudFormationTemplateResponse)
{-# DEPRECATED gcftrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
