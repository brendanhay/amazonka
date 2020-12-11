{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.CreateCloudFormationTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS CloudFormation template.
module Network.AWS.ServerlessApplicationRepository.CreateCloudFormationTemplate
  ( -- * Creating a request
    CreateCloudFormationTemplate (..),
    mkCreateCloudFormationTemplate,

    -- ** Request lenses
    ccftSemanticVersion,
    ccftApplicationId,

    -- * Destructuring the response
    CreateCloudFormationTemplateResponse (..),
    mkCreateCloudFormationTemplateResponse,

    -- ** Response lenses
    ccftrsCreationTime,
    ccftrsStatus,
    ccftrsTemplateId,
    ccftrsSemanticVersion,
    ccftrsApplicationId,
    ccftrsTemplateURL,
    ccftrsExpirationTime,
    ccftrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'mkCreateCloudFormationTemplate' smart constructor.
data CreateCloudFormationTemplate = CreateCloudFormationTemplate'
  { semanticVersion ::
      Lude.Maybe Lude.Text,
    applicationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCloudFormationTemplate' with the minimum fields required to make a request.
--
-- * 'applicationId' - The Amazon Resource Name (ARN) of the application.
-- * 'semanticVersion' - The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/>
mkCreateCloudFormationTemplate ::
  -- | 'applicationId'
  Lude.Text ->
  CreateCloudFormationTemplate
mkCreateCloudFormationTemplate pApplicationId_ =
  CreateCloudFormationTemplate'
    { semanticVersion = Lude.Nothing,
      applicationId = pApplicationId_
    }

-- | The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/>
--
-- /Note:/ Consider using 'semanticVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccftSemanticVersion :: Lens.Lens' CreateCloudFormationTemplate (Lude.Maybe Lude.Text)
ccftSemanticVersion = Lens.lens (semanticVersion :: CreateCloudFormationTemplate -> Lude.Maybe Lude.Text) (\s a -> s {semanticVersion = a} :: CreateCloudFormationTemplate)
{-# DEPRECATED ccftSemanticVersion "Use generic-lens or generic-optics with 'semanticVersion' instead." #-}

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccftApplicationId :: Lens.Lens' CreateCloudFormationTemplate Lude.Text
ccftApplicationId = Lens.lens (applicationId :: CreateCloudFormationTemplate -> Lude.Text) (\s a -> s {applicationId = a} :: CreateCloudFormationTemplate)
{-# DEPRECATED ccftApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest CreateCloudFormationTemplate where
  type
    Rs CreateCloudFormationTemplate =
      CreateCloudFormationTemplateResponse
  request = Req.postJSON serverlessApplicationRepositoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateCloudFormationTemplateResponse'
            Lude.<$> (x Lude..?> "creationTime")
            Lude.<*> (x Lude..?> "status")
            Lude.<*> (x Lude..?> "templateId")
            Lude.<*> (x Lude..?> "semanticVersion")
            Lude.<*> (x Lude..?> "applicationId")
            Lude.<*> (x Lude..?> "templateUrl")
            Lude.<*> (x Lude..?> "expirationTime")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCloudFormationTemplate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateCloudFormationTemplate where
  toJSON CreateCloudFormationTemplate' {..} =
    Lude.object
      ( Lude.catMaybes
          [("semanticVersion" Lude..=) Lude.<$> semanticVersion]
      )

instance Lude.ToPath CreateCloudFormationTemplate where
  toPath CreateCloudFormationTemplate' {..} =
    Lude.mconcat
      ["/applications/", Lude.toBS applicationId, "/templates"]

instance Lude.ToQuery CreateCloudFormationTemplate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateCloudFormationTemplateResponse' smart constructor.
data CreateCloudFormationTemplateResponse = CreateCloudFormationTemplateResponse'
  { creationTime ::
      Lude.Maybe
        Lude.Text,
    status ::
      Lude.Maybe Status,
    templateId ::
      Lude.Maybe
        Lude.Text,
    semanticVersion ::
      Lude.Maybe
        Lude.Text,
    applicationId ::
      Lude.Maybe
        Lude.Text,
    templateURL ::
      Lude.Maybe
        Lude.Text,
    expirationTime ::
      Lude.Maybe
        Lude.Text,
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

-- | Creates a value of 'CreateCloudFormationTemplateResponse' with the minimum fields required to make a request.
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
mkCreateCloudFormationTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateCloudFormationTemplateResponse
mkCreateCloudFormationTemplateResponse pResponseStatus_ =
  CreateCloudFormationTemplateResponse'
    { creationTime =
        Lude.Nothing,
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
ccftrsCreationTime :: Lens.Lens' CreateCloudFormationTemplateResponse (Lude.Maybe Lude.Text)
ccftrsCreationTime = Lens.lens (creationTime :: CreateCloudFormationTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTime = a} :: CreateCloudFormationTemplateResponse)
{-# DEPRECATED ccftrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Status of the template creation workflow.
--
-- Possible values: PREPARING | ACTIVE | EXPIRED
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccftrsStatus :: Lens.Lens' CreateCloudFormationTemplateResponse (Lude.Maybe Status)
ccftrsStatus = Lens.lens (status :: CreateCloudFormationTemplateResponse -> Lude.Maybe Status) (\s a -> s {status = a} :: CreateCloudFormationTemplateResponse)
{-# DEPRECATED ccftrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The UUID returned by CreateCloudFormationTemplate.
--
-- Pattern: [0-9a-fA-F]{8}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{12}
--
-- /Note:/ Consider using 'templateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccftrsTemplateId :: Lens.Lens' CreateCloudFormationTemplateResponse (Lude.Maybe Lude.Text)
ccftrsTemplateId = Lens.lens (templateId :: CreateCloudFormationTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {templateId = a} :: CreateCloudFormationTemplateResponse)
{-# DEPRECATED ccftrsTemplateId "Use generic-lens or generic-optics with 'templateId' instead." #-}

-- | The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/>
--
-- /Note:/ Consider using 'semanticVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccftrsSemanticVersion :: Lens.Lens' CreateCloudFormationTemplateResponse (Lude.Maybe Lude.Text)
ccftrsSemanticVersion = Lens.lens (semanticVersion :: CreateCloudFormationTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {semanticVersion = a} :: CreateCloudFormationTemplateResponse)
{-# DEPRECATED ccftrsSemanticVersion "Use generic-lens or generic-optics with 'semanticVersion' instead." #-}

-- | The application Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccftrsApplicationId :: Lens.Lens' CreateCloudFormationTemplateResponse (Lude.Maybe Lude.Text)
ccftrsApplicationId = Lens.lens (applicationId :: CreateCloudFormationTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {applicationId = a} :: CreateCloudFormationTemplateResponse)
{-# DEPRECATED ccftrsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | A link to the template that can be used to deploy the application using
--
--  AWS CloudFormation.
--
-- /Note:/ Consider using 'templateURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccftrsTemplateURL :: Lens.Lens' CreateCloudFormationTemplateResponse (Lude.Maybe Lude.Text)
ccftrsTemplateURL = Lens.lens (templateURL :: CreateCloudFormationTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {templateURL = a} :: CreateCloudFormationTemplateResponse)
{-# DEPRECATED ccftrsTemplateURL "Use generic-lens or generic-optics with 'templateURL' instead." #-}

-- | The date and time this template expires. Templates
--
--  expire 1 hour after creation.
--
-- /Note:/ Consider using 'expirationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccftrsExpirationTime :: Lens.Lens' CreateCloudFormationTemplateResponse (Lude.Maybe Lude.Text)
ccftrsExpirationTime = Lens.lens (expirationTime :: CreateCloudFormationTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {expirationTime = a} :: CreateCloudFormationTemplateResponse)
{-# DEPRECATED ccftrsExpirationTime "Use generic-lens or generic-optics with 'expirationTime' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccftrsResponseStatus :: Lens.Lens' CreateCloudFormationTemplateResponse Lude.Int
ccftrsResponseStatus = Lens.lens (responseStatus :: CreateCloudFormationTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCloudFormationTemplateResponse)
{-# DEPRECATED ccftrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
