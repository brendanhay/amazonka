{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.ValidateTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Validates a specified template. AWS CloudFormation first checks if the template is valid JSON. If it isn't, AWS CloudFormation checks if the template is valid YAML. If both these checks fail, AWS CloudFormation returns a template validation error.
module Network.AWS.CloudFormation.ValidateTemplate
  ( -- * Creating a request
    ValidateTemplate (..),
    mkValidateTemplate,

    -- ** Request lenses
    vtTemplateBody,
    vtTemplateURL,

    -- * Destructuring the response
    ValidateTemplateResponse (..),
    mkValidateTemplateResponse,

    -- ** Response lenses
    vtrsDeclaredTransforms,
    vtrsCapabilitiesReason,
    vtrsParameters,
    vtrsDescription,
    vtrsCapabilities,
    vtrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for 'ValidateTemplate' action.
--
-- /See:/ 'mkValidateTemplate' smart constructor.
data ValidateTemplate = ValidateTemplate'
  { templateBody ::
      Lude.Maybe Lude.Text,
    templateURL :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ValidateTemplate' with the minimum fields required to make a request.
--
-- * 'templateBody' - Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must pass @TemplateURL@ or @TemplateBody@ . If both are passed, only @TemplateBody@ is used.
-- * 'templateURL' - Location of file containing the template body. The URL must point to a template (max size: 460,800 bytes) that is located in an Amazon S3 bucket. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must pass @TemplateURL@ or @TemplateBody@ . If both are passed, only @TemplateBody@ is used.
mkValidateTemplate ::
  ValidateTemplate
mkValidateTemplate =
  ValidateTemplate'
    { templateBody = Lude.Nothing,
      templateURL = Lude.Nothing
    }

-- | Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must pass @TemplateURL@ or @TemplateBody@ . If both are passed, only @TemplateBody@ is used.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtTemplateBody :: Lens.Lens' ValidateTemplate (Lude.Maybe Lude.Text)
vtTemplateBody = Lens.lens (templateBody :: ValidateTemplate -> Lude.Maybe Lude.Text) (\s a -> s {templateBody = a} :: ValidateTemplate)
{-# DEPRECATED vtTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | Location of file containing the template body. The URL must point to a template (max size: 460,800 bytes) that is located in an Amazon S3 bucket. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must pass @TemplateURL@ or @TemplateBody@ . If both are passed, only @TemplateBody@ is used.
--
-- /Note:/ Consider using 'templateURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtTemplateURL :: Lens.Lens' ValidateTemplate (Lude.Maybe Lude.Text)
vtTemplateURL = Lens.lens (templateURL :: ValidateTemplate -> Lude.Maybe Lude.Text) (\s a -> s {templateURL = a} :: ValidateTemplate)
{-# DEPRECATED vtTemplateURL "Use generic-lens or generic-optics with 'templateURL' instead." #-}

instance Lude.AWSRequest ValidateTemplate where
  type Rs ValidateTemplate = ValidateTemplateResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "ValidateTemplateResult"
      ( \s h x ->
          ValidateTemplateResponse'
            Lude.<$> ( x Lude..@? "DeclaredTransforms" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "CapabilitiesReason")
            Lude.<*> ( x Lude..@? "Parameters" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "Description")
            Lude.<*> ( x Lude..@? "Capabilities" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ValidateTemplate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ValidateTemplate where
  toPath = Lude.const "/"

instance Lude.ToQuery ValidateTemplate where
  toQuery ValidateTemplate' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ValidateTemplate" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "TemplateBody" Lude.=: templateBody,
        "TemplateURL" Lude.=: templateURL
      ]

-- | The output for 'ValidateTemplate' action.
--
-- /See:/ 'mkValidateTemplateResponse' smart constructor.
data ValidateTemplateResponse = ValidateTemplateResponse'
  { declaredTransforms ::
      Lude.Maybe [Lude.Text],
    capabilitiesReason ::
      Lude.Maybe Lude.Text,
    parameters ::
      Lude.Maybe [TemplateParameter],
    description :: Lude.Maybe Lude.Text,
    capabilities :: Lude.Maybe [Capability],
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

-- | Creates a value of 'ValidateTemplateResponse' with the minimum fields required to make a request.
--
-- * 'capabilities' - The capabilities found within the template. If your template contains IAM resources, you must specify the CAPABILITY_IAM or CAPABILITY_NAMED_IAM value for this parameter when you use the 'CreateStack' or 'UpdateStack' actions with your template; otherwise, those actions return an InsufficientCapabilities error.
--
-- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates> .
-- * 'capabilitiesReason' - The list of resources that generated the values in the @Capabilities@ response element.
-- * 'declaredTransforms' - A list of the transforms that are declared in the template.
-- * 'description' - The description found within the template.
-- * 'parameters' - A list of @TemplateParameter@ structures.
-- * 'responseStatus' - The response status code.
mkValidateTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ValidateTemplateResponse
mkValidateTemplateResponse pResponseStatus_ =
  ValidateTemplateResponse'
    { declaredTransforms = Lude.Nothing,
      capabilitiesReason = Lude.Nothing,
      parameters = Lude.Nothing,
      description = Lude.Nothing,
      capabilities = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of the transforms that are declared in the template.
--
-- /Note:/ Consider using 'declaredTransforms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtrsDeclaredTransforms :: Lens.Lens' ValidateTemplateResponse (Lude.Maybe [Lude.Text])
vtrsDeclaredTransforms = Lens.lens (declaredTransforms :: ValidateTemplateResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {declaredTransforms = a} :: ValidateTemplateResponse)
{-# DEPRECATED vtrsDeclaredTransforms "Use generic-lens or generic-optics with 'declaredTransforms' instead." #-}

-- | The list of resources that generated the values in the @Capabilities@ response element.
--
-- /Note:/ Consider using 'capabilitiesReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtrsCapabilitiesReason :: Lens.Lens' ValidateTemplateResponse (Lude.Maybe Lude.Text)
vtrsCapabilitiesReason = Lens.lens (capabilitiesReason :: ValidateTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {capabilitiesReason = a} :: ValidateTemplateResponse)
{-# DEPRECATED vtrsCapabilitiesReason "Use generic-lens or generic-optics with 'capabilitiesReason' instead." #-}

-- | A list of @TemplateParameter@ structures.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtrsParameters :: Lens.Lens' ValidateTemplateResponse (Lude.Maybe [TemplateParameter])
vtrsParameters = Lens.lens (parameters :: ValidateTemplateResponse -> Lude.Maybe [TemplateParameter]) (\s a -> s {parameters = a} :: ValidateTemplateResponse)
{-# DEPRECATED vtrsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The description found within the template.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtrsDescription :: Lens.Lens' ValidateTemplateResponse (Lude.Maybe Lude.Text)
vtrsDescription = Lens.lens (description :: ValidateTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ValidateTemplateResponse)
{-# DEPRECATED vtrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The capabilities found within the template. If your template contains IAM resources, you must specify the CAPABILITY_IAM or CAPABILITY_NAMED_IAM value for this parameter when you use the 'CreateStack' or 'UpdateStack' actions with your template; otherwise, those actions return an InsufficientCapabilities error.
--
-- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates> .
--
-- /Note:/ Consider using 'capabilities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtrsCapabilities :: Lens.Lens' ValidateTemplateResponse (Lude.Maybe [Capability])
vtrsCapabilities = Lens.lens (capabilities :: ValidateTemplateResponse -> Lude.Maybe [Capability]) (\s a -> s {capabilities = a} :: ValidateTemplateResponse)
{-# DEPRECATED vtrsCapabilities "Use generic-lens or generic-optics with 'capabilities' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtrsResponseStatus :: Lens.Lens' ValidateTemplateResponse Lude.Int
vtrsResponseStatus = Lens.lens (responseStatus :: ValidateTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ValidateTemplateResponse)
{-# DEPRECATED vtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
