{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.EstimateTemplateCost
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the estimated monthly cost of a template. The return value is an AWS Simple Monthly Calculator URL with a query string that describes the resources required to run the template.
module Network.AWS.CloudFormation.EstimateTemplateCost
  ( -- * Creating a request
    EstimateTemplateCost (..),
    mkEstimateTemplateCost,

    -- ** Request lenses
    etcParameters,
    etcTemplateBody,
    etcTemplateURL,

    -- * Destructuring the response
    EstimateTemplateCostResponse (..),
    mkEstimateTemplateCostResponse,

    -- ** Response lenses
    etcrsURL,
    etcrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for an 'EstimateTemplateCost' action.
--
-- /See:/ 'mkEstimateTemplateCost' smart constructor.
data EstimateTemplateCost = EstimateTemplateCost'
  { parameters ::
      Lude.Maybe [Parameter],
    templateBody :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'EstimateTemplateCost' with the minimum fields required to make a request.
--
-- * 'parameters' - A list of @Parameter@ structures that specify input parameters.
-- * 'templateBody' - Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.)
--
-- Conditional: You must pass @TemplateBody@ or @TemplateURL@ . If both are passed, only @TemplateBody@ is used.
-- * 'templateURL' - Location of file containing the template body. The URL must point to a template that is located in an Amazon S3 bucket. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must pass @TemplateURL@ or @TemplateBody@ . If both are passed, only @TemplateBody@ is used.
mkEstimateTemplateCost ::
  EstimateTemplateCost
mkEstimateTemplateCost =
  EstimateTemplateCost'
    { parameters = Lude.Nothing,
      templateBody = Lude.Nothing,
      templateURL = Lude.Nothing
    }

-- | A list of @Parameter@ structures that specify input parameters.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etcParameters :: Lens.Lens' EstimateTemplateCost (Lude.Maybe [Parameter])
etcParameters = Lens.lens (parameters :: EstimateTemplateCost -> Lude.Maybe [Parameter]) (\s a -> s {parameters = a} :: EstimateTemplateCost)
{-# DEPRECATED etcParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.)
--
-- Conditional: You must pass @TemplateBody@ or @TemplateURL@ . If both are passed, only @TemplateBody@ is used.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etcTemplateBody :: Lens.Lens' EstimateTemplateCost (Lude.Maybe Lude.Text)
etcTemplateBody = Lens.lens (templateBody :: EstimateTemplateCost -> Lude.Maybe Lude.Text) (\s a -> s {templateBody = a} :: EstimateTemplateCost)
{-# DEPRECATED etcTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | Location of file containing the template body. The URL must point to a template that is located in an Amazon S3 bucket. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must pass @TemplateURL@ or @TemplateBody@ . If both are passed, only @TemplateBody@ is used.
--
-- /Note:/ Consider using 'templateURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etcTemplateURL :: Lens.Lens' EstimateTemplateCost (Lude.Maybe Lude.Text)
etcTemplateURL = Lens.lens (templateURL :: EstimateTemplateCost -> Lude.Maybe Lude.Text) (\s a -> s {templateURL = a} :: EstimateTemplateCost)
{-# DEPRECATED etcTemplateURL "Use generic-lens or generic-optics with 'templateURL' instead." #-}

instance Lude.AWSRequest EstimateTemplateCost where
  type Rs EstimateTemplateCost = EstimateTemplateCostResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "EstimateTemplateCostResult"
      ( \s h x ->
          EstimateTemplateCostResponse'
            Lude.<$> (x Lude..@? "Url") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders EstimateTemplateCost where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath EstimateTemplateCost where
  toPath = Lude.const "/"

instance Lude.ToQuery EstimateTemplateCost where
  toQuery EstimateTemplateCost' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("EstimateTemplateCost" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "Parameters"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> parameters),
        "TemplateBody" Lude.=: templateBody,
        "TemplateURL" Lude.=: templateURL
      ]

-- | The output for a 'EstimateTemplateCost' action.
--
-- /See:/ 'mkEstimateTemplateCostResponse' smart constructor.
data EstimateTemplateCostResponse = EstimateTemplateCostResponse'
  { url ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'EstimateTemplateCostResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'url' - An AWS Simple Monthly Calculator URL with a query string that describes the resources required to run the template.
mkEstimateTemplateCostResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  EstimateTemplateCostResponse
mkEstimateTemplateCostResponse pResponseStatus_ =
  EstimateTemplateCostResponse'
    { url = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An AWS Simple Monthly Calculator URL with a query string that describes the resources required to run the template.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etcrsURL :: Lens.Lens' EstimateTemplateCostResponse (Lude.Maybe Lude.Text)
etcrsURL = Lens.lens (url :: EstimateTemplateCostResponse -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: EstimateTemplateCostResponse)
{-# DEPRECATED etcrsURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etcrsResponseStatus :: Lens.Lens' EstimateTemplateCostResponse Lude.Int
etcrsResponseStatus = Lens.lens (responseStatus :: EstimateTemplateCostResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: EstimateTemplateCostResponse)
{-# DEPRECATED etcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
