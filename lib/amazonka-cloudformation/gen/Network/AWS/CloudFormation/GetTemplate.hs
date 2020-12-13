{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.GetTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the template body for a specified stack. You can get the template for running or deleted stacks.
--
-- For deleted stacks, GetTemplate returns the template for up to 90 days after the stack has been deleted.
module Network.AWS.CloudFormation.GetTemplate
  ( -- * Creating a request
    GetTemplate (..),
    mkGetTemplate,

    -- ** Request lenses
    gtChangeSetName,
    gtTemplateStage,
    gtStackName,

    -- * Destructuring the response
    GetTemplateResponse (..),
    mkGetTemplateResponse,

    -- ** Response lenses
    gtrsStagesAvailable,
    gtrsTemplateBody,
    gtrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for a 'GetTemplate' action.
--
-- /See:/ 'mkGetTemplate' smart constructor.
data GetTemplate = GetTemplate'
  { -- | The name or Amazon Resource Name (ARN) of a change set for which AWS CloudFormation returns the associated template. If you specify a name, you must also specify the @StackName@ .
    changeSetName :: Lude.Maybe Lude.Text,
    -- | For templates that include transforms, the stage of the template that AWS CloudFormation returns. To get the user-submitted template, specify @Original@ . To get the template after AWS CloudFormation has processed all transforms, specify @Processed@ .
    --
    -- If the template doesn't include transforms, @Original@ and @Processed@ return the same template. By default, AWS CloudFormation specifies @Original@ .
    templateStage :: Lude.Maybe TemplateStage,
    -- | The name or the unique stack ID that is associated with the stack, which are not always interchangeable:
    --
    --
    --     * Running stacks: You can specify either the stack's name or its unique stack ID.
    --
    --
    --     * Deleted stacks: You must specify the unique stack ID.
    --
    --
    -- Default: There is no default value.
    stackName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTemplate' with the minimum fields required to make a request.
--
-- * 'changeSetName' - The name or Amazon Resource Name (ARN) of a change set for which AWS CloudFormation returns the associated template. If you specify a name, you must also specify the @StackName@ .
-- * 'templateStage' - For templates that include transforms, the stage of the template that AWS CloudFormation returns. To get the user-submitted template, specify @Original@ . To get the template after AWS CloudFormation has processed all transforms, specify @Processed@ .
--
-- If the template doesn't include transforms, @Original@ and @Processed@ return the same template. By default, AWS CloudFormation specifies @Original@ .
-- * 'stackName' - The name or the unique stack ID that is associated with the stack, which are not always interchangeable:
--
--
--     * Running stacks: You can specify either the stack's name or its unique stack ID.
--
--
--     * Deleted stacks: You must specify the unique stack ID.
--
--
-- Default: There is no default value.
mkGetTemplate ::
  GetTemplate
mkGetTemplate =
  GetTemplate'
    { changeSetName = Lude.Nothing,
      templateStage = Lude.Nothing,
      stackName = Lude.Nothing
    }

-- | The name or Amazon Resource Name (ARN) of a change set for which AWS CloudFormation returns the associated template. If you specify a name, you must also specify the @StackName@ .
--
-- /Note:/ Consider using 'changeSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtChangeSetName :: Lens.Lens' GetTemplate (Lude.Maybe Lude.Text)
gtChangeSetName = Lens.lens (changeSetName :: GetTemplate -> Lude.Maybe Lude.Text) (\s a -> s {changeSetName = a} :: GetTemplate)
{-# DEPRECATED gtChangeSetName "Use generic-lens or generic-optics with 'changeSetName' instead." #-}

-- | For templates that include transforms, the stage of the template that AWS CloudFormation returns. To get the user-submitted template, specify @Original@ . To get the template after AWS CloudFormation has processed all transforms, specify @Processed@ .
--
-- If the template doesn't include transforms, @Original@ and @Processed@ return the same template. By default, AWS CloudFormation specifies @Original@ .
--
-- /Note:/ Consider using 'templateStage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtTemplateStage :: Lens.Lens' GetTemplate (Lude.Maybe TemplateStage)
gtTemplateStage = Lens.lens (templateStage :: GetTemplate -> Lude.Maybe TemplateStage) (\s a -> s {templateStage = a} :: GetTemplate)
{-# DEPRECATED gtTemplateStage "Use generic-lens or generic-optics with 'templateStage' instead." #-}

-- | The name or the unique stack ID that is associated with the stack, which are not always interchangeable:
--
--
--     * Running stacks: You can specify either the stack's name or its unique stack ID.
--
--
--     * Deleted stacks: You must specify the unique stack ID.
--
--
-- Default: There is no default value.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtStackName :: Lens.Lens' GetTemplate (Lude.Maybe Lude.Text)
gtStackName = Lens.lens (stackName :: GetTemplate -> Lude.Maybe Lude.Text) (\s a -> s {stackName = a} :: GetTemplate)
{-# DEPRECATED gtStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Lude.AWSRequest GetTemplate where
  type Rs GetTemplate = GetTemplateResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "GetTemplateResult"
      ( \s h x ->
          GetTemplateResponse'
            Lude.<$> ( x Lude..@? "StagesAvailable" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "TemplateBody")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTemplate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetTemplate where
  toPath = Lude.const "/"

instance Lude.ToQuery GetTemplate where
  toQuery GetTemplate' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetTemplate" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "ChangeSetName" Lude.=: changeSetName,
        "TemplateStage" Lude.=: templateStage,
        "StackName" Lude.=: stackName
      ]

-- | The output for 'GetTemplate' action.
--
-- /See:/ 'mkGetTemplateResponse' smart constructor.
data GetTemplateResponse = GetTemplateResponse'
  { -- | The stage of the template that you can retrieve. For stacks, the @Original@ and @Processed@ templates are always available. For change sets, the @Original@ template is always available. After AWS CloudFormation finishes creating the change set, the @Processed@ template becomes available.
    stagesAvailable :: Lude.Maybe [TemplateStage],
    -- | Structure containing the template body. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.)
    --
    -- AWS CloudFormation returns the same template that was used when the stack was created.
    templateBody :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTemplateResponse' with the minimum fields required to make a request.
--
-- * 'stagesAvailable' - The stage of the template that you can retrieve. For stacks, the @Original@ and @Processed@ templates are always available. For change sets, the @Original@ template is always available. After AWS CloudFormation finishes creating the change set, the @Processed@ template becomes available.
-- * 'templateBody' - Structure containing the template body. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.)
--
-- AWS CloudFormation returns the same template that was used when the stack was created.
-- * 'responseStatus' - The response status code.
mkGetTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTemplateResponse
mkGetTemplateResponse pResponseStatus_ =
  GetTemplateResponse'
    { stagesAvailable = Lude.Nothing,
      templateBody = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The stage of the template that you can retrieve. For stacks, the @Original@ and @Processed@ templates are always available. For change sets, the @Original@ template is always available. After AWS CloudFormation finishes creating the change set, the @Processed@ template becomes available.
--
-- /Note:/ Consider using 'stagesAvailable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrsStagesAvailable :: Lens.Lens' GetTemplateResponse (Lude.Maybe [TemplateStage])
gtrsStagesAvailable = Lens.lens (stagesAvailable :: GetTemplateResponse -> Lude.Maybe [TemplateStage]) (\s a -> s {stagesAvailable = a} :: GetTemplateResponse)
{-# DEPRECATED gtrsStagesAvailable "Use generic-lens or generic-optics with 'stagesAvailable' instead." #-}

-- | Structure containing the template body. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.)
--
-- AWS CloudFormation returns the same template that was used when the stack was created.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrsTemplateBody :: Lens.Lens' GetTemplateResponse (Lude.Maybe Lude.Text)
gtrsTemplateBody = Lens.lens (templateBody :: GetTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {templateBody = a} :: GetTemplateResponse)
{-# DEPRECATED gtrsTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrsResponseStatus :: Lens.Lens' GetTemplateResponse Lude.Int
gtrsResponseStatus = Lens.lens (responseStatus :: GetTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTemplateResponse)
{-# DEPRECATED gtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
