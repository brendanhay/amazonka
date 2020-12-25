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
    gtStackName,
    gtTemplateStage,

    -- * Destructuring the response
    GetTemplateResponse (..),
    mkGetTemplateResponse,

    -- ** Response lenses
    gtrrsStagesAvailable,
    gtrrsTemplateBody,
    gtrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for a 'GetTemplate' action.
--
-- /See:/ 'mkGetTemplate' smart constructor.
data GetTemplate = GetTemplate'
  { -- | The name or Amazon Resource Name (ARN) of a change set for which AWS CloudFormation returns the associated template. If you specify a name, you must also specify the @StackName@ .
    changeSetName :: Core.Maybe Types.ChangeSetNameOrId,
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
    stackName :: Core.Maybe Types.StackName,
    -- | For templates that include transforms, the stage of the template that AWS CloudFormation returns. To get the user-submitted template, specify @Original@ . To get the template after AWS CloudFormation has processed all transforms, specify @Processed@ .
    --
    -- If the template doesn't include transforms, @Original@ and @Processed@ return the same template. By default, AWS CloudFormation specifies @Original@ .
    templateStage :: Core.Maybe Types.TemplateStage
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTemplate' value with any optional fields omitted.
mkGetTemplate ::
  GetTemplate
mkGetTemplate =
  GetTemplate'
    { changeSetName = Core.Nothing,
      stackName = Core.Nothing,
      templateStage = Core.Nothing
    }

-- | The name or Amazon Resource Name (ARN) of a change set for which AWS CloudFormation returns the associated template. If you specify a name, you must also specify the @StackName@ .
--
-- /Note:/ Consider using 'changeSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtChangeSetName :: Lens.Lens' GetTemplate (Core.Maybe Types.ChangeSetNameOrId)
gtChangeSetName = Lens.field @"changeSetName"
{-# DEPRECATED gtChangeSetName "Use generic-lens or generic-optics with 'changeSetName' instead." #-}

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
gtStackName :: Lens.Lens' GetTemplate (Core.Maybe Types.StackName)
gtStackName = Lens.field @"stackName"
{-# DEPRECATED gtStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | For templates that include transforms, the stage of the template that AWS CloudFormation returns. To get the user-submitted template, specify @Original@ . To get the template after AWS CloudFormation has processed all transforms, specify @Processed@ .
--
-- If the template doesn't include transforms, @Original@ and @Processed@ return the same template. By default, AWS CloudFormation specifies @Original@ .
--
-- /Note:/ Consider using 'templateStage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtTemplateStage :: Lens.Lens' GetTemplate (Core.Maybe Types.TemplateStage)
gtTemplateStage = Lens.field @"templateStage"
{-# DEPRECATED gtTemplateStage "Use generic-lens or generic-optics with 'templateStage' instead." #-}

instance Core.AWSRequest GetTemplate where
  type Rs GetTemplate = GetTemplateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "GetTemplate")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "ChangeSetName" Core.<$> changeSetName)
                Core.<> (Core.toQueryValue "StackName" Core.<$> stackName)
                Core.<> (Core.toQueryValue "TemplateStage" Core.<$> templateStage)
            )
      }
  response =
    Response.receiveXMLWrapper
      "GetTemplateResult"
      ( \s h x ->
          GetTemplateResponse'
            Core.<$> (x Core..@? "StagesAvailable" Core..<@> Core.parseXMLList "member")
            Core.<*> (x Core..@? "TemplateBody")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The output for 'GetTemplate' action.
--
-- /See:/ 'mkGetTemplateResponse' smart constructor.
data GetTemplateResponse = GetTemplateResponse'
  { -- | The stage of the template that you can retrieve. For stacks, the @Original@ and @Processed@ templates are always available. For change sets, the @Original@ template is always available. After AWS CloudFormation finishes creating the change set, the @Processed@ template becomes available.
    stagesAvailable :: Core.Maybe [Types.TemplateStage],
    -- | Structure containing the template body. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.)
    --
    -- AWS CloudFormation returns the same template that was used when the stack was created.
    templateBody :: Core.Maybe Types.TemplateBody,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTemplateResponse' value with any optional fields omitted.
mkGetTemplateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetTemplateResponse
mkGetTemplateResponse responseStatus =
  GetTemplateResponse'
    { stagesAvailable = Core.Nothing,
      templateBody = Core.Nothing,
      responseStatus
    }

-- | The stage of the template that you can retrieve. For stacks, the @Original@ and @Processed@ templates are always available. For change sets, the @Original@ template is always available. After AWS CloudFormation finishes creating the change set, the @Processed@ template becomes available.
--
-- /Note:/ Consider using 'stagesAvailable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsStagesAvailable :: Lens.Lens' GetTemplateResponse (Core.Maybe [Types.TemplateStage])
gtrrsStagesAvailable = Lens.field @"stagesAvailable"
{-# DEPRECATED gtrrsStagesAvailable "Use generic-lens or generic-optics with 'stagesAvailable' instead." #-}

-- | Structure containing the template body. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.)
--
-- AWS CloudFormation returns the same template that was used when the stack was created.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsTemplateBody :: Lens.Lens' GetTemplateResponse (Core.Maybe Types.TemplateBody)
gtrrsTemplateBody = Lens.field @"templateBody"
{-# DEPRECATED gtrrsTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsResponseStatus :: Lens.Lens' GetTemplateResponse Core.Int
gtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
