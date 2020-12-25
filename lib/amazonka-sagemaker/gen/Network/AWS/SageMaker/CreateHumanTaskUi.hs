{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateHumanTaskUi
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Defines the settings you will use for the human review workflow user interface. Reviewers will see a three-panel interface with an instruction area, the item to review, and an input area.
module Network.AWS.SageMaker.CreateHumanTaskUi
  ( -- * Creating a request
    CreateHumanTaskUi (..),
    mkCreateHumanTaskUi,

    -- ** Request lenses
    chtuHumanTaskUiName,
    chtuUiTemplate,
    chtuTags,

    -- * Destructuring the response
    CreateHumanTaskUiResponse (..),
    mkCreateHumanTaskUiResponse,

    -- ** Response lenses
    chturrsHumanTaskUiArn,
    chturrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreateHumanTaskUi' smart constructor.
data CreateHumanTaskUi = CreateHumanTaskUi'
  { -- | The name of the user interface you are creating.
    humanTaskUiName :: Types.HumanTaskUiName,
    uiTemplate :: Types.UiTemplate,
    -- | An array of key-value pairs that contain metadata to help you categorize and organize a human review workflow user interface. Each tag consists of a key and a value, both of which you define.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateHumanTaskUi' value with any optional fields omitted.
mkCreateHumanTaskUi ::
  -- | 'humanTaskUiName'
  Types.HumanTaskUiName ->
  -- | 'uiTemplate'
  Types.UiTemplate ->
  CreateHumanTaskUi
mkCreateHumanTaskUi humanTaskUiName uiTemplate =
  CreateHumanTaskUi'
    { humanTaskUiName,
      uiTemplate,
      tags = Core.Nothing
    }

-- | The name of the user interface you are creating.
--
-- /Note:/ Consider using 'humanTaskUiName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chtuHumanTaskUiName :: Lens.Lens' CreateHumanTaskUi Types.HumanTaskUiName
chtuHumanTaskUiName = Lens.field @"humanTaskUiName"
{-# DEPRECATED chtuHumanTaskUiName "Use generic-lens or generic-optics with 'humanTaskUiName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'uiTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chtuUiTemplate :: Lens.Lens' CreateHumanTaskUi Types.UiTemplate
chtuUiTemplate = Lens.field @"uiTemplate"
{-# DEPRECATED chtuUiTemplate "Use generic-lens or generic-optics with 'uiTemplate' instead." #-}

-- | An array of key-value pairs that contain metadata to help you categorize and organize a human review workflow user interface. Each tag consists of a key and a value, both of which you define.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chtuTags :: Lens.Lens' CreateHumanTaskUi (Core.Maybe [Types.Tag])
chtuTags = Lens.field @"tags"
{-# DEPRECATED chtuTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateHumanTaskUi where
  toJSON CreateHumanTaskUi {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("HumanTaskUiName" Core..= humanTaskUiName),
            Core.Just ("UiTemplate" Core..= uiTemplate),
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateHumanTaskUi where
  type Rs CreateHumanTaskUi = CreateHumanTaskUiResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.CreateHumanTaskUi")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateHumanTaskUiResponse'
            Core.<$> (x Core..: "HumanTaskUiArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateHumanTaskUiResponse' smart constructor.
data CreateHumanTaskUiResponse = CreateHumanTaskUiResponse'
  { -- | The Amazon Resource Name (ARN) of the human review workflow user interface you create.
    humanTaskUiArn :: Types.HumanTaskUiArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateHumanTaskUiResponse' value with any optional fields omitted.
mkCreateHumanTaskUiResponse ::
  -- | 'humanTaskUiArn'
  Types.HumanTaskUiArn ->
  -- | 'responseStatus'
  Core.Int ->
  CreateHumanTaskUiResponse
mkCreateHumanTaskUiResponse humanTaskUiArn responseStatus =
  CreateHumanTaskUiResponse' {humanTaskUiArn, responseStatus}

-- | The Amazon Resource Name (ARN) of the human review workflow user interface you create.
--
-- /Note:/ Consider using 'humanTaskUiArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chturrsHumanTaskUiArn :: Lens.Lens' CreateHumanTaskUiResponse Types.HumanTaskUiArn
chturrsHumanTaskUiArn = Lens.field @"humanTaskUiArn"
{-# DEPRECATED chturrsHumanTaskUiArn "Use generic-lens or generic-optics with 'humanTaskUiArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chturrsResponseStatus :: Lens.Lens' CreateHumanTaskUiResponse Core.Int
chturrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED chturrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
