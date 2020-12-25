{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeHumanTaskUi
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the requested human task user interface (worker task template).
module Network.AWS.SageMaker.DescribeHumanTaskUi
  ( -- * Creating a request
    DescribeHumanTaskUi (..),
    mkDescribeHumanTaskUi,

    -- ** Request lenses
    dHumanTaskUiName,

    -- * Destructuring the response
    DescribeHumanTaskUiResponse (..),
    mkDescribeHumanTaskUiResponse,

    -- ** Response lenses
    dhturfrsHumanTaskUiArn,
    dhturfrsHumanTaskUiName,
    dhturfrsCreationTime,
    dhturfrsUiTemplate,
    dhturfrsHumanTaskUiStatus,
    dhturfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeHumanTaskUi' smart constructor.
newtype DescribeHumanTaskUi = DescribeHumanTaskUi'
  { -- | The name of the human task user interface (worker task template) you want information about.
    humanTaskUiName :: Types.HumanTaskUiName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeHumanTaskUi' value with any optional fields omitted.
mkDescribeHumanTaskUi ::
  -- | 'humanTaskUiName'
  Types.HumanTaskUiName ->
  DescribeHumanTaskUi
mkDescribeHumanTaskUi humanTaskUiName =
  DescribeHumanTaskUi' {humanTaskUiName}

-- | The name of the human task user interface (worker task template) you want information about.
--
-- /Note:/ Consider using 'humanTaskUiName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dHumanTaskUiName :: Lens.Lens' DescribeHumanTaskUi Types.HumanTaskUiName
dHumanTaskUiName = Lens.field @"humanTaskUiName"
{-# DEPRECATED dHumanTaskUiName "Use generic-lens or generic-optics with 'humanTaskUiName' instead." #-}

instance Core.FromJSON DescribeHumanTaskUi where
  toJSON DescribeHumanTaskUi {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("HumanTaskUiName" Core..= humanTaskUiName)]
      )

instance Core.AWSRequest DescribeHumanTaskUi where
  type Rs DescribeHumanTaskUi = DescribeHumanTaskUiResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DescribeHumanTaskUi")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeHumanTaskUiResponse'
            Core.<$> (x Core..: "HumanTaskUiArn")
            Core.<*> (x Core..: "HumanTaskUiName")
            Core.<*> (x Core..: "CreationTime")
            Core.<*> (x Core..: "UiTemplate")
            Core.<*> (x Core..:? "HumanTaskUiStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeHumanTaskUiResponse' smart constructor.
data DescribeHumanTaskUiResponse = DescribeHumanTaskUiResponse'
  { -- | The Amazon Resource Name (ARN) of the human task user interface (worker task template).
    humanTaskUiArn :: Types.HumanTaskUiArn,
    -- | The name of the human task user interface (worker task template).
    humanTaskUiName :: Types.HumanTaskUiName,
    -- | The timestamp when the human task user interface was created.
    creationTime :: Core.NominalDiffTime,
    uiTemplate :: Types.UiTemplateInfo,
    -- | The status of the human task user interface (worker task template). Valid values are listed below.
    humanTaskUiStatus :: Core.Maybe Types.HumanTaskUiStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeHumanTaskUiResponse' value with any optional fields omitted.
mkDescribeHumanTaskUiResponse ::
  -- | 'humanTaskUiArn'
  Types.HumanTaskUiArn ->
  -- | 'humanTaskUiName'
  Types.HumanTaskUiName ->
  -- | 'creationTime'
  Core.NominalDiffTime ->
  -- | 'uiTemplate'
  Types.UiTemplateInfo ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeHumanTaskUiResponse
mkDescribeHumanTaskUiResponse
  humanTaskUiArn
  humanTaskUiName
  creationTime
  uiTemplate
  responseStatus =
    DescribeHumanTaskUiResponse'
      { humanTaskUiArn,
        humanTaskUiName,
        creationTime,
        uiTemplate,
        humanTaskUiStatus = Core.Nothing,
        responseStatus
      }

-- | The Amazon Resource Name (ARN) of the human task user interface (worker task template).
--
-- /Note:/ Consider using 'humanTaskUiArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhturfrsHumanTaskUiArn :: Lens.Lens' DescribeHumanTaskUiResponse Types.HumanTaskUiArn
dhturfrsHumanTaskUiArn = Lens.field @"humanTaskUiArn"
{-# DEPRECATED dhturfrsHumanTaskUiArn "Use generic-lens or generic-optics with 'humanTaskUiArn' instead." #-}

-- | The name of the human task user interface (worker task template).
--
-- /Note:/ Consider using 'humanTaskUiName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhturfrsHumanTaskUiName :: Lens.Lens' DescribeHumanTaskUiResponse Types.HumanTaskUiName
dhturfrsHumanTaskUiName = Lens.field @"humanTaskUiName"
{-# DEPRECATED dhturfrsHumanTaskUiName "Use generic-lens or generic-optics with 'humanTaskUiName' instead." #-}

-- | The timestamp when the human task user interface was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhturfrsCreationTime :: Lens.Lens' DescribeHumanTaskUiResponse Core.NominalDiffTime
dhturfrsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED dhturfrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'uiTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhturfrsUiTemplate :: Lens.Lens' DescribeHumanTaskUiResponse Types.UiTemplateInfo
dhturfrsUiTemplate = Lens.field @"uiTemplate"
{-# DEPRECATED dhturfrsUiTemplate "Use generic-lens or generic-optics with 'uiTemplate' instead." #-}

-- | The status of the human task user interface (worker task template). Valid values are listed below.
--
-- /Note:/ Consider using 'humanTaskUiStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhturfrsHumanTaskUiStatus :: Lens.Lens' DescribeHumanTaskUiResponse (Core.Maybe Types.HumanTaskUiStatus)
dhturfrsHumanTaskUiStatus = Lens.field @"humanTaskUiStatus"
{-# DEPRECATED dhturfrsHumanTaskUiStatus "Use generic-lens or generic-optics with 'humanTaskUiStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhturfrsResponseStatus :: Lens.Lens' DescribeHumanTaskUiResponse Core.Int
dhturfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dhturfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
