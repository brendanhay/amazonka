{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeHumanTaskUi (..)
    , mkDescribeHumanTaskUi
    -- ** Request lenses
    , dHumanTaskUiName

    -- * Destructuring the response
    , DescribeHumanTaskUiResponse (..)
    , mkDescribeHumanTaskUiResponse
    -- ** Response lenses
    , dhturfrsHumanTaskUiArn
    , dhturfrsHumanTaskUiName
    , dhturfrsCreationTime
    , dhturfrsUiTemplate
    , dhturfrsHumanTaskUiStatus
    , dhturfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeHumanTaskUi' smart constructor.
newtype DescribeHumanTaskUi = DescribeHumanTaskUi'
  { humanTaskUiName :: Types.HumanTaskUiName
    -- ^ The name of the human task user interface (worker task template) you want information about.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeHumanTaskUi' value with any optional fields omitted.
mkDescribeHumanTaskUi
    :: Types.HumanTaskUiName -- ^ 'humanTaskUiName'
    -> DescribeHumanTaskUi
mkDescribeHumanTaskUi humanTaskUiName
  = DescribeHumanTaskUi'{humanTaskUiName}

-- | The name of the human task user interface (worker task template) you want information about.
--
-- /Note:/ Consider using 'humanTaskUiName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dHumanTaskUiName :: Lens.Lens' DescribeHumanTaskUi Types.HumanTaskUiName
dHumanTaskUiName = Lens.field @"humanTaskUiName"
{-# INLINEABLE dHumanTaskUiName #-}
{-# DEPRECATED humanTaskUiName "Use generic-lens or generic-optics with 'humanTaskUiName' instead"  #-}

instance Core.ToQuery DescribeHumanTaskUi where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeHumanTaskUi where
        toHeaders DescribeHumanTaskUi{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DescribeHumanTaskUi")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeHumanTaskUi where
        toJSON DescribeHumanTaskUi{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("HumanTaskUiName" Core..= humanTaskUiName)])

instance Core.AWSRequest DescribeHumanTaskUi where
        type Rs DescribeHumanTaskUi = DescribeHumanTaskUiResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeHumanTaskUiResponse' Core.<$>
                   (x Core..: "HumanTaskUiArn") Core.<*> x Core..: "HumanTaskUiName"
                     Core.<*> x Core..: "CreationTime"
                     Core.<*> x Core..: "UiTemplate"
                     Core.<*> x Core..:? "HumanTaskUiStatus"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeHumanTaskUiResponse' smart constructor.
data DescribeHumanTaskUiResponse = DescribeHumanTaskUiResponse'
  { humanTaskUiArn :: Types.HumanTaskUiArn
    -- ^ The Amazon Resource Name (ARN) of the human task user interface (worker task template).
  , humanTaskUiName :: Types.HumanTaskUiName
    -- ^ The name of the human task user interface (worker task template).
  , creationTime :: Core.NominalDiffTime
    -- ^ The timestamp when the human task user interface was created.
  , uiTemplate :: Types.UiTemplateInfo
  , humanTaskUiStatus :: Core.Maybe Types.HumanTaskUiStatus
    -- ^ The status of the human task user interface (worker task template). Valid values are listed below.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeHumanTaskUiResponse' value with any optional fields omitted.
mkDescribeHumanTaskUiResponse
    :: Types.HumanTaskUiArn -- ^ 'humanTaskUiArn'
    -> Types.HumanTaskUiName -- ^ 'humanTaskUiName'
    -> Core.NominalDiffTime -- ^ 'creationTime'
    -> Types.UiTemplateInfo -- ^ 'uiTemplate'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeHumanTaskUiResponse
mkDescribeHumanTaskUiResponse humanTaskUiArn humanTaskUiName
  creationTime uiTemplate responseStatus
  = DescribeHumanTaskUiResponse'{humanTaskUiArn, humanTaskUiName,
                                 creationTime, uiTemplate, humanTaskUiStatus = Core.Nothing,
                                 responseStatus}

-- | The Amazon Resource Name (ARN) of the human task user interface (worker task template).
--
-- /Note:/ Consider using 'humanTaskUiArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhturfrsHumanTaskUiArn :: Lens.Lens' DescribeHumanTaskUiResponse Types.HumanTaskUiArn
dhturfrsHumanTaskUiArn = Lens.field @"humanTaskUiArn"
{-# INLINEABLE dhturfrsHumanTaskUiArn #-}
{-# DEPRECATED humanTaskUiArn "Use generic-lens or generic-optics with 'humanTaskUiArn' instead"  #-}

-- | The name of the human task user interface (worker task template).
--
-- /Note:/ Consider using 'humanTaskUiName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhturfrsHumanTaskUiName :: Lens.Lens' DescribeHumanTaskUiResponse Types.HumanTaskUiName
dhturfrsHumanTaskUiName = Lens.field @"humanTaskUiName"
{-# INLINEABLE dhturfrsHumanTaskUiName #-}
{-# DEPRECATED humanTaskUiName "Use generic-lens or generic-optics with 'humanTaskUiName' instead"  #-}

-- | The timestamp when the human task user interface was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhturfrsCreationTime :: Lens.Lens' DescribeHumanTaskUiResponse Core.NominalDiffTime
dhturfrsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE dhturfrsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'uiTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhturfrsUiTemplate :: Lens.Lens' DescribeHumanTaskUiResponse Types.UiTemplateInfo
dhturfrsUiTemplate = Lens.field @"uiTemplate"
{-# INLINEABLE dhturfrsUiTemplate #-}
{-# DEPRECATED uiTemplate "Use generic-lens or generic-optics with 'uiTemplate' instead"  #-}

-- | The status of the human task user interface (worker task template). Valid values are listed below.
--
-- /Note:/ Consider using 'humanTaskUiStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhturfrsHumanTaskUiStatus :: Lens.Lens' DescribeHumanTaskUiResponse (Core.Maybe Types.HumanTaskUiStatus)
dhturfrsHumanTaskUiStatus = Lens.field @"humanTaskUiStatus"
{-# INLINEABLE dhturfrsHumanTaskUiStatus #-}
{-# DEPRECATED humanTaskUiStatus "Use generic-lens or generic-optics with 'humanTaskUiStatus' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhturfrsResponseStatus :: Lens.Lens' DescribeHumanTaskUiResponse Core.Int
dhturfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dhturfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
