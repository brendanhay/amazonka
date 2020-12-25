{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DisassociateTrialComponent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a trial component from a trial. This doesn't effect other trials the component is associated with. Before you can delete a component, you must disassociate the component from all trials it is associated with. To associate a trial component with a trial, call the 'AssociateTrialComponent' API.
--
-- To get a list of the trials a component is associated with, use the 'Search' API. Specify @ExperimentTrialComponent@ for the @Resource@ parameter. The list appears in the response under @Results.TrialComponent.Parents@ .
module Network.AWS.SageMaker.DisassociateTrialComponent
  ( -- * Creating a request
    DisassociateTrialComponent (..),
    mkDisassociateTrialComponent,

    -- ** Request lenses
    dtcTrialComponentName,
    dtcTrialName,

    -- * Destructuring the response
    DisassociateTrialComponentResponse (..),
    mkDisassociateTrialComponentResponse,

    -- ** Response lenses
    dtcrrsTrialArn,
    dtcrrsTrialComponentArn,
    dtcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDisassociateTrialComponent' smart constructor.
data DisassociateTrialComponent = DisassociateTrialComponent'
  { -- | The name of the component to disassociate from the trial.
    trialComponentName :: Types.ExperimentEntityName,
    -- | The name of the trial to disassociate from.
    trialName :: Types.ExperimentEntityName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateTrialComponent' value with any optional fields omitted.
mkDisassociateTrialComponent ::
  -- | 'trialComponentName'
  Types.ExperimentEntityName ->
  -- | 'trialName'
  Types.ExperimentEntityName ->
  DisassociateTrialComponent
mkDisassociateTrialComponent trialComponentName trialName =
  DisassociateTrialComponent' {trialComponentName, trialName}

-- | The name of the component to disassociate from the trial.
--
-- /Note:/ Consider using 'trialComponentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcTrialComponentName :: Lens.Lens' DisassociateTrialComponent Types.ExperimentEntityName
dtcTrialComponentName = Lens.field @"trialComponentName"
{-# DEPRECATED dtcTrialComponentName "Use generic-lens or generic-optics with 'trialComponentName' instead." #-}

-- | The name of the trial to disassociate from.
--
-- /Note:/ Consider using 'trialName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcTrialName :: Lens.Lens' DisassociateTrialComponent Types.ExperimentEntityName
dtcTrialName = Lens.field @"trialName"
{-# DEPRECATED dtcTrialName "Use generic-lens or generic-optics with 'trialName' instead." #-}

instance Core.FromJSON DisassociateTrialComponent where
  toJSON DisassociateTrialComponent {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TrialComponentName" Core..= trialComponentName),
            Core.Just ("TrialName" Core..= trialName)
          ]
      )

instance Core.AWSRequest DisassociateTrialComponent where
  type
    Rs DisassociateTrialComponent =
      DisassociateTrialComponentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DisassociateTrialComponent")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateTrialComponentResponse'
            Core.<$> (x Core..:? "TrialArn")
            Core.<*> (x Core..:? "TrialComponentArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisassociateTrialComponentResponse' smart constructor.
data DisassociateTrialComponentResponse = DisassociateTrialComponentResponse'
  { -- | The Amazon Resource Name (ARN) of the trial.
    trialArn :: Core.Maybe Types.TrialArn,
    -- | The ARN of the trial component.
    trialComponentArn :: Core.Maybe Types.TrialComponentArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateTrialComponentResponse' value with any optional fields omitted.
mkDisassociateTrialComponentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisassociateTrialComponentResponse
mkDisassociateTrialComponentResponse responseStatus =
  DisassociateTrialComponentResponse'
    { trialArn = Core.Nothing,
      trialComponentArn = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the trial.
--
-- /Note:/ Consider using 'trialArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrrsTrialArn :: Lens.Lens' DisassociateTrialComponentResponse (Core.Maybe Types.TrialArn)
dtcrrsTrialArn = Lens.field @"trialArn"
{-# DEPRECATED dtcrrsTrialArn "Use generic-lens or generic-optics with 'trialArn' instead." #-}

-- | The ARN of the trial component.
--
-- /Note:/ Consider using 'trialComponentArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrrsTrialComponentArn :: Lens.Lens' DisassociateTrialComponentResponse (Core.Maybe Types.TrialComponentArn)
dtcrrsTrialComponentArn = Lens.field @"trialComponentArn"
{-# DEPRECATED dtcrrsTrialComponentArn "Use generic-lens or generic-optics with 'trialComponentArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrrsResponseStatus :: Lens.Lens' DisassociateTrialComponentResponse Core.Int
dtcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
