{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.AssociateTrialComponent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a trial component with a trial. A trial component can be associated with multiple trials. To disassociate a trial component from a trial, call the 'DisassociateTrialComponent' API.
module Network.AWS.SageMaker.AssociateTrialComponent
  ( -- * Creating a request
    AssociateTrialComponent (..),
    mkAssociateTrialComponent,

    -- ** Request lenses
    atcTrialComponentName,
    atcTrialName,

    -- * Destructuring the response
    AssociateTrialComponentResponse (..),
    mkAssociateTrialComponentResponse,

    -- ** Response lenses
    atcrrsTrialArn,
    atcrrsTrialComponentArn,
    atcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkAssociateTrialComponent' smart constructor.
data AssociateTrialComponent = AssociateTrialComponent'
  { -- | The name of the component to associated with the trial.
    trialComponentName :: Types.ExperimentEntityName,
    -- | The name of the trial to associate with.
    trialName :: Types.ExperimentEntityName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateTrialComponent' value with any optional fields omitted.
mkAssociateTrialComponent ::
  -- | 'trialComponentName'
  Types.ExperimentEntityName ->
  -- | 'trialName'
  Types.ExperimentEntityName ->
  AssociateTrialComponent
mkAssociateTrialComponent trialComponentName trialName =
  AssociateTrialComponent' {trialComponentName, trialName}

-- | The name of the component to associated with the trial.
--
-- /Note:/ Consider using 'trialComponentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcTrialComponentName :: Lens.Lens' AssociateTrialComponent Types.ExperimentEntityName
atcTrialComponentName = Lens.field @"trialComponentName"
{-# DEPRECATED atcTrialComponentName "Use generic-lens or generic-optics with 'trialComponentName' instead." #-}

-- | The name of the trial to associate with.
--
-- /Note:/ Consider using 'trialName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcTrialName :: Lens.Lens' AssociateTrialComponent Types.ExperimentEntityName
atcTrialName = Lens.field @"trialName"
{-# DEPRECATED atcTrialName "Use generic-lens or generic-optics with 'trialName' instead." #-}

instance Core.FromJSON AssociateTrialComponent where
  toJSON AssociateTrialComponent {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TrialComponentName" Core..= trialComponentName),
            Core.Just ("TrialName" Core..= trialName)
          ]
      )

instance Core.AWSRequest AssociateTrialComponent where
  type Rs AssociateTrialComponent = AssociateTrialComponentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.AssociateTrialComponent")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateTrialComponentResponse'
            Core.<$> (x Core..:? "TrialArn")
            Core.<*> (x Core..:? "TrialComponentArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAssociateTrialComponentResponse' smart constructor.
data AssociateTrialComponentResponse = AssociateTrialComponentResponse'
  { -- | The Amazon Resource Name (ARN) of the trial.
    trialArn :: Core.Maybe Types.TrialArn,
    -- | The ARN of the trial component.
    trialComponentArn :: Core.Maybe Types.TrialComponentArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateTrialComponentResponse' value with any optional fields omitted.
mkAssociateTrialComponentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AssociateTrialComponentResponse
mkAssociateTrialComponentResponse responseStatus =
  AssociateTrialComponentResponse'
    { trialArn = Core.Nothing,
      trialComponentArn = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the trial.
--
-- /Note:/ Consider using 'trialArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcrrsTrialArn :: Lens.Lens' AssociateTrialComponentResponse (Core.Maybe Types.TrialArn)
atcrrsTrialArn = Lens.field @"trialArn"
{-# DEPRECATED atcrrsTrialArn "Use generic-lens or generic-optics with 'trialArn' instead." #-}

-- | The ARN of the trial component.
--
-- /Note:/ Consider using 'trialComponentArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcrrsTrialComponentArn :: Lens.Lens' AssociateTrialComponentResponse (Core.Maybe Types.TrialComponentArn)
atcrrsTrialComponentArn = Lens.field @"trialComponentArn"
{-# DEPRECATED atcrrsTrialComponentArn "Use generic-lens or generic-optics with 'trialComponentArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcrrsResponseStatus :: Lens.Lens' AssociateTrialComponentResponse Core.Int
atcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED atcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
