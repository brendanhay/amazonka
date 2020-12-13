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
    dtcfTrialComponentName,
    dtcfTrialName,

    -- * Destructuring the response
    DisassociateTrialComponentResponse (..),
    mkDisassociateTrialComponentResponse,

    -- ** Response lenses
    dtcrsTrialARN,
    dtcrsTrialComponentARN,
    dtcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDisassociateTrialComponent' smart constructor.
data DisassociateTrialComponent = DisassociateTrialComponent'
  { -- | The name of the component to disassociate from the trial.
    trialComponentName :: Lude.Text,
    -- | The name of the trial to disassociate from.
    trialName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateTrialComponent' with the minimum fields required to make a request.
--
-- * 'trialComponentName' - The name of the component to disassociate from the trial.
-- * 'trialName' - The name of the trial to disassociate from.
mkDisassociateTrialComponent ::
  -- | 'trialComponentName'
  Lude.Text ->
  -- | 'trialName'
  Lude.Text ->
  DisassociateTrialComponent
mkDisassociateTrialComponent pTrialComponentName_ pTrialName_ =
  DisassociateTrialComponent'
    { trialComponentName =
        pTrialComponentName_,
      trialName = pTrialName_
    }

-- | The name of the component to disassociate from the trial.
--
-- /Note:/ Consider using 'trialComponentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcfTrialComponentName :: Lens.Lens' DisassociateTrialComponent Lude.Text
dtcfTrialComponentName = Lens.lens (trialComponentName :: DisassociateTrialComponent -> Lude.Text) (\s a -> s {trialComponentName = a} :: DisassociateTrialComponent)
{-# DEPRECATED dtcfTrialComponentName "Use generic-lens or generic-optics with 'trialComponentName' instead." #-}

-- | The name of the trial to disassociate from.
--
-- /Note:/ Consider using 'trialName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcfTrialName :: Lens.Lens' DisassociateTrialComponent Lude.Text
dtcfTrialName = Lens.lens (trialName :: DisassociateTrialComponent -> Lude.Text) (\s a -> s {trialName = a} :: DisassociateTrialComponent)
{-# DEPRECATED dtcfTrialName "Use generic-lens or generic-optics with 'trialName' instead." #-}

instance Lude.AWSRequest DisassociateTrialComponent where
  type
    Rs DisassociateTrialComponent =
      DisassociateTrialComponentResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DisassociateTrialComponentResponse'
            Lude.<$> (x Lude..?> "TrialArn")
            Lude.<*> (x Lude..?> "TrialComponentArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateTrialComponent where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DisassociateTrialComponent" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateTrialComponent where
  toJSON DisassociateTrialComponent' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TrialComponentName" Lude..= trialComponentName),
            Lude.Just ("TrialName" Lude..= trialName)
          ]
      )

instance Lude.ToPath DisassociateTrialComponent where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateTrialComponent where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateTrialComponentResponse' smart constructor.
data DisassociateTrialComponentResponse = DisassociateTrialComponentResponse'
  { -- | The Amazon Resource Name (ARN) of the trial.
    trialARN :: Lude.Maybe Lude.Text,
    -- | The ARN of the trial component.
    trialComponentARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateTrialComponentResponse' with the minimum fields required to make a request.
--
-- * 'trialARN' - The Amazon Resource Name (ARN) of the trial.
-- * 'trialComponentARN' - The ARN of the trial component.
-- * 'responseStatus' - The response status code.
mkDisassociateTrialComponentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateTrialComponentResponse
mkDisassociateTrialComponentResponse pResponseStatus_ =
  DisassociateTrialComponentResponse'
    { trialARN = Lude.Nothing,
      trialComponentARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the trial.
--
-- /Note:/ Consider using 'trialARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrsTrialARN :: Lens.Lens' DisassociateTrialComponentResponse (Lude.Maybe Lude.Text)
dtcrsTrialARN = Lens.lens (trialARN :: DisassociateTrialComponentResponse -> Lude.Maybe Lude.Text) (\s a -> s {trialARN = a} :: DisassociateTrialComponentResponse)
{-# DEPRECATED dtcrsTrialARN "Use generic-lens or generic-optics with 'trialARN' instead." #-}

-- | The ARN of the trial component.
--
-- /Note:/ Consider using 'trialComponentARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrsTrialComponentARN :: Lens.Lens' DisassociateTrialComponentResponse (Lude.Maybe Lude.Text)
dtcrsTrialComponentARN = Lens.lens (trialComponentARN :: DisassociateTrialComponentResponse -> Lude.Maybe Lude.Text) (\s a -> s {trialComponentARN = a} :: DisassociateTrialComponentResponse)
{-# DEPRECATED dtcrsTrialComponentARN "Use generic-lens or generic-optics with 'trialComponentARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrsResponseStatus :: Lens.Lens' DisassociateTrialComponentResponse Lude.Int
dtcrsResponseStatus = Lens.lens (responseStatus :: DisassociateTrialComponentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateTrialComponentResponse)
{-# DEPRECATED dtcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
