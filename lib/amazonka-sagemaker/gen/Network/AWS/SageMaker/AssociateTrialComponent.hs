{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    atcrsTrialARN,
    atcrsTrialComponentARN,
    atcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkAssociateTrialComponent' smart constructor.
data AssociateTrialComponent = AssociateTrialComponent'
  { trialComponentName ::
      Lude.Text,
    trialName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateTrialComponent' with the minimum fields required to make a request.
--
-- * 'trialComponentName' - The name of the component to associated with the trial.
-- * 'trialName' - The name of the trial to associate with.
mkAssociateTrialComponent ::
  -- | 'trialComponentName'
  Lude.Text ->
  -- | 'trialName'
  Lude.Text ->
  AssociateTrialComponent
mkAssociateTrialComponent pTrialComponentName_ pTrialName_ =
  AssociateTrialComponent'
    { trialComponentName =
        pTrialComponentName_,
      trialName = pTrialName_
    }

-- | The name of the component to associated with the trial.
--
-- /Note:/ Consider using 'trialComponentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcTrialComponentName :: Lens.Lens' AssociateTrialComponent Lude.Text
atcTrialComponentName = Lens.lens (trialComponentName :: AssociateTrialComponent -> Lude.Text) (\s a -> s {trialComponentName = a} :: AssociateTrialComponent)
{-# DEPRECATED atcTrialComponentName "Use generic-lens or generic-optics with 'trialComponentName' instead." #-}

-- | The name of the trial to associate with.
--
-- /Note:/ Consider using 'trialName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcTrialName :: Lens.Lens' AssociateTrialComponent Lude.Text
atcTrialName = Lens.lens (trialName :: AssociateTrialComponent -> Lude.Text) (\s a -> s {trialName = a} :: AssociateTrialComponent)
{-# DEPRECATED atcTrialName "Use generic-lens or generic-optics with 'trialName' instead." #-}

instance Lude.AWSRequest AssociateTrialComponent where
  type Rs AssociateTrialComponent = AssociateTrialComponentResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          AssociateTrialComponentResponse'
            Lude.<$> (x Lude..?> "TrialArn")
            Lude.<*> (x Lude..?> "TrialComponentArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateTrialComponent where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.AssociateTrialComponent" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateTrialComponent where
  toJSON AssociateTrialComponent' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TrialComponentName" Lude..= trialComponentName),
            Lude.Just ("TrialName" Lude..= trialName)
          ]
      )

instance Lude.ToPath AssociateTrialComponent where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateTrialComponent where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateTrialComponentResponse' smart constructor.
data AssociateTrialComponentResponse = AssociateTrialComponentResponse'
  { trialARN ::
      Lude.Maybe Lude.Text,
    trialComponentARN ::
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

-- | Creates a value of 'AssociateTrialComponentResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'trialARN' - The Amazon Resource Name (ARN) of the trial.
-- * 'trialComponentARN' - The ARN of the trial component.
mkAssociateTrialComponentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateTrialComponentResponse
mkAssociateTrialComponentResponse pResponseStatus_ =
  AssociateTrialComponentResponse'
    { trialARN = Lude.Nothing,
      trialComponentARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the trial.
--
-- /Note:/ Consider using 'trialARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcrsTrialARN :: Lens.Lens' AssociateTrialComponentResponse (Lude.Maybe Lude.Text)
atcrsTrialARN = Lens.lens (trialARN :: AssociateTrialComponentResponse -> Lude.Maybe Lude.Text) (\s a -> s {trialARN = a} :: AssociateTrialComponentResponse)
{-# DEPRECATED atcrsTrialARN "Use generic-lens or generic-optics with 'trialARN' instead." #-}

-- | The ARN of the trial component.
--
-- /Note:/ Consider using 'trialComponentARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcrsTrialComponentARN :: Lens.Lens' AssociateTrialComponentResponse (Lude.Maybe Lude.Text)
atcrsTrialComponentARN = Lens.lens (trialComponentARN :: AssociateTrialComponentResponse -> Lude.Maybe Lude.Text) (\s a -> s {trialComponentARN = a} :: AssociateTrialComponentResponse)
{-# DEPRECATED atcrsTrialComponentARN "Use generic-lens or generic-optics with 'trialComponentARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcrsResponseStatus :: Lens.Lens' AssociateTrialComponentResponse Lude.Int
atcrsResponseStatus = Lens.lens (responseStatus :: AssociateTrialComponentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateTrialComponentResponse)
{-# DEPRECATED atcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
