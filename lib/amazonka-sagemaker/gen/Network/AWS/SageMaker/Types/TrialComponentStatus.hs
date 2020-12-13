{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialComponentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialComponentStatus
  ( TrialComponentStatus (..),

    -- * Smart constructor
    mkTrialComponentStatus,

    -- * Lenses
    tcsPrimaryStatus,
    tcsMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.TrialComponentPrimaryStatus

-- | The status of the trial component.
--
-- /See:/ 'mkTrialComponentStatus' smart constructor.
data TrialComponentStatus = TrialComponentStatus'
  { -- | The status of the trial component.
    primaryStatus :: Lude.Maybe TrialComponentPrimaryStatus,
    -- | If the component failed, a message describing why.
    message :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrialComponentStatus' with the minimum fields required to make a request.
--
-- * 'primaryStatus' - The status of the trial component.
-- * 'message' - If the component failed, a message describing why.
mkTrialComponentStatus ::
  TrialComponentStatus
mkTrialComponentStatus =
  TrialComponentStatus'
    { primaryStatus = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The status of the trial component.
--
-- /Note:/ Consider using 'primaryStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsPrimaryStatus :: Lens.Lens' TrialComponentStatus (Lude.Maybe TrialComponentPrimaryStatus)
tcsPrimaryStatus = Lens.lens (primaryStatus :: TrialComponentStatus -> Lude.Maybe TrialComponentPrimaryStatus) (\s a -> s {primaryStatus = a} :: TrialComponentStatus)
{-# DEPRECATED tcsPrimaryStatus "Use generic-lens or generic-optics with 'primaryStatus' instead." #-}

-- | If the component failed, a message describing why.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsMessage :: Lens.Lens' TrialComponentStatus (Lude.Maybe Lude.Text)
tcsMessage = Lens.lens (message :: TrialComponentStatus -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: TrialComponentStatus)
{-# DEPRECATED tcsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON TrialComponentStatus where
  parseJSON =
    Lude.withObject
      "TrialComponentStatus"
      ( \x ->
          TrialComponentStatus'
            Lude.<$> (x Lude..:? "PrimaryStatus") Lude.<*> (x Lude..:? "Message")
      )

instance Lude.ToJSON TrialComponentStatus where
  toJSON TrialComponentStatus' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PrimaryStatus" Lude..=) Lude.<$> primaryStatus,
            ("Message" Lude..=) Lude.<$> message
          ]
      )
