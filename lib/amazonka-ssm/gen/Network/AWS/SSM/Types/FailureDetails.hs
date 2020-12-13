{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.FailureDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.FailureDetails
  ( FailureDetails (..),

    -- * Smart constructor
    mkFailureDetails,

    -- * Lenses
    fdFailureType,
    fdFailureStage,
    fdDetails,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an Automation failure.
--
-- /See:/ 'mkFailureDetails' smart constructor.
data FailureDetails = FailureDetails'
  { -- | The type of Automation failure. Failure types include the following: Action, Permission, Throttling, Verification, Internal.
    failureType :: Lude.Maybe Lude.Text,
    -- | The stage of the Automation execution when the failure occurred. The stages include the following: InputValidation, PreVerification, Invocation, PostVerification.
    failureStage :: Lude.Maybe Lude.Text,
    -- | Detailed information about the Automation step failure.
    details :: Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FailureDetails' with the minimum fields required to make a request.
--
-- * 'failureType' - The type of Automation failure. Failure types include the following: Action, Permission, Throttling, Verification, Internal.
-- * 'failureStage' - The stage of the Automation execution when the failure occurred. The stages include the following: InputValidation, PreVerification, Invocation, PostVerification.
-- * 'details' - Detailed information about the Automation step failure.
mkFailureDetails ::
  FailureDetails
mkFailureDetails =
  FailureDetails'
    { failureType = Lude.Nothing,
      failureStage = Lude.Nothing,
      details = Lude.Nothing
    }

-- | The type of Automation failure. Failure types include the following: Action, Permission, Throttling, Verification, Internal.
--
-- /Note:/ Consider using 'failureType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdFailureType :: Lens.Lens' FailureDetails (Lude.Maybe Lude.Text)
fdFailureType = Lens.lens (failureType :: FailureDetails -> Lude.Maybe Lude.Text) (\s a -> s {failureType = a} :: FailureDetails)
{-# DEPRECATED fdFailureType "Use generic-lens or generic-optics with 'failureType' instead." #-}

-- | The stage of the Automation execution when the failure occurred. The stages include the following: InputValidation, PreVerification, Invocation, PostVerification.
--
-- /Note:/ Consider using 'failureStage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdFailureStage :: Lens.Lens' FailureDetails (Lude.Maybe Lude.Text)
fdFailureStage = Lens.lens (failureStage :: FailureDetails -> Lude.Maybe Lude.Text) (\s a -> s {failureStage = a} :: FailureDetails)
{-# DEPRECATED fdFailureStage "Use generic-lens or generic-optics with 'failureStage' instead." #-}

-- | Detailed information about the Automation step failure.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdDetails :: Lens.Lens' FailureDetails (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
fdDetails = Lens.lens (details :: FailureDetails -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {details = a} :: FailureDetails)
{-# DEPRECATED fdDetails "Use generic-lens or generic-optics with 'details' instead." #-}

instance Lude.FromJSON FailureDetails where
  parseJSON =
    Lude.withObject
      "FailureDetails"
      ( \x ->
          FailureDetails'
            Lude.<$> (x Lude..:? "FailureType")
            Lude.<*> (x Lude..:? "FailureStage")
            Lude.<*> (x Lude..:? "Details" Lude..!= Lude.mempty)
      )
