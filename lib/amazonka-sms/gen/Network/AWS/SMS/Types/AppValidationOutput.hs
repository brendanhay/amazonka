{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.AppValidationOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.AppValidationOutput
  ( AppValidationOutput (..),

    -- * Smart constructor
    mkAppValidationOutput,

    -- * Lenses
    avoSsmOutput,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SMS.Types.SSMOutput

-- | Output from validating an application.
--
-- /See:/ 'mkAppValidationOutput' smart constructor.
newtype AppValidationOutput = AppValidationOutput'
  { -- | Output from using SSM to validate the application.
    ssmOutput :: Lude.Maybe SSMOutput
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AppValidationOutput' with the minimum fields required to make a request.
--
-- * 'ssmOutput' - Output from using SSM to validate the application.
mkAppValidationOutput ::
  AppValidationOutput
mkAppValidationOutput =
  AppValidationOutput' {ssmOutput = Lude.Nothing}

-- | Output from using SSM to validate the application.
--
-- /Note:/ Consider using 'ssmOutput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avoSsmOutput :: Lens.Lens' AppValidationOutput (Lude.Maybe SSMOutput)
avoSsmOutput = Lens.lens (ssmOutput :: AppValidationOutput -> Lude.Maybe SSMOutput) (\s a -> s {ssmOutput = a} :: AppValidationOutput)
{-# DEPRECATED avoSsmOutput "Use generic-lens or generic-optics with 'ssmOutput' instead." #-}

instance Lude.FromJSON AppValidationOutput where
  parseJSON =
    Lude.withObject
      "AppValidationOutput"
      (\x -> AppValidationOutput' Lude.<$> (x Lude..:? "ssmOutput"))
