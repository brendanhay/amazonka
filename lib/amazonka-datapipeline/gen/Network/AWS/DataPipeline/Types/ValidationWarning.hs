{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.ValidationWarning
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.ValidationWarning
  ( ValidationWarning (..),

    -- * Smart constructor
    mkValidationWarning,

    -- * Lenses
    vwWarnings,
    vwId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Defines a validation warning. Validation warnings do not prevent pipeline activation. The set of validation warnings that can be returned are defined by AWS Data Pipeline.
--
-- /See:/ 'mkValidationWarning' smart constructor.
data ValidationWarning = ValidationWarning'
  { -- | A description of the validation warning.
    warnings :: Lude.Maybe [Lude.Text],
    -- | The identifier of the object that contains the validation warning.
    id :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ValidationWarning' with the minimum fields required to make a request.
--
-- * 'warnings' - A description of the validation warning.
-- * 'id' - The identifier of the object that contains the validation warning.
mkValidationWarning ::
  ValidationWarning
mkValidationWarning =
  ValidationWarning' {warnings = Lude.Nothing, id = Lude.Nothing}

-- | A description of the validation warning.
--
-- /Note:/ Consider using 'warnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vwWarnings :: Lens.Lens' ValidationWarning (Lude.Maybe [Lude.Text])
vwWarnings = Lens.lens (warnings :: ValidationWarning -> Lude.Maybe [Lude.Text]) (\s a -> s {warnings = a} :: ValidationWarning)
{-# DEPRECATED vwWarnings "Use generic-lens or generic-optics with 'warnings' instead." #-}

-- | The identifier of the object that contains the validation warning.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vwId :: Lens.Lens' ValidationWarning (Lude.Maybe Lude.Text)
vwId = Lens.lens (id :: ValidationWarning -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: ValidationWarning)
{-# DEPRECATED vwId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON ValidationWarning where
  parseJSON =
    Lude.withObject
      "ValidationWarning"
      ( \x ->
          ValidationWarning'
            Lude.<$> (x Lude..:? "warnings" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "id")
      )
