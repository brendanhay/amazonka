{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.ProblemDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.ProblemDetail
  ( ProblemDetail (..),

    -- * Smart constructor
    mkProblemDetail,

    -- * Lenses
    pdArn,
    pdName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a problem detail.
--
-- /See:/ 'mkProblemDetail' smart constructor.
data ProblemDetail = ProblemDetail'
  { -- | The problem detail's ARN.
    arn :: Lude.Maybe Lude.Text,
    -- | The problem detail's name.
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProblemDetail' with the minimum fields required to make a request.
--
-- * 'arn' - The problem detail's ARN.
-- * 'name' - The problem detail's name.
mkProblemDetail ::
  ProblemDetail
mkProblemDetail =
  ProblemDetail' {arn = Lude.Nothing, name = Lude.Nothing}

-- | The problem detail's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdArn :: Lens.Lens' ProblemDetail (Lude.Maybe Lude.Text)
pdArn = Lens.lens (arn :: ProblemDetail -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: ProblemDetail)
{-# DEPRECATED pdArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The problem detail's name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdName :: Lens.Lens' ProblemDetail (Lude.Maybe Lude.Text)
pdName = Lens.lens (name :: ProblemDetail -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ProblemDetail)
{-# DEPRECATED pdName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON ProblemDetail where
  parseJSON =
    Lude.withObject
      "ProblemDetail"
      ( \x ->
          ProblemDetail'
            Lude.<$> (x Lude..:? "arn") Lude.<*> (x Lude..:? "name")
      )
