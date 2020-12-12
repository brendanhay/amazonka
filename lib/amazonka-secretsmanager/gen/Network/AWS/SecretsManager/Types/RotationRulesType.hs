{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.Types.RotationRulesType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecretsManager.Types.RotationRulesType
  ( RotationRulesType (..),

    -- * Smart constructor
    mkRotationRulesType,

    -- * Lenses
    rrtAutomaticallyAfterDays,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A structure that defines the rotation configuration for the secret.
--
-- /See:/ 'mkRotationRulesType' smart constructor.
newtype RotationRulesType = RotationRulesType'
  { automaticallyAfterDays ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RotationRulesType' with the minimum fields required to make a request.
--
-- * 'automaticallyAfterDays' - Specifies the number of days between automatic scheduled rotations of the secret.
--
-- Secrets Manager schedules the next rotation when the previous one is complete. Secrets Manager schedules the date by adding the rotation interval (number of days) to the actual date of the last rotation. The service chooses the hour within that 24-hour date window randomly. The minute is also chosen somewhat randomly, but weighted towards the top of the hour and influenced by a variety of factors that help distribute load.
mkRotationRulesType ::
  RotationRulesType
mkRotationRulesType =
  RotationRulesType' {automaticallyAfterDays = Lude.Nothing}

-- | Specifies the number of days between automatic scheduled rotations of the secret.
--
-- Secrets Manager schedules the next rotation when the previous one is complete. Secrets Manager schedules the date by adding the rotation interval (number of days) to the actual date of the last rotation. The service chooses the hour within that 24-hour date window randomly. The minute is also chosen somewhat randomly, but weighted towards the top of the hour and influenced by a variety of factors that help distribute load.
--
-- /Note:/ Consider using 'automaticallyAfterDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrtAutomaticallyAfterDays :: Lens.Lens' RotationRulesType (Lude.Maybe Lude.Natural)
rrtAutomaticallyAfterDays = Lens.lens (automaticallyAfterDays :: RotationRulesType -> Lude.Maybe Lude.Natural) (\s a -> s {automaticallyAfterDays = a} :: RotationRulesType)
{-# DEPRECATED rrtAutomaticallyAfterDays "Use generic-lens or generic-optics with 'automaticallyAfterDays' instead." #-}

instance Lude.FromJSON RotationRulesType where
  parseJSON =
    Lude.withObject
      "RotationRulesType"
      ( \x ->
          RotationRulesType' Lude.<$> (x Lude..:? "AutomaticallyAfterDays")
      )

instance Lude.ToJSON RotationRulesType where
  toJSON RotationRulesType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AutomaticallyAfterDays" Lude..=)
              Lude.<$> automaticallyAfterDays
          ]
      )
