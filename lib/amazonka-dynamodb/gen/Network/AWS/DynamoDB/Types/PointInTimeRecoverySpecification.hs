-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.PointInTimeRecoverySpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.PointInTimeRecoverySpecification
  ( PointInTimeRecoverySpecification (..),

    -- * Smart constructor
    mkPointInTimeRecoverySpecification,

    -- * Lenses
    pitrsPointInTimeRecoveryEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the settings used to enable point in time recovery.
--
-- /See:/ 'mkPointInTimeRecoverySpecification' smart constructor.
newtype PointInTimeRecoverySpecification = PointInTimeRecoverySpecification'
  { pointInTimeRecoveryEnabled ::
      Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PointInTimeRecoverySpecification' with the minimum fields required to make a request.
--
-- * 'pointInTimeRecoveryEnabled' - Indicates whether point in time recovery is enabled (true) or disabled (false) on the table.
mkPointInTimeRecoverySpecification ::
  -- | 'pointInTimeRecoveryEnabled'
  Lude.Bool ->
  PointInTimeRecoverySpecification
mkPointInTimeRecoverySpecification pPointInTimeRecoveryEnabled_ =
  PointInTimeRecoverySpecification'
    { pointInTimeRecoveryEnabled =
        pPointInTimeRecoveryEnabled_
    }

-- | Indicates whether point in time recovery is enabled (true) or disabled (false) on the table.
--
-- /Note:/ Consider using 'pointInTimeRecoveryEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pitrsPointInTimeRecoveryEnabled :: Lens.Lens' PointInTimeRecoverySpecification Lude.Bool
pitrsPointInTimeRecoveryEnabled = Lens.lens (pointInTimeRecoveryEnabled :: PointInTimeRecoverySpecification -> Lude.Bool) (\s a -> s {pointInTimeRecoveryEnabled = a} :: PointInTimeRecoverySpecification)
{-# DEPRECATED pitrsPointInTimeRecoveryEnabled "Use generic-lens or generic-optics with 'pointInTimeRecoveryEnabled' instead." #-}

instance Lude.ToJSON PointInTimeRecoverySpecification where
  toJSON PointInTimeRecoverySpecification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("PointInTimeRecoveryEnabled" Lude..= pointInTimeRecoveryEnabled)
          ]
      )
