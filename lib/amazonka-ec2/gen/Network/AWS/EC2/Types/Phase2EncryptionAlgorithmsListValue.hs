-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase2EncryptionAlgorithmsListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase2EncryptionAlgorithmsListValue
  ( Phase2EncryptionAlgorithmsListValue (..),

    -- * Smart constructor
    mkPhase2EncryptionAlgorithmsListValue,

    -- * Lenses
    pealvValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The encryption algorithm for phase 2 IKE negotiations.
--
-- /See:/ 'mkPhase2EncryptionAlgorithmsListValue' smart constructor.
newtype Phase2EncryptionAlgorithmsListValue = Phase2EncryptionAlgorithmsListValue'
  { value ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Phase2EncryptionAlgorithmsListValue' with the minimum fields required to make a request.
--
-- * 'value' - The encryption algorithm.
mkPhase2EncryptionAlgorithmsListValue ::
  Phase2EncryptionAlgorithmsListValue
mkPhase2EncryptionAlgorithmsListValue =
  Phase2EncryptionAlgorithmsListValue' {value = Lude.Nothing}

-- | The encryption algorithm.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pealvValue :: Lens.Lens' Phase2EncryptionAlgorithmsListValue (Lude.Maybe Lude.Text)
pealvValue = Lens.lens (value :: Phase2EncryptionAlgorithmsListValue -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: Phase2EncryptionAlgorithmsListValue)
{-# DEPRECATED pealvValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromXML Phase2EncryptionAlgorithmsListValue where
  parseXML x =
    Phase2EncryptionAlgorithmsListValue'
      Lude.<$> (x Lude..@? "value")
