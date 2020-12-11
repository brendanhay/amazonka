-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase2EncryptionAlgorithmsRequestListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase2EncryptionAlgorithmsRequestListValue
  ( Phase2EncryptionAlgorithmsRequestListValue (..),

    -- * Smart constructor
    mkPhase2EncryptionAlgorithmsRequestListValue,

    -- * Lenses
    pearlveValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the encryption algorithm for the VPN tunnel for phase 2 IKE negotiations.
--
-- /See:/ 'mkPhase2EncryptionAlgorithmsRequestListValue' smart constructor.
newtype Phase2EncryptionAlgorithmsRequestListValue = Phase2EncryptionAlgorithmsRequestListValue'
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

-- | Creates a value of 'Phase2EncryptionAlgorithmsRequestListValue' with the minimum fields required to make a request.
--
-- * 'value' - The encryption algorithm.
mkPhase2EncryptionAlgorithmsRequestListValue ::
  Phase2EncryptionAlgorithmsRequestListValue
mkPhase2EncryptionAlgorithmsRequestListValue =
  Phase2EncryptionAlgorithmsRequestListValue' {value = Lude.Nothing}

-- | The encryption algorithm.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pearlveValue :: Lens.Lens' Phase2EncryptionAlgorithmsRequestListValue (Lude.Maybe Lude.Text)
pearlveValue = Lens.lens (value :: Phase2EncryptionAlgorithmsRequestListValue -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: Phase2EncryptionAlgorithmsRequestListValue)
{-# DEPRECATED pearlveValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.ToQuery Phase2EncryptionAlgorithmsRequestListValue where
  toQuery Phase2EncryptionAlgorithmsRequestListValue' {..} =
    Lude.mconcat ["Value" Lude.=: value]
