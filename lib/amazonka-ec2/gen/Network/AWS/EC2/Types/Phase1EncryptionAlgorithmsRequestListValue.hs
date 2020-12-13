{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase1EncryptionAlgorithmsRequestListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase1EncryptionAlgorithmsRequestListValue
  ( Phase1EncryptionAlgorithmsRequestListValue (..),

    -- * Smart constructor
    mkPhase1EncryptionAlgorithmsRequestListValue,

    -- * Lenses
    pearlvValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the encryption algorithm for the VPN tunnel for phase 1 IKE negotiations.
--
-- /See:/ 'mkPhase1EncryptionAlgorithmsRequestListValue' smart constructor.
newtype Phase1EncryptionAlgorithmsRequestListValue = Phase1EncryptionAlgorithmsRequestListValue'
  { -- | The value for the encryption algorithm.
    value :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Phase1EncryptionAlgorithmsRequestListValue' with the minimum fields required to make a request.
--
-- * 'value' - The value for the encryption algorithm.
mkPhase1EncryptionAlgorithmsRequestListValue ::
  Phase1EncryptionAlgorithmsRequestListValue
mkPhase1EncryptionAlgorithmsRequestListValue =
  Phase1EncryptionAlgorithmsRequestListValue' {value = Lude.Nothing}

-- | The value for the encryption algorithm.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pearlvValue :: Lens.Lens' Phase1EncryptionAlgorithmsRequestListValue (Lude.Maybe Lude.Text)
pearlvValue = Lens.lens (value :: Phase1EncryptionAlgorithmsRequestListValue -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: Phase1EncryptionAlgorithmsRequestListValue)
{-# DEPRECATED pearlvValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.ToQuery Phase1EncryptionAlgorithmsRequestListValue where
  toQuery Phase1EncryptionAlgorithmsRequestListValue' {..} =
    Lude.mconcat ["Value" Lude.=: value]
